using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace AsyncSizeAnalyzer
{
    /// <summary>
    /// Raises a diagnostic if, at an await, the minimum necessary size of the state 
    /// captured by the generated state machine exceeds a certain threshold.
    /// </summary>
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class Analyzer : DiagnosticAnalyzer
    {
        private static readonly object DebuggerLock = new object();

        private const int DEFAULT_MAX_SIZE_BYTES = -1;

#pragma warning disable RS2008 // As a proof of concept, can ignore this for now (if actually in use, todo: remove this)
        private static readonly DiagnosticDescriptor AsyncStateMachineExceedsConfiguredWarningSize =
            new DiagnosticDescriptor(
                "ASA1000",
                "Async State Machine Too Large",
                "Await captures {0} bytes (> maximum of {1} bytes): {2}",
                "AsyncSizeAnalyzer",
                DiagnosticSeverity.Warning,
                true
            );
#pragma warning restore RS2008

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(AsyncStateMachineExceedsConfiguredWarningSize);

        public int? MaxSizeBytes { get; set; }

        public override void Initialize(AnalysisContext context)
        {
            // ignore generated code, and we're thread safe
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

            if (!Debugger.IsAttached)
            {
                context.EnableConcurrentExecution();
            }

            context.RegisterCompilationStartAction(OnCompilationStart);
        }

        private void OnCompilationStart(CompilationStartAnalysisContext obj)
        {
            int? maximumSize = null;

            var opts = obj.Options;
            var configFile = opts.AdditionalFiles.SingleOrDefault(x => x.Path.EndsWith("ASA.txt"));

            if(configFile != null)
            {
                var text = configFile.GetText()?.ToString() ?? "";

                var configLines = text.Replace("\r\n", "\n").Split('\n');
                var bytes = configLines.Where(l => int.TryParse(l, out _)).Select(l => (int?)int.Parse(l)).FirstOrDefault();

                maximumSize = bytes;

                var shouldBreak = configLines.Any(l => l.Equals("break"));

                if (shouldBreak && !Debugger.IsAttached)
                {
                    Debugger.Launch();
                }
            }

            MaxSizeBytes = MaxSizeBytes ?? maximumSize ?? DEFAULT_MAX_SIZE_BYTES;

            obj.RegisterSyntaxNodeAction(OnMethodDeclaration, ImmutableArray.Create(SyntaxKind.MethodDeclaration, SyntaxKind.LocalFunctionStatement));
        }

        private void OnMethodDeclaration(SyntaxNodeAnalysisContext obj)
        {
            BlockSyntax? body = null;
            ExpressionSyntax? expBody = null;

            if (obj.Node is BaseMethodDeclarationSyntax mtdDecl)
            {
                if (!mtdDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AsyncKeyword)))
                {
                    return;
                }

                body = mtdDecl.Body;
                expBody = mtdDecl.ExpressionBody?.Expression;
            }
            else if (obj.Node is LocalFunctionStatementSyntax locFuncDecl)
            {
                if (!locFuncDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AsyncKeyword)))
                {
                    return;
                }

                body = locFuncDecl.Body;
                expBody = locFuncDecl.ExpressionBody?.Expression;
            }
            else
            {
                throw new InvalidOperationException("What?");
            }

            int maximumSize = MaxSizeBytes ?? DEFAULT_MAX_SIZE_BYTES;

            SyntaxNode first, last;
            IEnumerable<AwaitExpressionSyntax> awaitPoints;

            if (body != null)
            {
                if (body.Statements.Count == 0)
                {
                    return;
                }

                first = body.Statements.First();
                last = body.Statements.Last();
                awaitPoints = body.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>();
            }
            else if (expBody != null)
            {
                first = last = expBody;
                awaitPoints = expBody.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>();
            }
            else
            {
                return;
            }

            var model = obj.SemanticModel;

            var mtdSymbol = model.GetDeclaredSymbol(obj.Node);
            if (mtdSymbol?.ContainingType == null)
            {
                // todo: technically an error case?
                return;
            }

            foreach (var awaitExp in awaitPoints)
            {
                var postAwaitState = StateNeededAfterExpression(obj.Node, awaitExp, model);
                if (postAwaitState == null)
                {
                    // todo: technically an error case?
                    continue;
                }

                var (estimatedBytes, log) = EstimateBytesForStateMachine(mtdSymbol.ContainingType, postAwaitState.Value);

                if (estimatedBytes > maximumSize)
                {
                    obj.ReportDiagnostic(
                        CreateDiagnostic(
                            awaitExp.GetLocation(),
                            estimatedBytes,
                            maximumSize,
                            log
                        )
                    );
                }
            }
        }

        private static (int EstimatedBytes, string Description) EstimateBytesForStateMachine(
            ITypeSymbol containingType,
            (ImmutableList<ILocalSymbol> Locals, ImmutableList<IParameterSymbol> Parameters, ImmutableList<ITypeSymbol> ImplicitLocals, bool ThisIsReferenced) captured
        )
        {
            // todo: we're assuming 8 bytes... because 64-bits, but that's not necessarily true
            const int REFERENCE_SIZE = 8;

            var ret = 0;
            var log = new StringBuilder();

            if (captured.ThisIsReferenced)
            {
                var thisSize = EstimateSizeOfField(containingType);
                log.Append($" `this`: {thisSize} bytes,");
                ret += thisSize;
            }

            foreach (var l in captured.Locals)
            {
                var localSize = EstimateSizeOfField(l.Type); ;
                log.Append($" local `{l.Name}`: {localSize} bytes,");
                ret += localSize;
            }

            foreach (var p in captured.Parameters)
            {
                var paramSize = EstimateSizeOfField(p.Type);
                log.Append($" parameter `{p.Name}`: {paramSize} bytes,");
                ret += paramSize;
            }

            foreach (var i in captured.ImplicitLocals)
            {
                var localSize = EstimateSizeOfField(i);
                log.Append($" implicit local of type {i.ToString()}: {localSize} bytes,");
                ret += localSize;
            }

            return (ret, log.ToString().Trim(new[] { ' ', ',' }));

            static int EstimateSizeOfField(ITypeSymbol type)
            {
                if (type.IsReferenceType)
                {
                    return REFERENCE_SIZE;
                }

                if (!type.IsValueType)
                {
                    // implies generic
                    return 0;
                }

                switch (type.SpecialType)
                {
                    case SpecialType.System_Boolean: return sizeof(bool);
                    case SpecialType.System_Byte: return sizeof(byte);
                    case SpecialType.System_Char: return sizeof(char);
                    case SpecialType.System_Decimal: return sizeof(decimal);
                    case SpecialType.System_Double: return sizeof(double);
                    case SpecialType.System_Int16: return sizeof(short);
                    case SpecialType.System_Int32: return sizeof(int);
                    case SpecialType.System_Int64: return sizeof(long);
                    case SpecialType.System_SByte: return sizeof(sbyte);
                    case SpecialType.System_Single: return sizeof(float);
                    case SpecialType.System_UInt16: return sizeof(ushort);
                    case SpecialType.System_UInt32: return sizeof(uint);
                    case SpecialType.System_UInt64: return sizeof(ulong);
                }

                if (type is INamedTypeSymbol namedType && namedType.EnumUnderlyingType != null)
                {
                    return EstimateSizeOfField(namedType.EnumUnderlyingType);
                }

                var fields = type.GetMembers().OfType<IFieldSymbol>().Where(f => !f.IsStatic && !f.IsConst);

                return fields.Sum(f => EstimateSizeOfField(f.Type));
            }
        }

        private static Diagnostic CreateDiagnostic(
            Location? loc,
            int captured,
            int max,
            string log
        )
        => Diagnostic.Create(AsyncStateMachineExceedsConfiguredWarningSize, loc, new object[] { captured, max, log });

        /// <summary>
        /// Takes a bunch of syntax nodes, and figures out variables, parameters, and if `this`
        /// are used in the syntax nodes _between_ those nodes
        /// </summary>
        private sealed class Visitor : CSharpSyntaxWalker
        {
            private readonly SemanticModel Model;
            private readonly ITypeSymbol ContainingType;

            private ImmutableHashSet<ILocalSymbol>.Builder Variables;
            private ImmutableHashSet<IParameterSymbol>.Builder Parameters;
            private bool IsThisReferenced;

            public (ImmutableList<ILocalSymbol> Locals, ImmutableList<IParameterSymbol> Parameters, bool ThisIsReferenced) Results
            => (Variables.ToImmutableList(), Parameters.ToImmutableList(), IsThisReferenced);

            public Visitor(SemanticModel model, ITypeSymbol containingType)
            {
                Model = model;
                ContainingType = containingType;

                IsThisReferenced = false;
                Variables = ImmutableHashSet.CreateBuilder<ILocalSymbol>();
                Parameters = ImmutableHashSet.CreateBuilder<IParameterSymbol>();
            }

            public override void VisitThisExpression(ThisExpressionSyntax node)
            {
                IsThisReferenced = true;

                base.VisitThisExpression(node);
            }

            public override void VisitIdentifierName(IdentifierNameSyntax node)
            {
                var sym = Model.GetSymbolInfo(node);
                if (sym.Symbol is ILocalSymbol local)
                {
                    Variables.Add(local);
                }
                else if (sym.Symbol is IParameterSymbol param)
                {
                    Parameters.Add(param);
                }
                else if (sym.Symbol is IFieldSymbol field)
                {
                    IsThisReferenced |= !field.IsStatic && IsOnContainingType(field.ContainingType) && !node.Ancestors().OfType<MemberAccessExpressionSyntax>().Any();
                }
                else if (sym.Symbol is IMethodSymbol mtd)
                {
                    IsThisReferenced |= !mtd.IsStatic && IsOnContainingType(mtd.ContainingType) && !node.Ancestors().OfType<MemberAccessExpressionSyntax>().Any();
                }
                else if (sym.Symbol is IPropertySymbol prop)
                {
                    IsThisReferenced |= !prop.IsStatic && IsOnContainingType(prop.ContainingType) && !node.Ancestors().OfType<MemberAccessExpressionSyntax>().Any();
                }
                else if (sym.Symbol is IEventSymbol evt)
                {
                    IsThisReferenced |= !evt.IsStatic && IsOnContainingType(evt.ContainingType) && !node.Ancestors().OfType<MemberAccessExpressionSyntax>().Any();
                }

                base.VisitIdentifierName(node);

                bool IsOnContainingType(ITypeSymbol? memberOnType)
                {
                    if (memberOnType == null)
                    {
                        return false;
                    }

                    var conversion = Model.Compilation.ClassifyConversion(ContainingType, memberOnType);
                    return conversion.IsIdentity || conversion.IsImplicit;
                }
            }
        }

        // todo: naming is a bit weird here, these are statements or expressions technically (but I just say expression, because that's going to be what async is)

        public static (ImmutableList<ILocalSymbol> Locals, ImmutableList<IParameterSymbol> Parameters, ImmutableList<ITypeSymbol> ImplicitLocals, bool ThisIsReferenced)? StateNeededAfterExpression(SyntaxNode mtd, SyntaxNode entryNode, SemanticModel model)
        {
            var mtdSymbol = model.GetDeclaredSymbol(mtd);
            if (mtdSymbol?.ContainingType == null)
            {
                // todo: error condition
                return null;
            }

            var introducesVariable = entryNode.AncestorsAndSelf().OfType<VariableDeclaratorSyntax>().SingleOrDefault();

            var visitor = new Visitor(model, mtdSymbol.ContainingType);

            var implicits = ImmutableList.CreateBuilder<ITypeSymbol>();

            var reachedDeclarations = ImmutableHashSet.CreateBuilder<VariableDeclaratorSyntax>();
            var reachedCatchClauses = ImmutableHashSet.CreateBuilder<CatchDeclarationSyntax>();

            if (introducesVariable != null)
            {
                // sigh, special case!
                // if it's in the initializer of a for loop, it doesn't count!

                var inForLoop = entryNode.Ancestors().OfType<ForStatementSyntax>().SingleOrDefault(x => x.Declaration!= null && x.Declaration.DescendantNodesAndSelf().Contains(introducesVariable));

                if (inForLoop == null)
                {
                    reachedDeclarations.Add(introducesVariable);
                }
            }

            var reachableNodes = NodesReachableAfterExpression(entryNode, model);

            foreach (var reachableNode in reachableNodes)
            {
                if (reachableNode.Parent is ForEachStatementSyntax foreachStatement && reachableNode == foreachStatement.Expression)
                {
                    // foreach is special, in that the thing actually captured is the _enumerator_
                    var nodeType = model.GetTypeInfo(reachableNode);
                    if (nodeType.Type != null)
                    {
                        // todo: this doesn't handle non-generic GetEnumerator()
                        //       ... and is just kind of hacky in general

                        // todo: for c# 9, GetEnumerator can also be an extension method...

                        var concreteEnumerators =
                            nodeType.Type
                                .GetMembers("GetEnumerator")
                                .OfType<IMethodSymbol>()
                                .Where(x => !x.Parameters.Any() && !x.ReturnsVoid);
                        var interfaceEnumerator =
                            concreteEnumerators
                                .SingleOrDefault(x => x.ReturnType is INamedTypeSymbol namedType && namedType.IsGenericType && namedType.ConstructUnboundGenericType().Name == "System.Collections.Generic.IEnumerator<T>");
                        var duckTypedEnumerator =
                            concreteEnumerators.Except(new[] { interfaceEnumerator }).SingleOrDefault();

                        var enumeratorToUse = duckTypedEnumerator ?? interfaceEnumerator;

                        if (enumeratorToUse == null)
                        {
                            // implies this is a built-in enumeratable (array, string, or something)
                            if (nodeType.Type.SpecialType == SpecialType.System_String)
                            {
                                // string actually stashs a char[]
                                var c = model.Compilation.GetSpecialType(SpecialType.System_Char);
                                var cArr = model.Compilation.CreateArrayTypeSymbol(c);
                                implicits.Add(cArr);
                            }
                            else
                            {
                                implicits.Add(nodeType.Type);
                            }

                            // and then an int local to track position
                            implicits.Add(model.Compilation.GetSpecialType(SpecialType.System_Int32));
                        }
                        else
                        {
                            implicits.Add(enumeratorToUse.ReturnType);
                        }
                    }
                }
                else
                {
                    foreach (var varDecl in reachableNode.DescendantNodesAndSelf().OfType<VariableDeclaratorSyntax>())
                    {
                        reachedDeclarations.Add(varDecl);
                    }

                    foreach (var excDecl in reachableNode.DescendantNodesAndSelf().OfType<CatchDeclarationSyntax>())
                    {
                        reachedCatchClauses.Add(excDecl);
                    }

                    visitor.Visit(reachableNode);
                }
            }

            var (ls, ps, hasThis) = visitor.Results;

            var declaredAfterEntryNode = reachedDeclarations.ToImmutable();
            var catchesReachableAfterEntryNode = reachedCatchClauses.ToImmutable();
            var reachableButAlsoDeclared = ls.Where(l => declaredAfterEntryNode.Contains(l.DeclaringSyntaxReferences.Single().GetSyntax())).ToImmutableList();
            var reachableCatchClauses = ls.Where(l => catchesReachableAfterEntryNode.Contains(l.DeclaringSyntaxReferences.Single().GetSyntax())).ToImmutableList();

            var filteredLocals = ls.Except(reachableButAlsoDeclared).Except(reachableCatchClauses).ToImmutableList();

            return (filteredLocals, ps, implicits.ToImmutable(), hasThis);
        }

        public static ImmutableList<SyntaxNode> NodesReachableAfterExpression(SyntaxNode entryNode, SemanticModel model)
        => NodesReachableAfterExpressionImpl(entryNode, model, new HashSet<SyntaxNode>()).Distinct().ToImmutableList();

        internal static ImmutableList<SyntaxNode> NodesReachableAfterExpressionImpl(SyntaxNode entryNode, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
        {
            // possibilities:
            //   - entryNode is a statement
            //     * if it is a statement, everything after it in it's containing block is reachable
            //   - entryNode is an expression
            //     * most expression progress left to right, so everything _after_ entryNode needs to be handled
            //     * however, ternary (a ? b : c) expressions are special
            //       + if entry is in a, both b and c are reachable
            //       + if entry is in b or c, only they are reachable
            //     * if, & switch are also special, since all of their branches are reachable
            //
            // additionally, the statement entryNode is in a block
            //   - that block has fallthrough, so we should include anything _after_ that block
            //   - however, if it is in a for, foreach, while, or do while then we need to include some of the conditions
            //
            // when in loops, we have to look for continues and gotos 

            ImmutableList<SyntaxNode> basicRet;

            // starting at a whole statement
            if (entryNode is StatementSyntax statement)
            {
                basicRet = HandleStatement(statement, model, alreadyVisited);
            }
            else
            {
                // we're in an expression
                basicRet = HandleExpression(entryNode, model, alreadyVisited);
            }

            // now that we've found everything "normally" we need to handle all the try/catch/finally clauses that cover the reachable nodes
            var ret = ImmutableList.CreateBuilder<SyntaxNode>();

            // but, special case, the entryNode is actually considered reachable for this (since every try/catch/finally follows)
            ret.AddRange(HandleBeingInTryCatchFinally(entryNode, model, alreadyVisited));

            foreach (var node in basicRet)
            {
                ret.Add(node);
                ret.AddRange(HandleBeingInTryCatchFinally(node, model, alreadyVisited));
            }

            return ret.ToImmutable();

            static ImmutableList<SyntaxNode> HandleCatchClause(CatchClauseSyntax inCatch, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(inCatch))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                if (inCatch.Declaration != null)
                {
                    ret.Add(inCatch.Declaration);
                }

                var followingCatchBlock = inCatch.Block;
                if (followingCatchBlock.Statements.Any())
                {
                    var firstEntry = followingCatchBlock.Statements[0];
                    ret.Add(firstEntry);
                    ret.AddRange(HandleStatement(firstEntry, model, alreadyVisited));
                }

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleFinallyClause(FinallyClauseSyntax inFinally, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(inFinally))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                var finallyBlock = inFinally.Block;
                if (finallyBlock.Statements.Any())
                {
                    var firstEntry = finallyBlock.Statements[0];
                    ret.Add(firstEntry);
                    ret.AddRange(HandleStatement(firstEntry, model, alreadyVisited));
                }

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleBeingInTryCatchFinally(SyntaxNode node, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                // don't check if we handled the node, because try/catch/finally isn't necessarily found the way other nodes are

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                var inTryBlocks = node.Ancestors().OfType<BlockSyntax>().Where(b => b.Parent is TryStatementSyntax).Select(b => b.Parent).OfType<TryStatementSyntax>();
                foreach (var inTryBlock in inTryBlocks)
                {
                    foreach (var inCatch in inTryBlock.Catches)
                    {
                        // include the declaration, because then we'll toss it since it's declared in that scope
                        ret.AddRange(HandleCatchClause(inCatch, model, alreadyVisited));
                    }

                    if (inTryBlock.Finally != null)
                    {
                        ret.AddRange(HandleFinallyClause(inTryBlock.Finally, model, alreadyVisited));
                    }
                }

                // if we're already in a catch, we need to visit the _following_ catches (and the try)
                var inCatches = node.Ancestors().OfType<CatchClauseSyntax>();
                foreach (var inCatch in inCatches)
                {
                    var correspondingTry = inCatch.Ancestors().OfType<TryStatementSyntax>().FirstOrDefault();
                    if (correspondingTry == null)
                    {
                        // technically an error case
                        return ImmutableList<SyntaxNode>.Empty;
                    }

                    var catchIndex = correspondingTry.Catches.IndexOf(inCatch);
                    var followingCatches = correspondingTry.Catches.Skip(catchIndex + 1);

                    foreach (var followingCatch in followingCatches)
                    {
                        ret.AddRange(HandleCatchClause(followingCatch, model, alreadyVisited));
                    }

                    if (correspondingTry.Finally != null)
                    {
                        ret.AddRange(HandleFinallyClause(correspondingTry.Finally, model, alreadyVisited));
                    }
                }

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleExpression(SyntaxNode expression, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(expression))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                var cur = expression;
                while (cur != null && !(cur is StatementSyntax))
                {
                    var parent = cur.Parent;
                    if (parent == null)
                    {
                        throw new Exception("Shouldn't be possible?");
                    }

                    bool hasSpecialTreatment;

                    if (parent is ConditionalExpressionSyntax condExp)
                    {
                        hasSpecialTreatment = true;

                        if (condExp.Condition == cur)
                        {
                            // this is `expression ? a : b` => a & b are reachable
                            ret.Add(condExp.WhenTrue);
                            ret.Add(condExp.WhenFalse);
                        }
                    }
                    else if (parent is IfStatementSyntax ifStatement)
                    {
                        hasSpecialTreatment = true;

                        if (ifStatement.Condition == cur)
                        {
                            // this is `if(expression) { a } else { b }` => a & b are reachable
                            ret.Add(ifStatement.Statement);
                            if (ifStatement.Else?.Statement != null)
                            {
                                ret.Add(ifStatement.Else.Statement);
                            }
                        }
                    }
                    else if (parent is SwitchStatementSyntax switchStatement)
                    {
                        hasSpecialTreatment = true;

                        if (switchStatement.Expression == cur)
                        {
                            // this is `switch(expression) { case ... }` => all cases are reachable
                            ret.AddRange(switchStatement.Sections);
                        }
                    }
                    else if (parent is SwitchExpressionSyntax switchExp)
                    {
                        hasSpecialTreatment = true;

                        if (switchExp.GoverningExpression == cur)
                        {
                            // this is `expression switch { blah => ... }` => all arms are reachable
                            ret.AddRange(switchExp.Arms);
                        }
                    }
                    else if (parent is ForStatementSyntax forStatement)
                    {
                        hasSpecialTreatment = true;

                        // for is weird
                        //   - initializer is not reachable (it's either already happened, or contains the entry node)
                        //   - condition is reachable if:
                        //     * the entryNode is the intializer OR
                        //     * the end of the loop is reachable OR
                        //     * any continue in the loop is reachable
                        //   - incr is reachable if:
                        //     * the end of the loop is reachable OR
                        //     * any continue in the loop is reachable
                        //   - body is always reachable
                        //     * ok not actually, but close enough

                        var initializeContainsEntry = forStatement.Initializers.Any(i => i == cur);
                        var forMayBeContinued = false;
                        var forMayFallthrough = false;
                        StatementSyntax startOfFor;
                        {
                            startOfFor = forStatement.Statement;
                            StatementSyntax endOfFor = forStatement.Statement;
                            if (startOfFor is BlockSyntax block && block.Statements.Any())
                            {
                                startOfFor = block.Statements[0];
                                endOfFor = block.Statements.Last();
                            }

                            var fullReachability = model.AnalyzeControlFlow(startOfFor, endOfFor);
                            if (fullReachability != null && fullReachability.Succeeded && fullReachability.EndPointIsReachable)
                            {
                                forMayFallthrough = true;
                            }

                            var continuesInBody = forStatement.Statement.DescendantNodesAndSelf().OfType<ContinueStatementSyntax>();
                            foreach (var continueStatement in continuesInBody)
                            {
                                var matchingStatement = GetStatementWhichIsContinued(continueStatement);
                                if (matchingStatement != forStatement)
                                {
                                    continue;
                                }

                                var continueReachability = model.AnalyzeControlFlow(startOfFor, continueStatement);
                                if (continueReachability != null && continueReachability.Succeeded && continueReachability.EndPointIsReachable)
                                {
                                    forMayBeContinued = true;
                                    break;
                                }
                            }
                        }

                        var conditionIsReachable = initializeContainsEntry || forMayBeContinued || forMayFallthrough;
                        if (forStatement.Condition != null && conditionIsReachable)
                        {
                            ret.Add(forStatement.Condition);
                        }

                        var incrementsAreReachable = forMayFallthrough || forMayBeContinued;
                        if (incrementsAreReachable)
                        {
                            foreach (var incr in forStatement.Incrementors)
                            {
                                ret.Add(incr);
                            }
                        }

                        // shove the body in
                        ret.Add(startOfFor);
                        ret.AddRange(HandleStatement(startOfFor, model, alreadyVisited));
                    }
                    else if (parent is WhileStatementSyntax whileStatement)
                    {
                        hasSpecialTreatment = true;

                        var start = StartOfBlockOrStatement(whileStatement.Statement);

                        ret.AddRange(HandleStatement(start, model, alreadyVisited));
                    }
                    else if (parent is DoStatementSyntax doStatement)
                    {
                        hasSpecialTreatment = true;

                        var start = StartOfBlockOrStatement(doStatement.Statement);

                        ret.AddRange(HandleStatement(start, model, alreadyVisited));
                    }
                    else if (parent is ForEachStatementSyntax foreachStatement)
                    {
                        hasSpecialTreatment = true;

                        var start = StartOfBlockOrStatement(foreachStatement.Statement);

                        ret.AddRange(HandleStatement(start, model, alreadyVisited));
                    }
                    else
                    {
                        hasSpecialTreatment = false;
                    }

                    if (!hasSpecialTreatment)
                    {
                        var indexInParent = GetIndexInParent(cur);
                        var siblingsAfterCur = parent.ChildNodes().Skip(indexInParent + 1);
                        ret.AddRange(siblingsAfterCur);
                    }

                    cur = parent;
                }

                // we've now walked all the way out to the _statement_ which contains the expression and have included all the siblings to the _right_ of the target

                if (cur is StatementSyntax statement)
                {
                    // we need to handle everything that follows the statement that had the target expression
                    ret.AddRange(HandleStatement(statement, model, alreadyVisited));
                }

                return ret.ToImmutable();
            }

            static StatementSyntax StartOfBlockOrStatement(StatementSyntax statement)
            {
                if (statement is BlockSyntax block && block.Statements.Any())
                {
                    return block.Statements[0];
                }

                return statement;
            }

            static ImmutableList<SyntaxNode> HandleStatement(StatementSyntax entryStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(entryStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                // trace from the entry statement through to other reachable statements in it's containg block...
                if (entryStatement.Parent is BlockSyntax block)
                {
                    var indexOfEntryStatement = GetIndexInParent(entryStatement);

                    var maybeReachable = block.Statements.Skip(indexOfEntryStatement + 1);
                    var reachedEnd = true;
                    foreach (var statement in maybeReachable)
                    {
                        ret.Add(statement);
                        if (statement is ContinueStatementSyntax continueStatement)
                        {
                            ret.AddRange(HandleContinue(continueStatement, model, alreadyVisited));
                            reachedEnd = false;
                            break;
                        }
                        else if (statement is GotoStatementSyntax gotoStatement)
                        {
                            ret.AddRange(HandleGoto(gotoStatement, model, alreadyVisited));
                            reachedEnd = false;
                            break;
                        }
                        else if (statement is ReturnStatementSyntax returnStatement)
                        {
                            reachedEnd = false;
                            break;
                        }
                    }

                    // if we're going to fallthrough, we need to handle it
                    if (reachedEnd)
                    {
                        ret.AddRange(HandleFallthroughFromBlock(block, model, alreadyVisited));
                    }
                }
                else if (entryStatement.Parent is SwitchSectionSyntax switchSection)
                {
                    var indexOfEntryStatement = GetIndexInParent(entryStatement);

                    var maybeReachable = entryStatement.Parent.ChildNodes().Skip(indexOfEntryStatement + 1);
                    var reachedEnd = true;
                    var doesntFallThrough = false;
                    foreach (var statement in maybeReachable)
                    {
                        doesntFallThrough = true;

                        ret.Add(statement);
                        if (statement is ContinueStatementSyntax continueStatement)
                        {
                            ret.AddRange(HandleContinue(continueStatement, model, alreadyVisited));
                            reachedEnd = false;
                            break;
                        }
                        else if (statement is GotoStatementSyntax gotoStatement)
                        {
                            ret.AddRange(HandleGoto(gotoStatement, model, alreadyVisited));
                            reachedEnd = false;
                            break;
                        }
                        else if (statement is ReturnStatementSyntax returnStatement)
                        {
                            reachedEnd = false;
                            break;
                        }
                    }

                    // for a switch, we either fall through (going to next lexical branch, if any)
                    // or break to the end of the switch _if_ we hit the end
                    if (reachedEnd)
                    {
                        var parentSwitch = switchSection.Ancestors().OfType<SwitchStatementSyntax>().First();

                        if (doesntFallThrough)
                        {
                            // go to whatever is after the switch if we hit the end
                            var switchIx = GetIndexInParent(parentSwitch);
                            var nextStatement = parentSwitch.ChildNodes().Skip(switchIx + 1).FirstOrDefault();
                            if (nextStatement != null)
                            {
                                ret.Add(nextStatement);
                                ret.AddRange(NodesReachableAfterExpressionImpl(nextStatement, model, alreadyVisited));
                            }
                        }
                        else
                        {
                            // fall through to next branch
                            var sectionIndex = parentSwitch.Sections.IndexOf(switchSection);

                            // skip until we find a non-empty section
                            var nextSection = parentSwitch.Sections.ElementAtOrDefault(sectionIndex + 1);
                            while (nextSection != null)
                            {
                                if (!nextSection.Statements.Any())
                                {
                                    sectionIndex++;
                                    nextSection = parentSwitch.Sections.ElementAtOrDefault(sectionIndex + 1);
                                }
                                else
                                {
                                    break;
                                }
                            }
                            if (nextSection != null && nextSection.Statements.Any())
                            {
                                var startOfFallthrough = nextSection.Statements[0];
                                ret.Add(startOfFallthrough);
                                ret.AddRange(NodesReachableAfterExpressionImpl(startOfFallthrough, model, alreadyVisited));
                            }
                        }
                    }
                }
                else
                {
                    // this is a single statement block of sorts
                    // like `if(blah) continue;`
                    // or `for(x, y, z) do();`

                    ret.AddRange(HandleFallthroughFromBlock(entryStatement, model, alreadyVisited));
                }

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleGoto(GotoStatementSyntax gotoStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(gotoStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var kind = gotoStatement.Kind();
                if (kind == SyntaxKind.GotoCaseStatement)
                {
                    // todo: these are complicated, just bail for now
                }
                else if (kind == SyntaxKind.GotoDefaultStatement)
                {
                    // todo: these are complicated, just bail for now
                }
                else
                {
                    var labelName = (gotoStatement.Expression as IdentifierNameSyntax)?.Identifier.ValueText ?? "";

                    SyntaxNode? cur = gotoStatement;
                    while (cur != null)
                    {
                        if (cur is BlockSyntax block)
                        {
                            var target = block.DescendantNodes().OfType<LabeledStatementSyntax>().FirstOrDefault(l => l.Identifier.ValueText == labelName);
                            if (target != null)
                            {
                                var ret = ImmutableList.CreateBuilder<SyntaxNode>();
                                ret.Add(target.Statement);
                                ret.AddRange(HandleStatement(target.Statement, model, alreadyVisited));

                                return ret.ToImmutable();
                            }
                        }

                        cur = cur.Parent;
                    }
                }

                // todo: this is an error case, yes?
                return ImmutableList<SyntaxNode>.Empty;
            }

            static StatementSyntax? GetStatementWhichIsContinued(ContinueStatementSyntax continueStatement)
            {
                SyntaxNode? cur = continueStatement;
                while (cur != null)
                {
                    if (cur is ForStatementSyntax forStatement)
                    {
                        return forStatement;
                    }
                    else if (cur is ForEachStatementSyntax foreachStatement)
                    {
                        return foreachStatement;
                    }
                    else if (cur is WhileStatementSyntax whileStatement)
                    {
                        return whileStatement;
                    }
                    else if (cur is DoStatementSyntax doStatement)
                    {
                        return doStatement;
                    }

                    cur = cur.Parent;
                }

                return null;
            }

            static ImmutableList<SyntaxNode> HandleContinue(ContinueStatementSyntax continueStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(continueStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var continuedStatement = GetStatementWhichIsContinued(continueStatement);
                if (continueStatement != null)
                {
                    if (continuedStatement is ForStatementSyntax forStatement)
                    {
                        return HandleForContinued(forStatement, model, alreadyVisited);

                    }
                    else if (continuedStatement is ForEachStatementSyntax foreachStatement)
                    {
                        return HandleForEachContinued(foreachStatement, model, alreadyVisited);
                    }
                    else if (continuedStatement is WhileStatementSyntax whileStatement)
                    {
                        return HandleWhileContinued(whileStatement, model, alreadyVisited);
                    }
                    else if (continuedStatement is DoStatementSyntax doStatement)
                    {
                        return HandleDoContinued(doStatement, model, alreadyVisited);
                    }
                }

                // todo: this is kind of an error condition, yes?
                return ImmutableList<SyntaxNode>.Empty;
            }

            static ImmutableList<SyntaxNode> HandleLoopBody(StatementSyntax statementOrBlock, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(statementOrBlock))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                // and now repeat the loop
                var firstStatement = statementOrBlock;
                if (firstStatement is BlockSyntax block && block.Statements.Any())
                {
                    firstStatement = block.Statements[0];
                }

                ret.Add(firstStatement);
                ret.AddRange(HandleStatement(firstStatement, model, alreadyVisited));

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleForContinued(ForStatementSyntax forStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(forStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                if (forStatement.Condition != null)
                {
                    ret.Add(forStatement.Condition);
                }
                foreach (var incr in forStatement.Incrementors)
                {
                    ret.Add(incr);
                }

                ret.AddRange(HandleLoopBody(forStatement.Statement, model, alreadyVisited));

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleForEachContinued(ForEachStatementSyntax foreachStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(foreachStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                // todo: this needs to actually refer to the enumerator, somehow
                ret.Add(foreachStatement.Expression);

                ret.AddRange(HandleLoopBody(foreachStatement.Statement, model, alreadyVisited));

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleWhileContinued(WhileStatementSyntax whileStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(whileStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                return ImmutableList.Create<SyntaxNode>(whileStatement.Condition);
            }

            static ImmutableList<SyntaxNode> HandleDoContinued(DoStatementSyntax doStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                if (!alreadyVisited.Add(doStatement))
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();
                ret.Add(doStatement.Condition);

                ret.AddRange(HandleLoopBody(doStatement.Statement, model, alreadyVisited));

                return ret.ToImmutable();
            }

            static ImmutableList<SyntaxNode> HandleFallthroughFromBlock(SyntaxNode blockStatement, SemanticModel model, HashSet<SyntaxNode> alreadyVisited)
            {
                // intentionally not checking alreadyVisited here, we have to do it further down the method

                var blockParent = blockStatement.Parent;
                if (blockParent == null)
                {
                    return ImmutableList<SyntaxNode>.Empty;
                }

                var ret = ImmutableList.CreateBuilder<SyntaxNode>();

                // we may go up a level here because the fallthrough implies handling a bit more than just the blockParent
                SyntaxNode effectiveBlockStatement = blockStatement;
                var effectiveBlockParent = blockParent;

                var skipMarkBecauseExitingTry = false;

                // special cases for loop constructs & try / catch / finally
                if (blockParent is ForStatementSyntax forStatement)
                {
                    effectiveBlockStatement = forStatement;
                    effectiveBlockParent = forStatement.Parent;

                    ret.AddRange(HandleForContinued(forStatement, model, alreadyVisited));
                }
                else if (blockParent is ForEachStatementSyntax foreachStatement)
                {
                    effectiveBlockStatement = foreachStatement;
                    effectiveBlockParent = foreachStatement.Parent;

                    ret.AddRange(HandleForEachContinued(foreachStatement, model, alreadyVisited));
                }
                else if (blockParent is WhileStatementSyntax whileStatement)
                {
                    effectiveBlockStatement = whileStatement;
                    effectiveBlockParent = whileStatement.Parent;

                    ret.AddRange(HandleWhileContinued(whileStatement, model, alreadyVisited));
                }
                else if (blockParent is DoStatementSyntax doStatement)
                {
                    effectiveBlockStatement = doStatement;
                    effectiveBlockParent = doStatement.Parent;

                    ret.AddRange(HandleDoContinued(doStatement, model, alreadyVisited));
                }
                else if (blockParent is TryStatementSyntax tryStatement)
                {
                    skipMarkBecauseExitingTry = true;

                    effectiveBlockStatement = tryStatement;
                    effectiveBlockParent = tryStatement.Parent;

                    // nothing extra, control flow into catch & finally are indirect and handled elsewhere
                }
                else if (blockParent is CatchClauseSyntax catchStatement)
                {
                    skipMarkBecauseExitingTry = true;

                    effectiveBlockStatement = blockParent.Ancestors().OfType<TryStatementSyntax>().FirstOrDefault();
                    if (effectiveBlockStatement == null)
                    {
                        // todo: this is kind of an error case, yes?
                        return ImmutableList<SyntaxNode>.Empty;
                    }

                    effectiveBlockParent = effectiveBlockStatement.Parent;

                    // nothing extra, control flow into catch & finally are indirect and handled elsewhere
                }
                else if (blockParent is FinallyClauseSyntax finallyClause)
                {
                    skipMarkBecauseExitingTry = true;

                    effectiveBlockStatement = blockParent.Ancestors().OfType<TryStatementSyntax>().FirstOrDefault();
                    if (effectiveBlockStatement == null)
                    {
                        // todo: this is kind of an error case, yes?
                        return ImmutableList<SyntaxNode>.Empty;
                    }

                    effectiveBlockParent = effectiveBlockStatement.Parent;

                    // nothing extra, control flow into catch & finally are indirect and handled elsewhere
                }
                else if (blockParent is LabeledStatementSyntax labeledStatement)
                {
                    effectiveBlockStatement = labeledStatement;
                    effectiveBlockParent = labeledStatement.Parent;
                }

                // have to defer until here because we're changing the statement we're handling
                if (!skipMarkBecauseExitingTry && !alreadyVisited.Add(effectiveBlockStatement) && ret.Count == 0) // ret will have stuff if we actually handled one of the weird loops
                {                                                                   //    so that serves as a way to say "still do work, please"
                    return ImmutableList<SyntaxNode>.Empty;
                }


                if (effectiveBlockParent == null)
                {
                    // todo: this is kind of an error case, yes?
                    return ImmutableList<SyntaxNode>.Empty;
                }

                // fallthrough
                var effectiveBlockIndexInParent = GetIndexInParent(effectiveBlockStatement);
                var firstStatementAfterEffectiveBlockStatement = effectiveBlockParent.ChildNodes().Skip(effectiveBlockIndexInParent + 1).FirstOrDefault();

                if (firstStatementAfterEffectiveBlockStatement != null)
                {
                    // have to include it here, because the recursion is going to start looking after it
                    ret.Add(firstStatementAfterEffectiveBlockStatement);
                    NodesReachableAfterExpressionImpl(firstStatementAfterEffectiveBlockStatement, model, alreadyVisited);
                }

                return ret.ToImmutable();
            }

            static int GetIndexInParent(SyntaxNode node)
            {
                var parent = node.Parent;
                if (parent == null)
                {
                    throw new Exception("Shouldn't be possible");
                }

                return parent.ChildNodes().Select((s, ix) => (s, ix)).Single(t => t.s == node).ix;
            }
        }
    }
}
