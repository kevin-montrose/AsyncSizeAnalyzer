using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.FlowAnalysis;
using Microsoft.CodeAnalysis.Operations;
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

        public override void Initialize(AnalysisContext context)
        {
            // ignore generated code, and we're thread safe
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            context.RegisterCompilationStartAction(OnCompilationStart);
        }

        private void OnCompilationStart(CompilationStartAnalysisContext obj)
        {
            // this is a convenient place to add a Debugger.Launch() if you're testing

            obj.RegisterOperationAction(OnMethodOperation, OperationKind.MethodBody);
        }

        private void OnMethodOperation(OperationAnalysisContext obj)
        {
            var body = (IMethodBodyOperation)obj.Operation;

            int? maxBytes = null;
            var opts = obj.Options.AnalyzerConfigOptionsProvider.GetOptions(obj.Operation.Syntax.SyntaxTree);
            if (opts.TryGetValue("dotnet_diagnostic.ASA1000.warn_when_larger_than_bytes", out var maxBytesValue))
            {
                if (int.TryParse(maxBytesValue, out var maxBytesParsed))
                {
                    maxBytes = maxBytesParsed;
                }
            }

            var declaringType = body.Syntax.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().FirstOrDefault();
            if (declaringType == null)
            {
                // todo: technically an error?
                return;
            }

            var containingType = obj.Operation.SemanticModel.GetDeclaredSymbol(declaringType);
            if (containingType == null)
            {
                // todo: technically an error?
                return;
            }

            int maximumSize = maxBytes ?? DEFAULT_MAX_SIZE_BYTES;

            var awaitPoints = body.DescendantsAndSelf().OfType<IAwaitOperation>();

            foreach (var awaitExp in awaitPoints)
            {
                var postAwaitState = StateNeededAfterExpression(awaitExp);
                if (postAwaitState == null)
                {
                    // todo: technically an error case?
                    continue;
                }

                var (estimatedBytes, log) = EstimateBytesForStateMachine(containingType, postAwaitState.Value);

                if (estimatedBytes > maximumSize)
                {
                    obj.ReportDiagnostic(
                        CreateDiagnostic(
                            awaitExp.Syntax.GetLocation(),
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

        public static
            (ImmutableList<ILocalSymbol> Locals, ImmutableList<IParameterSymbol> Parameters, ImmutableList<ITypeSymbol> ImplicitLocals, bool ThisIsReferenced)?
            StateNeededAfterExpression(IAwaitOperation awaitOp)
        {
            var (reachableOperations, localsNeedCapture, capturedImplicitTypes) = OperationsReachableAfterAsyncExpression(awaitOp);
            var allOperations = reachableOperations.SelectMany(r => r.DescendantsAndSelf()).Distinct().ToImmutableList();

            var parameters = allOperations.OfType<IParameterReferenceOperation>().Select(l => l.Parameter).Distinct().ToImmutableList();

            var thisReferences = allOperations.OfType<IInstanceReferenceOperation>().ToImmutableList();

            var referencesThis = thisReferences.Any();

            return (localsNeedCapture, parameters, capturedImplicitTypes, referencesThis);
        }

        public static (ImmutableList<IOperation> Operations, ImmutableList<ILocalSymbol> LocalsToCapture, ImmutableList<ITypeSymbol> ImplicitCaptures) OperationsReachableAfterAsyncExpression(IAwaitOperation entryNode)
        {
            // control flow can't start at an expression (le sigh)
            var inMethod = GetContaining<IMethodBodyOperation>(entryNode);

            var inLocalFunc = GetContaining<ILocalFunctionOperation>(entryNode);

            ImmutableArray<BasicBlock> methodBlocks;
            if (inMethod != null)
            {
                var methodGraph = ControlFlowGraph.Create(inMethod);

                if (inLocalFunc != null)
                {
                    var funcGraph = methodGraph.GetLocalFunctionControlFlowGraphInScope(inLocalFunc.Symbol);
                    methodBlocks = funcGraph.Blocks;
                }
                else
                {
                    methodBlocks = methodGraph.Blocks;
                }
            }
            else
            {
                return (ImmutableList<IOperation>.Empty, ImmutableList<ILocalSymbol>.Empty, ImmutableList<ITypeSymbol>.Empty);
            }

            var operationsInOrderVisited = ImmutableList.CreateBuilder<IOperation>();

            var entryOperationInitializesLocals = ImmutableList.CreateBuilder<ILocalSymbol>();
            var entryOperationInitializesCaptures = ImmutableList.CreateBuilder<CaptureId>();


            // check to see if the op is in any block bodies
            var blocksWithEntryNodeInBody = methodBlocks.Where(b => b.Operations.Any(o => ContainsOperation(o, entryNode))).OrderBy(x => x.Ordinal);
            foreach (var blockWithEntryNodeInBody in blocksWithEntryNodeInBody)
            {
                var entryNodeIndex = blockWithEntryNodeInBody.Operations.Select((o, ix) => (o, ix)).Single(t => ContainsOperation(t.o, entryNode)).ix;
                var operationWithEntryNode = blockWithEntryNodeInBody.Operations[entryNodeIndex];

                var lowestOperationWithEntryNode = GetLowestChildContaining(operationWithEntryNode, entryNode, 0);
                var entryNodeOperationForReachability = lowestOperationWithEntryNode.Operation;

                GetOperationsFromBlockContainingEntryNode(operationsInOrderVisited, blockWithEntryNodeInBody, methodBlocks, entryNodeOperationForReachability, operationWithEntryNode);

                // include everything after the op
                var afterEntryNode = blockWithEntryNodeInBody.Operations.Skip(entryNodeIndex + 1);
                operationsInOrderVisited.AddRange(afterEntryNode);

                if (operationWithEntryNode is IAssignmentOperation assignOp)
                {
                    entryOperationInitializesLocals.AddRange(operationWithEntryNode.DescendantsAndSelf().OfType<ILocalReferenceOperation>().Select(l => l.Local));
                }

                if (operationWithEntryNode is IFlowCaptureOperation captureOp)
                {
                    entryOperationInitializesCaptures.Add(captureOp.Id);
                }
            }

            // check to see if the op is in any conditions
            var blocksWithEntryNodeInCondition = methodBlocks.Where(b => b.BranchValue != null && ContainsOperation(b.BranchValue, entryNode)).OrderBy(x => x.Ordinal);
            foreach (var blockWithEntryNodeInCondition in blocksWithEntryNodeInCondition)
            {
                var operationWithEntryNode = blockWithEntryNodeInCondition.BranchValue;

                var lowestOperationContaining = GetLowestChildContaining(operationWithEntryNode, entryNode, 0);
                var entryNodeOperationForReachability = lowestOperationContaining.Operation;

                GetOperationsFromBlockContainingEntryNode(operationsInOrderVisited, blockWithEntryNodeInCondition, methodBlocks, entryNodeOperationForReachability, operationWithEntryNode);
            }

            var ops = operationsInOrderVisited.ToImmutable();

            var localsFromEntryNode = entryOperationInitializesLocals.ToImmutable();
            var capturesFromEntryNode = entryOperationInitializesCaptures.ToImmutableList();

            var (locals, captureIds) = GetLocalAndCapturesUsedBeforeAssigned(ops, localsFromEntryNode, capturesFromEntryNode);

            var allFlowCaptures = methodBlocks.SelectMany(b => b.Operations).SelectMany(o => o.DescendantsAndSelf()).OfType<IFlowCaptureOperation>().Distinct().ToImmutableList();
            var capturesNeedCapture = captureIds.Select(i => allFlowCaptures.Single(f => f.Id.Equals(i)).Value.Type).ToImmutableList();

            return (operationsInOrderVisited.ToImmutable(), locals, capturesNeedCapture);

            static (ImmutableList<ILocalSymbol> Locals, ImmutableList<CaptureId> ImplicitCaptures) GetLocalAndCapturesUsedBeforeAssigned(
                ImmutableList<IOperation> ops,
                ImmutableList<ILocalSymbol> alreadyAssignedLocals,
                ImmutableList<CaptureId> alreadyCaptured
            )
            {
                var pending = new Stack<IOperation>();
                foreach (var op in ops.Reverse())
                {
                    pending.Push(op);
                }

                var locNeedCapture = ImmutableHashSet.CreateBuilder<ILocalSymbol>();
                var locAssigned = ImmutableHashSet.CreateBuilder<ILocalSymbol>();
                foreach (var loc in alreadyAssignedLocals)
                {
                    locAssigned.Add(loc);
                }

                var captureNeedCapture = ImmutableHashSet.CreateBuilder<CaptureId>();
                var captureAssigned = ImmutableHashSet.CreateBuilder<CaptureId>();
                foreach (var capture in alreadyCaptured)
                {
                    captureAssigned.Add(capture);
                }

                while (pending.Count > 0)
                {
                    var curOp = pending.Pop();
                    if (curOp is ILocalReferenceOperation locRef)
                    {
                        if (locAssigned.Add(locRef.Local))
                        {
                            // first time we've seen it, means it wasn't initialized in these ops... so we need to capture it
                            locNeedCapture.Add(locRef.Local);
                        }
                    }
                    else if (curOp is IAssignmentOperation assignOp)
                    {
                        var initializedRefs = assignOp.Target.DescendantsAndSelf().OfType<ILocalReferenceOperation>();
                        foreach (var initializedRef in initializedRefs)
                        {
                            locAssigned.Add(initializedRef.Local);
                        }

                        pending.Push(assignOp.Value);
                    }
                    else if (curOp is IFlowCaptureReferenceOperation captureRef)
                    {
                        if (captureAssigned.Add(captureRef.Id))
                        {
                            // first time we've seen it, means it wasn't initialized in these ops... so we need to capture it
                            captureNeedCapture.Add(captureRef.Id);
                        }
                    }
                    else if (curOp is IFlowCaptureOperation captureOp)
                    {
                        captureAssigned.Add(captureOp.Id);

                        pending.Push(captureOp.Value);
                    }
                    else
                    {
                        foreach (var child in curOp.Children)
                        {
                            pending.Push(child);
                        }
                    }
                }

                return (locNeedCapture.ToImmutableList(), captureNeedCapture.ToImmutableList());
            }

            static void GetOperationsFromBlockContainingEntryNode(
                ImmutableList<IOperation>.Builder operationsInExecutionOrder,
                BasicBlock blockWithEntryNode,
                ImmutableArray<BasicBlock> methodBlocks,
                IOperation entryNodeOperationForReachability,
                IOperation operationWithEntryNode
            )
            {
                var visitedBlocks = ImmutableArray.CreateBuilder<BasicBlock>();
                visitedBlocks.Add(blockWithEntryNode);

                // get everything _in_ the entryNode operation that happens after the entryNode
                GetOperationsReachableAfterOperation(operationsInExecutionOrder, entryNodeOperationForReachability, operationWithEntryNode);

                // figure out what happens from falling through (or branching out of) the initial block
                var subVisitedBlocks = VisitBlocks(operationsInExecutionOrder, blockWithEntryNode, includeOperationsFromFirstBlock: false);
                foreach (var block in subVisitedBlocks)
                {
                    visitedBlocks.Add(block);
                }

                // figure out what blocks (and operations) get pulled in because of try/catch/finally
                var opsFromCatchFinally = ImmutableList.CreateBuilder<IOperation>();

                var alreadyVisited = ImmutableHashSet.CreateBuilder<BasicBlock>();
                var blocksToVisitHandlers = new Stack<BasicBlock>(visitedBlocks);
                while (blocksToVisitHandlers.Count > 0)
                {
                    var block = blocksToVisitHandlers.Pop();

                    var catchFinallyBlocks = GetHandlersVisitedByExisting(block, methodBlocks);
                    foreach (var subBlock in catchFinallyBlocks)
                    {
                        if (!alreadyVisited.Add(subBlock))
                        {
                            continue;
                        }

                        var newlyVisitedBlocks = VisitBlocks(operationsInExecutionOrder, subBlock, true);

                        foreach (var newBlock in newlyVisitedBlocks)
                        {
                            blocksToVisitHandlers.Push(newBlock);
                        }
                    }
                }
            }

            static (IOperation Operation, int Depth) GetLowestChildContaining(IOperation haystack, IOperation needle, int depth)
            {
                var best = (Operation: haystack, Depth: depth);

                foreach (var child in haystack.Children)
                {
                    if (!ContainsOperation(child, needle))
                    {
                        continue;
                    }

                    var lowest = GetLowestChildContaining(child, needle, depth + 1);
                    if (lowest.Depth > best.Depth)
                    {
                        best = lowest;
                    }
                }

                return best;
            }

            static ImmutableList<BasicBlock> GetHandlersVisitedByExisting(BasicBlock block, ImmutableArray<BasicBlock> allMethodBlocks)
            {
                var catchesToVisit = ImmutableList.CreateBuilder<ControlFlowRegion>();
                var finalliesToVisit = ImmutableList.CreateBuilder<ControlFlowRegion>();

                var cur = block.EnclosingRegion;
                while (cur.EnclosingRegion != null)
                {
                    var parent = cur.EnclosingRegion;
                    if (parent.Kind == ControlFlowRegionKind.TryAndCatch)
                    {
                        var indexOfSelfInSiblings = parent.NestedRegions.IndexOf(cur);

                        var relevantSubRegions =
                            parent
                                .NestedRegions
                                .Skip(indexOfSelfInSiblings + 1)        // skip past ourself, as we've run and earlier blocks are also done
                                .Where(x => x.Kind == ControlFlowRegionKind.Catch || x.Kind == ControlFlowRegionKind.Filter || x.Kind == ControlFlowRegionKind.FilterAndHandler);

                        catchesToVisit.AddRange(relevantSubRegions);
                    }
                    else if (parent.Kind == ControlFlowRegionKind.TryAndFinally)
                    {
                        var indexOfSelfInSiblings = parent.NestedRegions.IndexOf(cur);

                        var relevantSubRegions =
                            parent
                                .NestedRegions
                                .Skip(indexOfSelfInSiblings + 1)        // skip past ourself, as we've run and earlier blocks are also done
                                .Where(x => x.Kind == ControlFlowRegionKind.Finally);

                        finalliesToVisit.AddRange(relevantSubRegions);
                    }

                    cur = parent;
                }

                var blocksToVisit = ImmutableList.CreateBuilder<BasicBlock>();
                foreach (var catchBlock in catchesToVisit)
                {
                    var relevantBlocks = allMethodBlocks.Skip(catchBlock.FirstBlockOrdinal).Take(catchBlock.LastBlockOrdinal - catchBlock.FirstBlockOrdinal + 1);

                    blocksToVisit.AddRange(relevantBlocks);
                }

                foreach (var finallyBlock in finalliesToVisit)
                {
                    var relevantBlocks = allMethodBlocks.Skip(finallyBlock.FirstBlockOrdinal).Take(finallyBlock.LastBlockOrdinal - finallyBlock.FirstBlockOrdinal + 1);

                    blocksToVisit.AddRange(relevantBlocks);
                }

                return blocksToVisit.ToImmutable();
            }

            static ImmutableList<BasicBlock> VisitBlocks(
                ImmutableList<IOperation>.Builder operationsInExuectionOrder,
                BasicBlock entryBlock,
                bool includeOperationsFromFirstBlock
             )
            {
                var visited = ImmutableHashSet.CreateBuilder<BasicBlock>();

                var toExit = new Stack<BasicBlock>();
                toExit.Push(entryBlock);

                var includeAllOperations = includeOperationsFromFirstBlock;

                while (toExit.Count > 0)
                {
                    var cur = toExit.Pop();

                    if (!cur.IsReachable)
                    {
                        continue;
                    }

                    if (includeAllOperations && !visited.Add(cur))
                    {
                        continue;
                    }

                    if (includeAllOperations)
                    {
                        operationsInExuectionOrder.AddRange(cur.Operations);

                        if (cur.BranchValue != null)
                        {
                            operationsInExuectionOrder.Add(cur.BranchValue);
                        }
                    }

                    if (cur.ConditionalSuccessor?.Destination != null)
                    {
                        toExit.Push(cur.ConditionalSuccessor.Destination);
                    }

                    if (cur.FallThroughSuccessor?.Destination != null)
                    {
                        toExit.Push(cur.FallThroughSuccessor.Destination);
                    }

                    // after the entry block (which we've already handled) include everything
                    includeAllOperations = true;
                }

                return visited.OrderBy(x => x.Ordinal).ToImmutableList();
            }

            static void GetOperationsReachableAfterOperation(
                ImmutableList<IOperation>.Builder operationsInExuectionOrder,
                IOperation after,
                IOperation terminateAt
            )
            {
                var cur = after;
                while (cur != terminateAt)
                {
                    var parent = cur.Parent;
                    if (parent == null)
                    {
                        break;
                    }

                    var curIndexInParent = parent.Children.Select((x, ix) => (x, ix)).Single(t => t.x == cur).ix;
                    var rightOfCur = parent.Children.Skip(curIndexInParent + 1);

                    operationsInExuectionOrder.AddRange(rightOfCur);

                    cur = parent;
                }
            }

            static bool ContainsOperation(IOperation haystack, IOperation needle)
            {
                var isReference = haystack is IFlowCaptureReferenceOperation;

                // don't count references, because we're looking for the original
                if (!isReference && haystack.Syntax == needle.Syntax)
                {
                    return true;
                }

                foreach (var child in haystack.Children)
                {
                    if (ContainsOperation(child, needle))
                    {
                        return true;
                    }
                }

                return false;
            }

            static T? GetContaining<T>(IOperation op)
            {
                IOperation? cur = op;
                while (cur != null)
                {
                    if (cur is T mtd)
                    {
                        return mtd;
                    }

                    cur = cur.Parent;
                }

                return default;
            }
        }
    }
}
