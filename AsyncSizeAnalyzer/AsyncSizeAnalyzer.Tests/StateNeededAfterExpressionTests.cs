using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;
using Xunit;

namespace AsyncSizeAnalyzer.Tests
{
    public class StateNeededAfterExpressionTests
    {
        [Fact]
        public async Task SimpleAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                int Q => 17;
                                int C => 4;

                                async System.Threading.Tasks.Task Fizz(int a, int x)
                                {
                                    var dead = 123 + x;
                                    var b = 2 + Q;
                                    await System.Threading.Tasks.Task.Yield();
                                    System.Console.WriteLine(a + b + C);
                                }
                            }

                            static class Hello
                            {
                                static int Q => 17;
                                static int C => 4;

                                static async System.Threading.Tasks.Task Fuzz(int a, int x)
                                {
                                    var dead = 123 + x;
                                    var b = 2 + Q;
                                    await System.Threading.Tasks.Task.Yield();
                                    System.Console.WriteLine(a + b + C);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // fizz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("b", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // fuzz
            {
                var fuzzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fuzz");

                var statements = fuzzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fuzzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("b", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task ForAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                int Q => 17;
                                int C => 4;
                                static int T => 15;

                                System.Threading.Tasks.Task<int> GetIntAsync()
                                => System.Threading.Tasks.Task.FromResult(7);

                                static System.Threading.Tasks.Task<int> GetStaticIntAsync()
                                => System.Threading.Tasks.Task.FromResult(9);

                                async System.Threading.Tasks.Task NoBody(int a, int x)
                                {
                                    for(var i = a + Q; i < x; i++)
                                        await System.Threading.Tasks.Task.Yield();
                                }

                                // A & Q aren't needed after await
                                async System.Threading.Tasks.Task Fizz(int a, int x)
                                {
                                    for(var i = a + Q; i < x; i++)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                        System.Console.WriteLine(x + C);
                                    }
                                }

                                // A & Q aren't needed after await, and this isn't needed
                                async System.Threading.Tasks.Task Buzz(int a, int x)
                                {
                                    for(var i = a + Q; i < x; i++)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                        System.Console.WriteLine(x + T);
                                    }
                                }

                                // a is needed after wait, this is needed
                                async System.Threading.Tasks.Task Hello(int a, int x)
                                {
                                    for(var i = await GetIntAsync() + a; i < x; i++)
                                    {
                                        System.Console.WriteLine(x + C);
                                    }
                                }

                                // a isn't needed after wait, this isn't needed
                                async System.Threading.Tasks.Task World(int a, int x)
                                {
                                    for(var i = a + await GetIntAsync(); i < x; i++)
                                    {
                                        System.Console.WriteLine(x + T);
                                    }
                                }

                                // a isn't needed after wait, this is needed
                                async System.Threading.Tasks.Task Post(int a, int x)
                                {
                                    for(var i = a; i < await GetIntAsync() + x; i++)
                                    {
                                        System.Console.WriteLine(x + C);
                                    }
                                }

                                // a isn't needed after wait, this isn't needed (the awaited method is no longer an instance method)
                                async System.Threading.Tasks.Task Mail(int a, int x)
                                {
                                    // trick here is the condition is evaluated multiple times, so it doesn't matter that it was moved
                                    for(var i = a; i < x + await GetStaticIntAsync(); i++)
                                    {
                                        System.Console.WriteLine(x + T);
                                    }
                                }

                                // a isn't needed after wait, this is needed
                                async System.Threading.Tasks.Task News(int a, int x)
                                {
                                    for(var i = a; i < x; i += await GetIntAsync())
                                    {
                                        System.Console.WriteLine(x + T);
                                    }
                                }

                                // a isn't needed after wait, this isn't needed
                                async System.Threading.Tasks.Task Nite(int a, int x)
                                {
                                    for(var i = a; i < x; i += await GetStaticIntAsync())
                                    {
                                        System.Console.WriteLine(x + T);
                                    }
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // NoBody
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "NoBody");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("x", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // fizz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("x", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // buzz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("x", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // hello
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Hello");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters.OrderBy(x => x.Name),
                    p => Assert.Equal("a", p.Name),
                    p => Assert.Equal("x", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // world
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "World");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("x", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // post
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Post");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("x", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // mail
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Mail");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("x", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // news
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "News");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("x", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // night
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Nite");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("x", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task WhileAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                int Q => 17;
                                int C => 4;
                                static int T => 15;

                                System.Threading.Tasks.Task<int> GetIntAsync()
                                => System.Threading.Tasks.Task.FromResult(7);

                                static System.Threading.Tasks.Task<int> GetStaticIntAsync()
                                => System.Threading.Tasks.Task.FromResult(9);

                                async System.Threading.Tasks.Task NoBody(int a, int x)
                                {
                                    var i = 123;
                                    while(a < i + Q)
                                        await System.Threading.Tasks.Task.Yield();
                                }

                                async System.Threading.Tasks.Task Fizz(int a, int x)
                                {
                                    System.Console.WriteLine(x);

                                    var i = 123;
                                    while(a < Q + i)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task Buzz(int a, int x)
                                {
                                    System.Console.WriteLine(x);

                                    var i = 123;
                                    while(a < i + await GetStaticIntAsync())
                                    {
                                        System.Console.WriteLine(i);
                                    }
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // NoBody
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "NoBody");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // Fizz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // Buzz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task DoAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                int Q => 17;
                                int C => 4;
                                static int T => 15;

                                System.Threading.Tasks.Task<int> GetIntAsync()
                                => System.Threading.Tasks.Task.FromResult(7);

                                static System.Threading.Tasks.Task<int> GetStaticIntAsync()
                                => System.Threading.Tasks.Task.FromResult(9);

                                async System.Threading.Tasks.Task NoBody(int a, int x)
                                {
                                    var i = 123;
                                    do
                                        await System.Threading.Tasks.Task.Yield();
                                    while(a < i + Q);
                                }

                                async System.Threading.Tasks.Task Fizz(int a, int x)
                                {
                                    System.Console.WriteLine(x);

                                    var i = 123;
                                    do
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                    while(a < Q + i);
                                }

                                async System.Threading.Tasks.Task Buzz(int a, int x)
                                {
                                    System.Console.WriteLine(x);

                                    var i = 123;
                                    
                                    do
                                    {
                                        System.Console.WriteLine(i);
                                    }
                                    while(a < i + await GetStaticIntAsync());
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // NoBody
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "NoBody");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // Fizz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }

            // Buzz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Collection(state.Value.Parameters, p => Assert.Equal("a", p.Name));
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task ForeachAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                int Q => 17;
                                int C => 4;
                                static int T => 15;

                                System.Threading.Tasks.Task<int> GetIntAsync()
                                => System.Threading.Tasks.Task.FromResult(7);

                                static System.Threading.Tasks.Task<int[]> GetStaticIntsAsync()
                                => System.Threading.Tasks.Task.FromResult(new [] { 9 });

                                async System.Threading.Tasks.Task NoBody(int a, int[] x)
                                {
                                    foreach(var _ in x)
                                        await System.Threading.Tasks.Task.Yield();
                                }

                                async System.Threading.Tasks.Task Fizz(int a, int[] x)
                                {
                                    System.Console.WriteLine(x);

                                    var i = System.Console.ReadLine();
                                    foreach(var _ in x)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                        var t = Q + i; 
                                    }
                                }

                                async System.Threading.Tasks.Task Buzz(int a, int x)
                                {
                                    System.Console.WriteLine(x);

                                    var i = System.Console.ReadLine();
                                    
                                    foreach(var q in await GetStaticIntsAsync())
                                    {
                                        System.Console.WriteLine(i + q);
                                    }
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // NoBody
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "NoBody");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    x => Assert.Equal("System.Collections.IEnumerator", x.ToString())
                );
                Assert.False(state.Value.ThisIsReferenced);
            }

            // Fizz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(state.Value.Locals, l => Assert.Equal("i", l.Name));
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    x => Assert.Equal("System.Collections.IEnumerator", x.ToString())
                );
                Assert.True(state.Value.ThisIsReferenced);
            }

            // Buzz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Collection(
                    state.Value.Locals.OrderBy(x => x.Name),
                    l => Assert.Equal("i", l.Name)
                );
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task UnreachableAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Fizz(int a, int b, int c)
                                {
                                    if (a == b)
                                    {
                                        await System.Threading.Tasks.Task.Yield();

                                        System.Console.WriteLine(a+b);

                                        return;
                                    }

                                    System.Console.WriteLine(c);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Fizz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters.OrderBy(x => x.Name),
                    l => Assert.Equal("a", l.Name),
                    l => Assert.Equal("b", l.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task GotosAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Buzz(int a, int b, int c)
                                {
                                    if(a == b)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                        goto skip;
                                    }

                                    System.Console.WriteLine(c);
                                    return;
                        
                                    skip:
                                    System.Console.WriteLine(a);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Buzz
            {
                var fizzMtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = fizzMtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(fizzMtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    l => Assert.Equal("a", l.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task ForeachEnumeratorsAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                static System.Threading.Tasks.Task<int[]> GetArrayAsync()
                                => null!;

                                static System.Threading.Tasks.Task<System.Collections.Generic.IEnumerable<int>> GetEnumerableAsync()
                                => null!;

                                static System.Threading.Tasks.Task<System.Collections.Generic.List<int>> GetListAsync()
                                => null!;

                                async System.Threading.Tasks.Task BuiltIn()
                                {
                                    foreach(var a in await GetArrayAsync())
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                }

                                async System.Threading.Tasks.Task BuiltIn2(int[] x)
                                {
                                    foreach(var a in x)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task Interface()
                                {
                                    foreach(var a in await GetEnumerableAsync())
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                }

                                async System.Threading.Tasks.Task Interface2(System.Collections.Generic.IEnumerable<int> x)
                                {
                                    foreach(var a in x)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task DuckTyped()
                                {
                                    foreach(var a in await GetListAsync())
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                }

                                async System.Threading.Tasks.Task DuckTyped2(System.Collections.Generic.List<int> x)
                                {
                                    foreach(var a in x)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // int[]
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "BuiltIn");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // int[], but capturing the enumerator
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "BuiltIn2");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    l => Assert.Equal("System.Collections.IEnumerator", l.ToString())
                );
                Assert.False(state.Value.ThisIsReferenced);
            }

            // interface
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Interface");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // interface, but capturing the enumerator
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Interface2");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    l => Assert.Equal("System.Collections.Generic.IEnumerator<int>", l.ToString())
                );
                Assert.False(state.Value.ThisIsReferenced);
            }

            // duck typed
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "DuckTyped");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // duck typed, but capturing the enumerator
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "DuckTyped2");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);

                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    l => Assert.Equal("System.Collections.Generic.List<int>.Enumerator", l.ToString())
                );
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task StaticTakingSelfAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                int Q => 2;

                                System.Threading.Tasks.Task<int[]> GetArrayAsync()
                                => null!;

                                static async System.Threading.Tasks.Task Fizz(Bar self)
                                {
                                    await self.GetArrayAsync();
                                    System.Console.WriteLine(self.Q);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("self", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task DeclaredAfterAwaitAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                static async System.Threading.Tasks.Task Fizz(Bar self)
                                {
                                    await System.Threading.Tasks.Task.Yield();
                                    var i = 2;
                                    System.Console.WriteLine(i);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task TryCatchFinallyAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Fizz(int a)
                                {                                   
                                    try
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                        System.Console.WriteLine(a);
                                    }
                                    catch
                                    {
                                        var i = 0;
                                        System.Console.WriteLine(i);
                                    }
                                }

                                async System.Threading.Tasks.Task Buzz(int a)
                                {                                  
                                    var i = 0;
                                    try
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                    catch
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                    finally
                                    {
                                        System.Console.WriteLine(i);
                                    }               
                                }

                                async System.Threading.Tasks.Task Hello(int a)
                                {                                  
                                    var i = 0;
                                    try
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                    catch(System.Exception e)
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                        System.Console.WriteLine(e);
                                    }
                                    finally
                                    {
                                        System.Console.WriteLine(i);
                                    }               
                                }

                                async System.Threading.Tasks.Task World(int a)
                                {                                  
                                    var i = 0;
                                    try
                                    {
                                         await System.Threading.Tasks.Task.Yield();
                                    }
                                    catch(System.Exception e)
                                    {
                                        System.Console.WriteLine(e);
                                    }
                                    finally
                                    {
                                        System.Console.WriteLine(i);
                                    }               
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    a => Assert.Equal("a", a.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // buzz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals,
                    i => Assert.Equal("i", i.Name)
                );
                Assert.Collection(
                    state.Value.Parameters,
                    a => Assert.Equal("a", a.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // hello
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Hello");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals.OrderBy(o => o.Name),
                    i => Assert.Equal("e", i.Name),
                    i => Assert.Equal("i", i.Name)
                );
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // world
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "World");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals,
                    i => Assert.Equal("i", i.Name)
                );
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task IntroducesVariableAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                System.Threading.Tasks.Task<int> GetFooAsync()
                                => null!;

                                async System.Threading.Tasks.Task Fizz(int a)
                                {                
                                    var b = await GetFooAsync();

                                    System.Console.WriteLine(b);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task LabelledAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                System.Threading.Tasks.Task<int> GetFooAsync()
                                => null!;

                                async System.Threading.Tasks.Task Fizz(int a)
                                {                
                                    lab:
                                    await GetFooAsync();

                                    System.Console.WriteLine(a);
                                    goto lab;
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    a => Assert.Equal("a", a.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.True(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task NestedTryCatchFinallyAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Fizz(int a, int b, int c)
                                {                
                                    try
                                    {
                                        System.Console.WriteLine(a);
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                    catch(System.ArgumentException)
                                    {
                                        System.Console.WriteLine(b);
                                    }
                                    catch
                                    {
                                        System.Console.WriteLine(c);
                                    }
                                }

                                async System.Threading.Tasks.Task Buzz(int a, int b, int c)
                                {                
                                    try
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                    catch(System.ArgumentException)
                                    {
                                        System.Console.WriteLine(b);
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                    catch
                                    {
                                        System.Console.WriteLine(c);
                                    }
                                }

                                async System.Threading.Tasks.Task Hello(int a, int b, int c)
                                {                
                                    try
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                    catch(System.ArgumentException)
                                    {
                                        System.Console.WriteLine(b);
                                    }
                                    catch
                                    {
                                        System.Console.WriteLine(c);
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task World(int a, int b, int c)
                                {                
                                    try
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                    catch(System.ArgumentException e)
                                    {
                                        try
                                        {
                                            System.Console.WriteLine(b);
                                            await System.Threading.Tasks.Task.Yield();
                                        }
                                        catch(System.ArgumentException)
                                        {
                                            System.Console.WriteLine(e);
                                        }
                                    }
                                    catch
                                    {
                                        System.Console.WriteLine(c);
                                    }
                                }

                                async System.Threading.Tasks.Task Ice(int a, int b, int c)
                                {                
                                    try
                                    {
                                        System.Console.WriteLine(a);
                                    }
                                    catch(System.ArgumentException e)
                                    {
                                        try
                                        {
                                            System.Console.WriteLine(b);
                                        }
                                        finally
                                        {
                                            System.Console.WriteLine(e);
                                            await System.Threading.Tasks.Task.Yield();
                                        }
                                    }
                                    finally
                                    {
                                        System.Console.WriteLine(c);
                                    }
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    a => Assert.Equal("b", a.Name),
                    a => Assert.Equal("c", a.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // Buzz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    a => Assert.Equal("c", a.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // Hello
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Hello");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // World
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "World");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals,
                    l => Assert.Equal("e", l.Name)
                );
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("c", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // Ice
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Ice");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Collection(
                    state.Value.Parameters,
                    p => Assert.Equal("c", p.Name)
                );
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task UsingBlocksAsync()
        {
            var comp =
               await TestHelpers.GetCompilationAsync(
                   @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Fizz(System.Collections.Generic.List<int> a)
                                {                
                                    using(var e = a.GetEnumerator())
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task Buzz(System.Collections.Generic.List<int> a)
                                {                
                                    using(a.GetEnumerator())
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task Hello(System.Func<System.IO.MemoryStream> a)
                                {                
                                    await using(var e = a())
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }

                                async System.Threading.Tasks.Task World(System.Func<System.IO.MemoryStream> a)
                                {                
                                    await using(a())
                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }
                                }
                            }
                      }"
               );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals,
                    l => Assert.Equal("e", l.Name)
                );
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // Buzz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Buzz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    l => Assert.Equal("System.Collections.Generic.List<int>.Enumerator", l.ToString())
                );
                Assert.False(state.Value.ThisIsReferenced);
            }

            // Hello
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Hello");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals,
                    l => Assert.Equal("e", l.Name)
                );
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }

            // World
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "World");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Empty(state.Value.Locals);
                Assert.Empty(state.Value.Parameters);
                Assert.Collection(
                    state.Value.ImplicitLocals,
                    l => Assert.Equal("System.IO.MemoryStream", l.ToString())
                );
                Assert.False(state.Value.ThisIsReferenced);
            }
        }

        [Fact]
        public async Task UsingStatementAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Fizz(System.Collections.Generic.List<int> a)
                                {                
                                    using var e = a.GetEnumerator();

                                    {
                                        await System.Threading.Tasks.Task.Yield();
                                    }

                                    System.Console.WriteLine();
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();

            var model = comp.GetSemanticModel(tree);

            // Fizz
            {
                var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), m => m.Identifier.ValueText == "Fizz");

                var statements = mtd.Body?.Statements;
                Assert.NotNull(statements);

                var awaitStatement = Assert.Single(mtd.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>());

                var state = Analyzer.StateNeededAfterExpression((IAwaitOperation)model.GetOperation(awaitStatement));
                Assert.NotNull(state);
                Assert.Collection(
                    state.Value.Locals,
                    l => Assert.Equal("e", l.Name)
                );
                Assert.Empty(state.Value.Parameters);
                Assert.Empty(state.Value.ImplicitLocals);
                Assert.False(state.Value.ThisIsReferenced);
            }
        }
    }
}
