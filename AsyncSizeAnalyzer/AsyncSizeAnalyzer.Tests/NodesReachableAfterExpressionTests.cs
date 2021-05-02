using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Xunit;

namespace AsyncSizeAnalyzer.Tests
{
    public class NodesReachableAfterExpressionTests
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
                                void Fizz()
                                {
                                    var a = 1;
                                    var b = 2;
                                    System.Console.WriteLine(a);
                                    System.Console.WriteLine(b);
                                }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();
            var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>());

            var model = comp.GetSemanticModel(tree);

            var statements = mtd.Body?.Statements;
            Assert.NotNull(statements);

            {
                var l1 = statements.Value[0];
                var afterL1 = Analyzer.NodesReachableAfterExpression(l1, model);
                var afterL1Text = MakeText(afterL1);
                Assert.Equal("var b = 2;\r\nSystem.Console.WriteLine(a);\r\nSystem.Console.WriteLine(b);", afterL1Text);
            }

            {
                var l2 = statements.Value[1];
                var afterL2 = Analyzer.NodesReachableAfterExpression(l2, model);
                var afterL2Text = MakeText(afterL2);
                Assert.Equal("System.Console.WriteLine(a);\r\nSystem.Console.WriteLine(b);", afterL2Text);
            }

            {
                var l3 = statements.Value[2];
                var afterL3 = Analyzer.NodesReachableAfterExpression(l3, model);
                var afterL3Text = MakeText(afterL3);
                Assert.Equal("System.Console.WriteLine(b);", afterL3Text);
            }

            {
                var aExp = statements.Value[2].DescendantNodesAndSelf().OfType<ArgumentSyntax>().Single();
                var afterAExp = Analyzer.NodesReachableAfterExpression(aExp, model);
                var afterAExpText = MakeText(afterAExp);
                Assert.Equal("System.Console.WriteLine(b);", afterAExpText);
            }

            {
                var l4 = statements.Value[3];
                var afterL4 = Analyzer.NodesReachableAfterExpression(l4, model);
                var afterL4Text = MakeText(afterL4);
                Assert.Equal("", afterL4Text);
            }

            {
                var bExp = statements.Value[3].DescendantNodesAndSelf().OfType<ArgumentSyntax>().Single();
                var afterBExp = Analyzer.NodesReachableAfterExpression(bExp, model);
                var afterBExpText = MakeText(afterBExp);
                Assert.Equal("", afterBExpText);
            }

            static string MakeText(IEnumerable<SyntaxNode> e)
            => string.Join("\r\n", e.OrderBy(x => x.GetLocation().SourceSpan.Start).Select(x => x.GetText().ToString().Trim()));
        }

        [Fact]
        public async Task ComplicatedExpressionAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task FizzAsync()
                                {
                                    var x = true;
                                    var y = false;
                                    var z = x ? await BuzzAsync(1, 2) : y;

                                    var q = 123;
                                    var t = 456;
                                    var u = 789;
                                    var w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };

                                    System.Console.WriteLine();
                                }

                                static bool Hello(int a, bool b, int c)
                                => a == c ? b : false;

                                System.Threading.Tasks.Task<bool> BuzzAsync(int a, int b)
                                => System.Threading.Tasks.Task.FromResult(true);
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();
            var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), q => q.Identifier.ValueText == "FizzAsync");

            var model = comp.GetSemanticModel(tree);

            var statements = mtd.Body?.Statements;
            Assert.NotNull(statements);

            {
                var l1 = statements.Value[0];
                var afterL1 = Analyzer.NodesReachableAfterExpression(l1, model);
                var afterL1Text = MakeText(afterL1);
                Assert.Equal("var y = false;\r\nvar z = x ? await BuzzAsync(1, 2) : y;\r\nvar q = 123;\r\nvar t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL1Text);
            }

            {
                var l2 = statements.Value[1];
                var afterL2 = Analyzer.NodesReachableAfterExpression(l2, model);
                var afterL2Text = MakeText(afterL2);
                Assert.Equal("var z = x ? await BuzzAsync(1, 2) : y;\r\nvar q = 123;\r\nvar t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL2Text);
            }

            {
                var l3 = statements.Value[2];
                var afterL3 = Analyzer.NodesReachableAfterExpression(l3, model);
                var afterL3Text = MakeText(afterL3);
                Assert.Equal("var q = 123;\r\nvar t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL3Text);
            }

            {
                var l3X = statements.Value[2].DescendantNodes().OfType<IdentifierNameSyntax>().Single(x => x.Identifier.ValueText == "x");
                var afterL3X = Analyzer.NodesReachableAfterExpression(l3X, model);
                var afterL3XText = MakeText(afterL3X);
                Assert.Equal("await BuzzAsync(1, 2)\r\ny\r\nvar q = 123;\r\nvar t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL3XText);
            }

            {
                var l3Async = statements.Value[2].DescendantNodes().OfType<AwaitExpressionSyntax>().Single();
                var afterL3Async = Analyzer.NodesReachableAfterExpression(l3Async, model);
                var afterL3AsyncText = MakeText(afterL3Async);
                Assert.Equal("var q = 123;\r\nvar t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL3AsyncText);
            }

            {
                var l3Y = statements.Value[2].DescendantNodes().OfType<IdentifierNameSyntax>().Single(y => y.Identifier.ValueText == "y");
                var afterL3Y = Analyzer.NodesReachableAfterExpression(l3Y, model);
                var afterL3YText = MakeText(afterL3Y);
                Assert.Equal("var q = 123;\r\nvar t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL3YText);
            }

            {
                var l4 = statements.Value[3];
                var afterL4 = Analyzer.NodesReachableAfterExpression(l4, model);
                var afterL4Text = MakeText(afterL4);
                Assert.Equal("var t = 456;\r\nvar u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL4Text);
            }

            {
                var l5 = statements.Value[4];
                var afterL5 = Analyzer.NodesReachableAfterExpression(l5, model);
                var afterL5Text = MakeText(afterL5);
                Assert.Equal("var u = 789;\r\nvar w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL5Text);
            }

            {
                var l6 = statements.Value[5];
                var afterL6 = Analyzer.NodesReachableAfterExpression(l6, model);
                var afterL6Text = MakeText(afterL6);
                Assert.Equal("var w = y switch { true => Hello(q, await BuzzAsync(4, t), u + 012), _ => z };\r\nSystem.Console.WriteLine();", afterL6Text);
            }

            {
                var l7 = statements.Value[6];
                var afterL7 = Analyzer.NodesReachableAfterExpression(l7, model);
                var afterL7Text = MakeText(afterL7);
                Assert.Equal("System.Console.WriteLine();", afterL7Text);
            }

            {
                var l7Y = statements.Value[6].DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Single(y => y.Identifier.ValueText == "y");
                var afterL7Y = Analyzer.NodesReachableAfterExpression(l7Y, model);
                var afterL7YText = MakeText(afterL7Y);
                Assert.Equal("true => Hello(q, await BuzzAsync(4, t), u + 012)\r\n_ => z\r\nSystem.Console.WriteLine();", afterL7YText);
            }

            {
                var l7Q = statements.Value[6].DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Single(y => y.Identifier.ValueText == "q");
                var afterL7Q = Analyzer.NodesReachableAfterExpression(l7Q, model);
                var afterL7QText = MakeText(afterL7Q);
                Assert.Equal("await BuzzAsync(4, t)\r\nu + 012\r\nSystem.Console.WriteLine();", afterL7QText);
            }

            {
                var l7Await = statements.Value[6].DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>().Single();
                var afterL7Await = Analyzer.NodesReachableAfterExpression(l7Await, model);
                var afterL7AwaitText = MakeText(afterL7Await);
                Assert.Equal("u + 012\r\nSystem.Console.WriteLine();", afterL7AwaitText);
            }

            {
                var l7U = statements.Value[6].DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Single(y => y.Identifier.ValueText == "u");
                var afterL7U = Analyzer.NodesReachableAfterExpression(l7U, model);
                var afterL7UText = MakeText(afterL7U);
                Assert.Equal("012\r\nSystem.Console.WriteLine();", afterL7UText);
            }

            {
                var l7Z = statements.Value[6].DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Single(y => y.Identifier.ValueText == "z");
                var afterL7Z = Analyzer.NodesReachableAfterExpression(l7Z, model);
                var afterL7ZText = MakeText(afterL7Z);
                Assert.Equal("System.Console.WriteLine();", afterL7ZText);
            }

            {
                var l8 = statements.Value[7];
                var afterL8 = Analyzer.NodesReachableAfterExpression(l8, model);
                var afterL8Text = MakeText(afterL8);
                Assert.Equal("", afterL8Text);
            }

            static string MakeText(IEnumerable<SyntaxNode> e)
            => string.Join("\r\n", e.OrderBy(x => x.GetLocation().SourceSpan.Start).Select(x => x.GetText().ToString().Trim()));
        }

        [Fact]
        public async Task BranchesAsync()
        {
            var comp =
                await TestHelpers.GetCompilationAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task FizzAsync(int q)
                                {
                                    for (var x = 0; x < 123; x++)
                                    {
                                        var b = x * 2;
                                        
                                        if (await BuzzAsync(x, 2))          // can reach both branches
                                        {
                                            System.Console.WriteLine();     // can't reach Hello(q)
                                            
                                            Hello(b);
                                        }
                                        else
                                        {
                                            Hello(q); // can't reach Hello(789)
                                            return;
                                        }
                                    }

                                    Hello(789);
                                }

                                System.Threading.Tasks.Task<bool> BuzzAsync(int a, int b)
                                => System.Threading.Tasks.Task.FromResult(System.Console.ReadLine().Length > 5);

                                void Hello(int c) { }
                            }
                      }"
                );

            var tree = Assert.Single(comp.SyntaxTrees);
            var root = tree.GetRoot();
            var mtd = Assert.Single(root.DescendantNodesAndSelf().OfType<MethodDeclarationSyntax>(), q => q.Identifier.ValueText == "FizzAsync");

            var model = comp.GetSemanticModel(tree);

            var body = mtd.Body;
            Assert.NotNull(body);

            {
                var forLoop = body.DescendantNodesAndSelf().OfType<ForStatementSyntax>().Single();
                var afterForLoop = Analyzer.NodesReachableAfterExpression(forLoop, model);
                var afterForLoopText = MakeText(afterForLoop);
                Assert.Equal("Hello(789);", afterForLoopText);
            }

            {
                var awaitExp = body.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>().Single();
                var afterAwaitExp = Analyzer.NodesReachableAfterExpression(awaitExp, model);
                var afterAwaitExpText = MakeText(afterAwaitExp);
                // this one looks weird because the if's condition only gets visited on the second pass through
                Assert.Equal("x < 123\r\nx++\r\nvar b = x * 2;\r\n\r\nif (await BuzzAsync(x, 2))          // can reach both branches\r\n{\r\nSystem.Console.WriteLine();     // can't reach Hello(q)\r\n\r\nHello(b);\r\n}\r\nelse\r\n{\r\nHello(q); // can't reach Hello(789)\r\nreturn;\r\n}\r\n{\r\nSystem.Console.WriteLine();     // can't reach Hello(q)\r\n\r\nHello(b);\r\n}\r\n{\r\nHello(q); // can't reach Hello(789)\r\nreturn;\r\n}\r\nHello(789);", afterAwaitExpText);
            }

            {
                var xppExp = body.DescendantNodesAndSelf().Single(x => x.ToString().Trim() == "x++");
                var afterXppExp = Analyzer.NodesReachableAfterExpression(xppExp, model);
                var afterXppExpText = MakeText(afterXppExp);
                Assert.Equal("x < 123\r\nx++\r\nvar b = x * 2;\r\n\r\nif (await BuzzAsync(x, 2))          // can reach both branches\r\n{\r\nSystem.Console.WriteLine();     // can't reach Hello(q)\r\n\r\nHello(b);\r\n}\r\nelse\r\n{\r\nHello(q); // can't reach Hello(789)\r\nreturn;\r\n}\r\nHello(789);", afterXppExpText);
            }

            static string MakeText(IEnumerable<SyntaxNode> e)
            => string.Join(
                   "\r\n",
                   e.OrderBy(
                        x => x.GetLocation().SourceSpan.Start
                   ).SelectMany(
                        x => x.GetText().ToString().Split("\r\n", System.StringSplitOptions.RemoveEmptyEntries)
                   )
                   .Select(t => t.Trim())
                );

        }
        
        // todo: need to test all the special cases
    }
}
