using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;
using Xunit;

namespace AsyncSizeAnalyzer.Tests
{
    public class AnalyzerTests
    {
        [Fact]
        public async Task SimpleAsync()
        {
            var diags =
                await RunAnalyzerAsync(
                    @"namespace Foo
                      {
                            class Bar
                            {
                                async System.Threading.Tasks.Task Buzz(int a, int b, int c)
                                {
                                    await DoSomething(a);
                                    await DoSomething(b);
                                    await DoSomething(c);
                                }

                                System.Threading.Tasks.Task DoSomething(int x)
                                => null!;
                            }
                      }",
                    -1
                );

            Assert.Collection(
                diags,
                d1 => Assert.Equal("Await captures 16 bytes (> maximum of -1 bytes): `this`: 8 bytes, parameter `b`: 4 bytes, parameter `c`: 4 bytes", d1.GetMessage()),
                d2 => Assert.Equal("Await captures 12 bytes (> maximum of -1 bytes): `this`: 8 bytes, parameter `c`: 4 bytes", d2.GetMessage()),
                d3 => Assert.Equal("Await captures 0 bytes (> maximum of -1 bytes): ", d3.GetMessage())
            );
        }

        static async Task<ImmutableArray<Diagnostic>> RunAnalyzerAsync(string file, int maxBytes)
        {
            var comp = await TestHelpers.GetCompilationWithAnalyzerAsync(file, new Analyzer() { MaxSizeBytes = maxBytes });
            
            return await comp.GetAllDiagnosticsAsync();
        }
    }
}
