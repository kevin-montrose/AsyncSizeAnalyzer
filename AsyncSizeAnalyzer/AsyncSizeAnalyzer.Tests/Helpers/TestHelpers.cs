using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace AsyncSizeAnalyzer.Tests
{
    public static class TestHelpers
    {
        public static async Task<Compilation> GetCompilationAsync(
            string testFile,
            [CallerMemberName] string caller = null
        )
        {
            var trustedAssemblies = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")).Split(Path.PathSeparator);
            var systemAssemblies = trustedAssemblies.Where(p => Path.GetFileName(p).StartsWith("System.")).ToList();

            var references = systemAssemblies.Select(s => MetadataReference.CreateFromFile(s)).ToList();

            var projectName = $"AsyncSizeAnalyzers.Tests";
            var projectId = ProjectId.CreateNewId(projectName);

            var compilationOptions =
                new CSharpCompilationOptions(
                    OutputKind.DynamicallyLinkedLibrary,
                    allowUnsafe: true,
                    nullableContextOptions: NullableContextOptions.Enable,
                    optimizationLevel: OptimizationLevel.Release
                );

            var parseOptions = new CSharpParseOptions(LanguageVersion.CSharp8);

            var projectInfo =
                ProjectInfo.Create(
                    projectId,
                    VersionStamp.Create(),
                    projectName,
                    projectName,
                    LanguageNames.CSharp,
                    compilationOptions: compilationOptions,
                    parseOptions: parseOptions
                );

            var workspace = new AdhocWorkspace();

            var solution =
                workspace
                    .CurrentSolution
                    .AddProject(projectInfo);

            foreach (var reference in references)
            {
                solution = solution.AddMetadataReference(projectId, reference);
            }

            var csFile = $"{caller}.cs";
            var docId = DocumentId.CreateNewId(projectId, csFile);

            var project = solution.GetProject(projectId);

            project = project.AddDocument(csFile, testFile).Project;

            var comp = await project.GetCompilationAsync();
            var diags = comp.GetDiagnostics();

            Assert.Empty(diags);

            return comp;
        }

        public static async ValueTask<CompilationWithAnalyzers> GetCompilationWithAnalyzerAsync(
            string testFile,
            DiagnosticAnalyzer analyzer, 
            IEnumerable<(string Name, string Value)> editorConfig,
            [CallerMemberName] string caller = null
        )
        {
            var trustedAssemblies = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")).Split(Path.PathSeparator);
            var systemAssemblies = trustedAssemblies.Where(p => Path.GetFileName(p).StartsWith("System.")).ToList();

            var references = systemAssemblies.Select(s => MetadataReference.CreateFromFile(s)).ToList();

            var projectName = $"AsyncSizeAnalyzers.Tests";
            var projectId = ProjectId.CreateNewId(projectName);

            var compilationOptions =
                new CSharpCompilationOptions(
                    OutputKind.DynamicallyLinkedLibrary,
                    allowUnsafe: true,
                    nullableContextOptions: NullableContextOptions.Enable
                );

            var parseOptions = new CSharpParseOptions(LanguageVersion.CSharp8);

            var projectInfo =
                ProjectInfo.Create(
                    projectId,
                    VersionStamp.Create(),
                    projectName,
                    projectName,
                    LanguageNames.CSharp,
                    compilationOptions: compilationOptions,
                    parseOptions: parseOptions
                );

            var workspace = new AdhocWorkspace();

            var solution =
                workspace
                    .CurrentSolution
                    .AddProject(projectInfo);

            foreach (var reference in references)
            {
                solution = solution.AddMetadataReference(projectId, reference);
            }

            var csFile = $"{caller}.cs";
            var docId = DocumentId.CreateNewId(projectId, csFile);

            var project = solution.GetProject(projectId);

            project = project.AddDocument(csFile, testFile).Project;


            if (editorConfig.Any())
            {
                var sb = new StringBuilder();
                sb.AppendLine("root = true");
                sb.AppendLine("[*]");

                foreach (var (name, val) in editorConfig)
                {
                    sb.Append(name);
                    sb.Append(" = ");
                    sb.Append(val);
                    sb.AppendLine();
                }

                /*var editorConfigFile =
                    project.AddAdditionalDocument(
                        ".editorconfig",
                        SourceText.From(sb.ToString(), Encoding.UTF8)
                    );

                project = editorConfigFile.Project;
                solution = project.Solution;*/

                //project = project.AddAnalyzerConfigDocument(".editorconfig", SourceText.From(sb.ToString(), Encoding.UTF8), filePath: "/fake/path").Project;
                //project = project.AddAdditionalDocument(".editorconfig", SourceText.From(sb.ToString(), Encoding.UTF8)).Project;
                solution = project.Solution;
                solution = solution.AddAnalyzerConfigDocument(DocumentId.CreateNewId(projectId), ".editorconfig", SourceText.From(sb.ToString(), Encoding.UTF8), filePath: "/fake/path");
                project = solution.GetProject(projectId);
            }

            Assert.True(workspace.TryApplyChanges(solution));

            var comp = await project.GetCompilationAsync();
            var diags = comp.GetDiagnostics();

            Assert.Empty(diags);

            return
                comp.WithAnalyzers(
                    ImmutableArray.Create(analyzer)
                );
        }

        private class TextDocumentText : AdditionalText
        {
            private readonly TextDocument Inner;

            public TextDocumentText(TextDocument inner)
            {
                Inner = inner;
            }

            public override string Path => Inner.FilePath;

            public override SourceText GetText(CancellationToken cancellationToken = default)
            => Inner.GetTextAsync(cancellationToken).Result;
        }
    }
}
