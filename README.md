# Proof of Concept - Not Actually Fit To Use

## Async Size Analyzer

This is an analyzer which estimates how much storage is needed for a state machine if an `await` expression has to suspend.

## Config

<strike>
Slap an `ASA.txt` in your project, and a `<AdditionalFiles Include="ASA.txt" />` in an `ItemGroup` in your csproj file.  Yes this stinks, but config isn't the point.
</strike>

With 1.0.2 you can configure via an `.editorconfig` file.  Put `dotnet_diagnostic.ASA1000.warn_when_larger_than_bytes = <some number>` into the relevant `.editorconfig` for your project.  You may need to add it as an additional file, with a 

```
 <ItemGroup>
    <AdditionalFiles Include="$(MSBuildThisFileDirectory)\.editorconfig" />
  </ItemGroup>
```

in the `.csproj` depending on your setup.

You can grab the nuget package from the repo.  Put it in a folder, add the folder as source, then add a reference to the package.

## Limitations

Roslyn doesn't expose enough to really see what it's doing with state machine generation.  This is maybe <strike>8-hours</strike> 16-hours worth of hacking, so a complete re-implementation of the relevant state tracking is... buggy, I guarantee it.  It'd be nice if `SemanticModel` could expose this information.

In theory this can deal with any sort of C# control flow, but in practice only relatively simple code is in the test cases.  I ran it against a version of Cesil (which is pretty complicated) and the results are "believable".

That said, there are something things that are definitely wrong:

 - always assumes references are 8 bytes
 - assumes generic types are 0 bytes
 - assumes fields can be perfectly packed, so there's no need for padding in the state machine

## What's it look like?

![this](https://i.stack.imgur.com/Za2hB.png)
