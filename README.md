# Proof of Concept - Not Actually Fit To Use

## Async Size Analyzer

This is an analyzer which estimates how much storage is needed for a state machine if an `await` expression has to suspend.

## Config

Slap an `ASA.txt` in your project, and a `<AdditionalFiles Include="ASA.txt" />` in an `ItemGroup` in your csproj file.  Yes this stinks, but config isn't the point.

The first number in the file is the number of bytes to warn about (set it < 0 to warn on every await).

You can grab the nuget package from the repo.  Put it in a folder, add the folder as source, then add a reference to the package.

## Limitations

Roslyn doesn't expose enough to really see what it's doing with state machine generation.  This is maybe <strike>8-hours</strike> 16-hours worth of hacking, so a complete re-implementation of the relevant state tracking is... buggy, I guarantee it.  It'd be nice if `SemanticModel` could expose this information.

In theory this can deal with any sort of C# control flow, but in practice only relatively simple code is in the test cases.  I ran it against a version of Cesil (which is pretty complicated) and the results are "believable".

That said, there are something things that are definitell wrong:

 - always assumes references are 8 bytes
 - assumes generic types are 0 bytes
 - assumes fields can be perfectly packed, so there's no need for padding in the state machine

## What's it look like?

![this](https://i.stack.imgur.com/Za2hB.png)