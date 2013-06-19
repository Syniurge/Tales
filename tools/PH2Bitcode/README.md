When generating IR/bitcode files from the headers of a library (eg. Ogre, Bullet, cAudio, etc.) and from TalesRuntime.c we need to preserve all class/struct/union types, function and variable declarations even if they aren't used by a function definition.

Vanilla LLVM and Clang write all those declarations on demand, and the only current way to make them emit the needed declarations is to modify their code.
Patches are generated between the modified files and the original versions taken from LLVM/Clang 3.3 then applied to a LLVM source tree (despite the fairly high number of files I've tried to keep the changes minimal so that they remain compatible with LLVM releases for as long as possible).

The resulting Clang driver, TalesClang, can take a pseudo-header parameter (-pseudoheader-whitelist option) that contains the declarations to whitelist for emission. When no Tales-specific parameter is passed it behaves exactly like the vanilla driver.