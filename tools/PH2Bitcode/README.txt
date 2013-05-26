When generating IR/bitcode files from the headers of a library (eg. Ogre, Bullet, cAudio, etc.) and from TalesRuntime.c we need to preserve all class/struct/union types even if they aren't used by a function definition.

Vanilla LLVM doesn't store the types in the modules themselves but in a LLVM context, so the type declarations are written on demand when a module is saved into bitcode.
To make Clang emit IR/bitcode for all declared types (inside a specific file or folder), both Clang and LLVM need to be modified.

The original files are kept in .orig, so that a patch can be generated if the modified file needs to be updated (SVN rev of LLVM/Clang/compiler-rt: r182710)