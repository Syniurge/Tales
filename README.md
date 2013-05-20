Tales
======

After a lot of tinkering with Lua I've started to notice some limitations that are unknown to most game programmers who chose Lua as their scripting language. Notably the costs of a global string table indexed by their hashes at string creation (including strings resulting from a concatenation), discussed for example in:
        http://lua-users.org/wiki/SpeedingUpStrings
can be a source of much choppiness and needs to be kept in mind by developers who rely on Lua for a large part of their game logic.

Tales is a LLVM-based Just-In-Time compiled language, which tries to be compatible with Lua's syntax while adding features that are essential for performance: optional static typing and classes.
But the main reasons to create a new language apart from performance considerations are Tales' upcoming features :
 - table/class ownership, needed for fragmented serialization
 - game networking features that will greatly simplify writing the game logic for a multiplayer game. They cannot be done with Lua for a number of reasons.

Tales is released functional but incomplete. Dynamic values, tables and functions (which are not return values) work. The reason for this early release is that I'm looking for work at the moment in a field loosely related to programming, and wish to show my programming skills.

I wish I had devoted more time to Tales and its related project, Crucible. The roadmap has been lying in a corner of my head since october (where I've tinkered with Lua source code, wrote Luabinder, ported Pluto to Lua 5.2, and implemented ownership in Lua and Pluto), but it's the result of only 3 weeks of work mostly during my April vacations.

Most of the syntax remains compatible with Lua, but it offers the possibility to statically specify types, and structure types, which should allow Tales to reach the speed of compiled languages such as C#. The idea is that a novice programmer whose main goal is to tell a story (Crucible is meant to be a spiritual successor to Neverwinter Nights) doesn't have to worry about types at all, but if the performance hit begins to be felt, he will already be fairly acquainted with programming and can simply learn to add the right keywords and class declarations (instead of tables with dynamic values) to improve the performance. But overall the language has to remain simple enough so that any piece of code isn't too arcane, i.e so that the code of any module (in Neverwinter Nights' terms) is easy to understand by other module makers. And for an experienced programmer, one reason to want a statically-typed language is to not have a second thought about implementing the entire game logic in Tales, such as procedural animation, RTS AI, etc., which may be too much for Lua to handle (every time it has to find a non-local variable, Lua skims through the chain of pairs of the environment table).

Granted, there is LuaJIT which uses a number of techniques to guess in advance the types of Lua objects, and even runtime monitoring techniques to spot possible optimizations and to recompile some functions accordingly. But LuaJIT still is so complex that it'd be extremely hard to add new features to the Lua language (see the time it took to implement the changes brought by Lua 5.2). Tales should be much less obscure in its functioning. It relies on LLVM which already allows pretty good optimization and makes Tales' code platform-agnostic. That clearness will come in handy when I'll be implementing the planned game networking features, such as universal client-side prediction, which I could never have done with LuaJIT.

Build
======

You can already test Tales through its simple evaluator, Scheherazade:

        mkdir build
        cd build
        cmake .. && make

Note that Tales requires uncommon third-parties such as Bisonc++, Flexc++ and libbobcat.

A testing function talesprint(value) is available in Scheherazade.

Language
=========

A proper language documentation will be written later, but the two most important currently implemented specific additions to Lua to be aware of are:

 - when a variable is not declared with a type or the type of a function argument isn't specified, they are Lua-like dynamic values, whereas if one of the type keywords ("number", "string" or "table") is specified, they are statically typed values.

 - tables can have a struct-like "statically-typed part" too, and that part is defined in the initial table expression. For example:

        table t = { number n, string message = "Oh wow!", function printN(table t) talesprint(t.n) talesprint("\n") talesprint(t.message) end }
        t.toto = "I'm a dynamic value."

  n, message and printN form a structure, while toto is a Lua-like dynamic value in the chain of pairs of t.
  You can only assign tables with an identical structure part to t, for example:

        table t = { number n, string message = "Oh wow!", function printN(table t) talesprint(t.n) talesprint("\n") talesprint(t.message) end }
        t = {}

  is forbidden. If you want to create a pure dynamic table, you should write:

        table t = { n = 0, message = "Oh wow!" }
        t.printN = function (table t) talesprint(t.n) talesprint("\n") talesprint(t.message) end

  NOTE: as in Lua you must specify n = 0, otherwise it'll be considered to be t[0] instead of t.n.