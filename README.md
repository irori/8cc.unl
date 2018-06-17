# DEPRECATED
This is now integrated to [ELVM](https://github.com/shinh/elvm/) as the Unlambda backend.

# A C compiler in Unlambda
This is an Unlambda version of [@shinh](https://github.com/shinh)'s [C -> Brainfuck compiler](https://github.com/shinh/bflisp).

8cc.unl is a C compiler in Unlambda. This is generated from C code by the [BFS-targeted 8cc](https://github.com/shinh/8cc/tree/bfs) and [BFS assembly -> Unlambda translator](https://github.com/irori/8cc.unl/blob/master/unlcore.scm).

Unfortunately, compiled Unlambda program is slower than Brainfuck. It takes ~37 hours and >10GB memory to self-compile 8cc by 8cc.unl.
