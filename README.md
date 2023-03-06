# A-Functional-Language-for-Parallel-Computing

This is the first domain-specific language (DSL) ever designed by me.

There are three main goals in the design of programming languages (PLs):
1. Performance,
2. Safety,
3. Productivity.

While modern **statically typed functional languages** like *ML, Haskell, Scala* do well in the last two, and some *cumbersome* languages like *C++, Rust* are good at the first, there is still a huge gap between this two kinds.

I believe this gap can be bridged by compilers -- with the effort of PL and compiler people, we will certainly have a language (maybe domain-specific, but still general enough to matter) that achieves all the three goals.

This DSL involves three things I am the most fascinated about:
1. Performance engineering and parallel computing
2. Compiler development
3. PLs and functional programming
 
I have fallen in love with computer systems for years, but have little experience in the research of this field.
This is the first time I have ever involved in a project whose feasibility is so uncertain.
If I do not do well, this will still be a *glorious failure" (quoted from the preface of Hennessy and Patterson) for me.

I will keep updating this project even after the winter quarter of 2023.

## Tough Things
1. Low-level IR close to CUDA
2. Expression-oriented style in FP to statement-oriented style in CUDA
