# A-Functional-Language-for-Parallel-Computing

This is the first domain-specific language (DSL) ever designed by me.

There are three main goals in the design of programming languages (PLs):
1. Performance,
2. Safety,
3. Productivity.

While modern **statically typed functional languages** like *ML, Haskell, Scala* do well in the last two, and some *cumbersome* languages like *C++, Rust* are good at the first, there is still a huge gap between this two kinds.

I believe this gap can be bridged by compilers -- with the effort of PL and compiler people, we will certainly have a language (maybe domain-specific, but still general enough to matter) that achieves all the three goals.
Hopefully, more and more research work is emerging in related fields.

To explore the feasibility, this project designs a DSL embedded in Scala, and produces CUDA code.
If the idea is feasible, we can have a language portable to different hardware platform, but this project targets only CUDA at the moment, since it is widely available and its auto-scheduling is a well-known hard problem.

***"We choose to go to the moon, not because it's easy, but because it's hard."***

This DSL involves three things I am the most fascinated about:
1. Performance engineering and parallel computing
2. Compiler development
3. PLs and functional programming
 
I have fallen in love with computer systems for years, but have little experience in the research of this field.
This is the first time I have ever involved in a project whose feasibility is so uncertain.
If I do not do well, this will still be a *glorious failure* (quoted from the preface of Hennessy and Patterson) for me.

I will keep updating this project even after the winter quarter of 2023.

## Related Work
### Industrial Strength
1. CUDA Thrust (https://docs.nvidia.com/cuda/thrust/): performant, hard to use
2. OpenAI Triton (https://openai.com/research/triton): not functional enough, still too imperative
3. Scala Parallel Collections (https://docs.scala-lang.org/overviews/parallel-collections/overview.html): easy to use, only supporting CPU multithreading
### Academic Work (Still Incomplete)
1. LIFT (https://michel.steuwer.info/files/publications/2017/CGO-2017.pdf, https://michel.steuwer.info/files/publications/2019/FHPNC-1-2019.pdf)
2. Kokkos (https://kokkos.github.io/kokkos-core-wiki/)
3. Sequoia (http://graphics.stanford.edu/papers/sequoia/sequoia_sc06.pdf)

## Tough Things
1. Low-level IR close to CUDA
    - Two IRs are needed: 
      - high-level, basically abstract syntax tree (AST) of the source language, expression-oriented;
      - low-level, close to CUDA, statement-oriented.
2. Expression-oriented style in FP to statement-oriented style in CUDA.
3. Achieving high performance while maintaining functional and easy-to-use APIs (previous work sacrifice the latter for the former, but I will try the other way -- always keeping the APIs functional and easy-to-use, and making attempts to improve its performance).
4. In CUDA, synchronization can only occur within the same block, which implies:
   - Narrow dependencies are still easy to cope with, since previous results can be kept in registers.
   - Wide intra-block dependencies are not hard, since previous results can be kept in shared memory. In this case, although `__syncthreads()` is required, we can still put all the code within the same function.
   - Wide inter-block dependencies are truly hard to manage automatically. To wait for all blocks to finish a stage of computation, a function call is required because there is no primitive to synchronize all blocks. Newly allocated memory is also required to hold intermediate result. At the moment, I understand why *Triton* asks the user to manually manage inter-block dependencies.
5. Some CUDA threads should do nothing because they are out of bound, e.g., an array has only 31 elements but 32 (equal to warp size) CUDA threads are used.
6. Each CUDA thread can be responsible for the computation of multiple elements in an array, which requires a for loop, and it is even harder in terms of the size of shared memory we need to allocate for this array..

## Requirements
Scala 3 (no other dependency required)
