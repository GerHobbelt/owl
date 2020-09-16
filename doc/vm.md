## Runtime

Source code of the virtual machine is at c/ovm.c It consists of about 1500
lines of C responsible for implementing the primitive data types and
operations, garbage collection, loading FASL-encoded images and implementing a
simple register-based virtual machine for running the bytecode.

No external dependencies are allowed and the VM should compile and run as such
without extra configuration steps. The goal has been to keep all of the
required runtime code at around 1000 lines with 2000 as the absolute maximum.

## Data Representation

The VM works on registers having the width of a pointer, which is generally 32
or 64 bits. Values stored in registers are called words. There are two kinds
words: allocated and immediate ones. An immediate word holds some value and
information about its type within itself. An allocated word similarly has a few
bits of metadata, but mainly it's job is to point to memory where the rest of
the value is stored.

All pointers on 32- and 64-bit
machines have two or three zero bits at the bottom. One of these is used by the
GC, and the other is used to mark whether the number is an immediate value. As a consequence, a
lisp pointer is just a regular pointer.

Immediate values further have the type of the value and room for the actual
encoded value in the subsequent higher bits. An immediate 32-bit value is
encoded as:


                             .------------> 24-bit payload
                             |      .-----> type tag if immediate
                             |      |.----> immediateness
    .------------------------| .----||.---> mark bit (can only be 1 during gc)
    |                        | |    |||
   [pppppppp pppppppp pppppppp tttttt10]

An allocated value is represented simply as the pointer to the
corresponding object, because the pointers are always aligned
to 32-bit words and consequently the 2 lowest bits as always 0,
which means the value is treated as an allocated one.

Non-immediate values are called allocated, because they point to allocated
memory. In this case the pointer points to an immediate value, which is the
header of an allocated object. The header mainly contains the type of the object,
its size, and a flag whether the contents are themselves descriptors or just
raw data.


## Garbage Collection

Owl uses a single continuous chunk of memory. Originally only the sbrk() system
call was needed to adjust the size of the memory, but this was converted to more
complex malloc/resize because it turned out OSX does not support is properly.

The GC is order preserving. Objects are in general
allocated in the same or reverse order to how they are later read. Moving objects
around would cause unnecessary load on caches, so we want to preseve the order
in which they were allocated. Since we are also compacting the heap, this means
the GC usually makes the heap more cache friendly. This also makes it possible
to define total order on objects, which is an important feature for implementing
some core data structures efficiently.

Purely functional operation and order preserving heap result in a unidirectional
memory where pointers can only point down. This seemed to be a unique feature on
which very little existing information or algorithms were found, so it made sense
to desing a new GC taking advantage of the property.

The design relies on the ideas I first saw in threading garbage collectors.
In particular the paper "Efficient Garbage Compaction Algorithm" by Johannes
Martin (1982). In a nutshell, we find all the places pointing to a particular
object and reverse the links to a single chain, so that the original object holds
a linked list of all places where it was linked from. This makes it possible to place
the object anywhere in the heap, because traversing the list and restoring the pointers
makes it possible to point them to the new location.

This process is used to first thread all the pointers starting from VM registers.
All objects not reached from the registers have an empty thread of links pointing
to them, so they can be freed. The next step is to slide all objects down in the
heap and unthread the pointers to them.

This would be sufficient for reclaiming free space, but we additionally take
advantage of the fact that young objects tend to be discarded frequently while
old ones tend to stick around for a long time. This is taken advantage of by
generational collectors, which can treat newer objects differently from other
ones. The idea is to use each free space resulting from compaction phase as a
new generation, if it is at least a few kilobytes in size. The next GC will
therefore only process the new objects and not look at the rest of the heap
most of the time.



## Operating System Interface

Most calls to the underlying operating system happen via prim_sys() function. It
is called via sys-prim primop from lisp side. The function receives a system call
number to call and three arbitrary lisp values. prim_sys() is then expected to do its
thing and return one lisp value.

New calls can be added by placing them to the switch statement in ovm.c, recompiling the vm
with #{make bin/vm}, starting it with #{bin/vm fasl/init.fasl} and calling the newly added
primitive with something like #{(sys-prim 42 1 2 3)}.

Ideally we would like to just call the kernel just like done in assembly, but there are
no sufficiently portable ways to do this from C, so we use the standard library functions
and definitions.


## Virtual Machine

Most of evaluation time is spent in vm() function. It implements a simple virtual
register machine. It uses a traditional large switch statement for instruction dispatch,
which these days is reliably compiled to a jump table. Each bytecode has six bits
for the instruction and two bits for optional extra information.

There is also a separate dispatch loop for instructions generated by the C-compiler.
This is not needed when running vanilla bytecode, such as fasl/init.fasl.



