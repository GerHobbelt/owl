# Internals

Most of the system is implemented in Owl itself. The code responsible for implementing language 
features resides mainly in (owl eval ...) modules. These allow translating S-expressions down to 
bytecode for a really simple virtual machine. The bytecode can also be converted to C to make 
programs standalone and reduce some interpretive overhead.

