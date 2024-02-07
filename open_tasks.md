- Implicit return nil type is interesting, how to know if all code paths return?
  - requiring a return runs into the same issue
  - unless you require a return in the outer scope which is easy but dumb
- ternary typing is wrong
- array upvalue capturing is probably fucked but idk if we really want to capture stack arrays anyway
- pool opcodes long and short versions
- expressions in array initializers (vlas)
- Byte machine
  - Need to add support for variable stack instructions
  - then add structs, closures are just structs (right?)
  - how to represent upval as bytes? Does it go on the stack?

- A closure should be a function pointer, a pointer to upvalues on the heap, and num upvalues
  - you should only be able to capture const vars
- String concat is big fucked

- Make references respect mutablility
- upcalue reverences?

- identifiers/