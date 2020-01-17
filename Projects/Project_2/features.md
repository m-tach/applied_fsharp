## Functions and local declarations
 * Block can now contain local declarations
    * Parsing implemented
    * Type checking implemented
    * Codegen implemented
    * Supports multiple declarations
    * Supports all types
 * Functions are now implemented
    * Parsing implemented
    * Type checking implemented
        * Checks that all returns are of the same type that the function expects
        * --Checks that everything returns as the last thing within a function 
    * Codegen implemented
    * Supports recursion
    * --Supports local functions
 * Return is now implemented
    * Checks that there is no returns outside of a function

## Procedures
 * Parsing implemented
 * Type checking implemented
 * Codegen implemented
 * Supports both implicit and explicit return statements

## Operators
 * Added support for `E * E`, `E % E`, `E / E`, `E = E`, `E < E`, `E > E`, `E <= E`, `E >= E`, `E <> E`, `E - E`, `!E`

## Pointers
 * Address-of operator &
    * Extended Lexer and Parser syntax (&x is Addr, has type PTyp)
    * Type checking implemented (PTyp with the basic type)
    * Codegen implemented
    * Uses the same fundamentals as Access, so it works for all cases where Access works (variables, arrays, pointer derefs)
 * Pointer dereferencing ^
    * Extended Lexer and Parser syntax
    * Type checking implemented (PTyp with the basic type)
           * Since we're using PTyp<Typ>, type checking for function/procedure arguments should also work properly
    * Codegen implemented
           * Array handling not yet added
    * [ ] Write code so you can't derenference a variable before setting it

## Arrays
 * [ ] Write explanation of features

## Strings
 * Chars
    * Char declaration type implemented and type-checked (CTyp)
    * Char value type implemented and type-checked (STR)
    * `PRINT` handles type-checking against int/bool and chars, and uses `PRINTC` if it's a char
    * Chars are defined using `'x'` (where x is a single symbol)
    * Assignment handles multi-char strings and single-char strings when assigning to char.
  * Strings
    * Support for char arrays, and setting the value of a char array to a literal string.
    * Can index individual characters of a char array by doing `arr[index]`.
    * `PRINT` handles char arrays and literal strings, as well as single characters.
    * Strings can be defined as either a char or using `"abcde"` (where abcde is a variable amount of characters. Not escaped!)
  * Finding basic types of expressions added; finds int/bool/char/arrays - not functions.

## Preincrement / Predecrement
  * Parsing / Lexing implemented
  * Type-checking only allows int types for this
  * Instructions are is ``<addr>; DUP; LDI; CSTI +-1; ADD; STI``
  * Tests added
  * Codegen added

## Ternary operators (conditional expressions)
  * Parsing / Lexing implemented - syntax in form of ``condition ? expr1 : expr2`` 
  * Type-checking checks that the condition gives a bool, and that expr1 and expr2 are of the same type.
  * Codegen added
  * Tests added
  * Instructions in form of ``<condition>; IFZERO L1; <expr1>; GOTO L2; Label L1; <expr2>; Label L2`` (L1 = false-case, L2 = end label)
  * Does not work well with arrays/strings, so be careful