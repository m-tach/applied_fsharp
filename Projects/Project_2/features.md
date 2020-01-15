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
    * Char declaration type implemented and type-checked
    * Char value type implemented and type-checked
    * PRINT handles type-checking against int/bool and chars, and uses PRINTC if it's a char
    * Chars are defined using 'x' (where x is a single symbol)