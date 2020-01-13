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

## Operators
 * Added support for `e - e`, `!e`