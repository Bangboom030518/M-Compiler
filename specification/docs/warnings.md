The following will raise warnings during compilation.

1. Unused variable declarations (including those preceeded with `_`).
   > **Warning**: Unused variable `identifier`.
   > 
   > *Help*: Remove declaration of variable `identifier`.

1. Variables that are declared `var`, but whose values are never reassigned.
   > **Warning**: Variable `identifier` is declared with `var`, but is never reasigned.
   > 
   > *Help*: Replace the `var` keyword with `let`.

1. A function does not have side effects but doesn't use the `pure` keyword.
   > **Warning**: Function does not have any side-effects but doesn't have the `pure` keyword.
   >
   > *Help*: Add the `pure` keyword to the function.
1. String flags are not alphabetised.

   > **Warning**: Flags `rm` aren't in alphabetical order.
   >
   > *Help*: Change flags `rm` to `mr`.