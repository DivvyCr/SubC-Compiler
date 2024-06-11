<h1 align="center">Compiling a Subset of C</h1>

<div align="center">

![Version](https://img.shields.io/badge/latest-v1.0-blue.svg)
![GitHub License](https://img.shields.io/github/license/DivvyCr/SubC-Compiler?color=blue)

</div>

This compiler follows the [LLVM Kaleidoscope Tutorial](https://llvm.org/docs/tutorial/) to construct a recursive descent parser for a subset of the C language, albeit with some minor modifications/extensions.
I have also referred to [this repository](https://github.com/MarkLeone/WeekendCompiler) to better understand the LLVM code generation process and the application of the visitor pattern (as suggested by the tutorial).

## Installation

 0. Install the prerequisites: `clang`, `llvm17`, and `llvm-devel`
   - I used the `dnf` package manager, so package names may differ on other platforms
   - Alternatively, see [getting started](https://llvm.org/docs/GettingStarted.html#getting-the-source-code-and-building-llvm) and [official downloads](https://releases.llvm.org/download.html)
 1. Clone this repository
 2. Run `make` in the cloned directory
 3. Run `./mccomp [FILE]`

## Language Definition

The full grammar is shown in the `grammar.txt` file.
It is in LL(1) form and it enforces operator precedence.

The gist of the language is as follows:
 - Literals and variables are either *integers*, *floats* (in decimal form), or *booleans* (`true` and `false`)
 - Variable declarations **must** occur at the start of a code block! (However, nested code blocks are supported, allowing circumvention of this requirement)
 - Supported operations are:
   - Logical `||` and `&&`, with proper precedence and short-circuit evaluation
   - Logical negation (`!`)
   - Basic arithmetic (`+`, `-`, `*`, `/`, `%`)
   - Basic comparison (`==`, `!=`, `>=`, `>`, `<=`, `<`)
 - Supported control flow:
   - `if` blocks (with optional `else` blocks)
   - `while` blocks
   - Early `return` statements
 - Functions may return `void`
 - Functions may have no parameters, declared as `f()` (ie. no need for explicit `f(void)`)
 - Functions can only be overloaded based on parameter count

## Example Program

```c
float cosine(float x) {
  float cos;
  float n;
  float term;
  float eps;
  float alt;

  n = 1.0;
  term = 1.0;
  eps = 0.000001;
  alt = -1.0;

  { // Using a nested code block to declare a new variable:
    float cos;
    cos = 1.0;

    while (term > eps) {
      term = term * x * x / n / (n+1);
      cos = cos + alt * term;
      alt = -alt;
      n = n + 2;
    }
    return cos;
  }
}
```
