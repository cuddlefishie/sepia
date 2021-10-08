# Sepia language requirements


mathematical / calculator requirements:

- infinite precision decimal numbers
- units and unit aliases
- SI prefixes
- uncertainty tracking `3.4m ± 0.3` / `12 exact` / `4.65 approximation`
  - both `exact` and `approximation` can be builtin values as they should propagate properly
  - `exact` is the default
  - (optional) support for uncertainty percentages `100 Ohm ± 5%`
- significant figures tracking and a special REPL mode for default rounding based on sigfigs

- vectors/tuples `(1, 2, 3)`
- (optional) range values `[0.01 .. 1.0]`
- tables for reading in CSV files
- defining variables `name = value`
- one-line functions `f(x) = 2 x`
- conversion functions for units `185K as °C` or `185K to °C` or `convert 185K to °C`
- `in` as a division alias
- named arguments `mean(values, uncertainty: true)`
- option to print in scientific notation or not
- querying variables by unit


programming requirements:

- references for mutable values
  ```
  x = ref(12)
  x := !x + 2
  ```
- conditionals
- loops
- multi-line functions and statements
  ```
  function sum(xs) {
    if length(xs) = 0 {
      return 0
    }
    acc = ref(xs[0])
    for i in 1 to length(xs) {
        acc := !acc + xs[i]
    }
    return !acc
  }
  ```
- string literals and manipulation
- modules `import chem` / `import chem as c` / `c.avogadro` / `import "local_file.sepia"`
  - implemented as tables?


syntax requirements:

- semicolon inference (lines can only continue with indentation)
- juxtaposition multiplication
- (to be debated) grouping based on whitespace `1 / x+1` = `1 / (x + 1)`
- unicode powers/subscripts, sqrt, symbols(π, Δ etc)
- `()` and `[]` as grouping symbols