# fancycut

A fancy version of `base::cut()`.

`cut()` forces every interval in a vector to use the same open/closed convention. `fancycut` lifts that restriction: you can mix left-open and right-open intervals, intervals closed on both ends, intervals open on both ends, and even single-point buckets, all in the same call. The range of the input doesn't have to be fully covered by your intervals either — unmatched values (and `NA`s) can be routed to their own buckets.

## Installation

```r
install.packages("fancycut")

# or the development version
devtools::install_github("adamleerich/fancycut")
```

## Usage

```r
library(fancycut)
```

There are two functions: `fancycut()` and `wafflecut()`. They do the same thing — `fancycut()` just uses `tag = value` syntax for convenience, while `wafflecut()` takes `intervals` and `buckets` as separate vectors (handy when they're already stored as vectors, e.g. from a lookup table).

### `fancycut()`

Intervals are given as `tag = "interval"` pairs, where a bare number is treated as a single point.

```r
fancycut(
  x = -10:10,
  Zero = 0,
  Small = '[0,2)',
  Medium = '[2,5]',
  Large = '(5,10]'
)
```

```r
# Basic low/high split
x <- seq.int(0, 1, 0.25)
fancycut(x, low = '[0, 0.5]', high = '(0.5, 1]')

# Not all values have to fall in a bucket
x <- seq.int(0, 1, 0.25)
fancycut(x, low = '(0.2, 0.3]', high = '(0.7, 0.8)')

# Route unmatched values with unmatched.bucket
x <- seq.int(0, 1, 0.25)
fancycut(x, low = '(0.2, 0.3]', high = '(0.7, 0.8)', unmatched.bucket = 'other')

# A point value: make the lower and upper bound equal
x <- seq.int(0, 1, 0.25)
fancycut(x, low = '[0, 0.5)', half = '[0.5,0.5]', high = '(0.5, 1]')

# Route NA values with na.bucket
x2 <- c(seq.int(0, 1, 0.25), NA)
fancycut(x2, low = '[0, 0.5)', high = '[0.5, 1]', na.bucket = 'missing')
```

### `wafflecut()`

Same behavior as `fancycut()`, but `intervals` and `buckets` are passed as parallel character vectors instead of named arguments.

```r
wafflecut(-10:10, c('[0,2)', '[2,5)', '[5,10]'), c('Small', 'Medium', 'Large'))

wafflecut(-10:10, c('[0,0]', '(0,2]', '(2,5)', '[5,10]'), c('Zero', 'Small', 'Medium', 'Large'))
```

```r
# Not all values have to fall in a bucket
x <- seq.int(0, 1, 0.25)
wafflecut(x, c('(0.2, 0.3)', '(0.7, 0.8)'), c('low', 'high'))

# Route unmatched values with unmatched.bucket
x <- seq.int(0, 1, 0.25)
wafflecut(x, c('(0.2, 0.3)', '(0.7, 0.8)'), c('low', 'high'), unmatched.bucket = 'other')

# Route NA values with na.bucket
x2 <- c(seq.int(0, 1, 0.25), NA)
wafflecut(x2, c('[0, 0.5)', '[0.5, 1]'), c('low', 'high'), na.bucket = 'missing')
```

### Arguments

| Argument | Description |
|---|---|
| `x` | a numeric vector to cut |
| `...` (`fancycut` only) | `tag = value` pairs — tags become bucket names, values are the interval definitions |
| `intervals` (`wafflecut` only) | a character vector of intervals |
| `buckets` (`wafflecut` only) | a character vector of level names, matched 1-to-1 with `intervals` |
| `na.bucket` | the level assigned to `NA` values in `x` (default `NA`) |
| `unmatched.bucket` | the level assigned to values in `x` not covered by any interval (default `NA`) |
| `out.as.factor` | if `TRUE` (default), returns a factor; if `FALSE`, returns a character vector |

### Interval syntax

Intervals are written the same way you'd write them in interval notation: `[` / `]` for closed (inclusive) ends, `(` / `)` for open (exclusive) ends — for example `'[0, 0.5)'` or `'(5, 10]'`. A bare number (e.g. `0`) is treated as a single-point bucket matching that exact value.

## Origin

Written for a project that needed to cut a numeric vector into buckets, but where the interval boundaries weren't uniformly open or closed — some were even single points. Kept as its own package for CRAN. Interval-parsing improvements contributed by Richie Cotton.

## Authors

- Adam Rich — author, maintainer
- Richie Cotton — contributor
- Claude helped with this README file

## License

CC0

