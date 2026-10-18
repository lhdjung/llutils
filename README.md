
<!-- README.md is generated from README.Rmd. Please edit that file -->

# llutils

<!-- badges: start -->

<!-- badges: end -->

llutils provides ultra-fast replacements for common R utility functions.

It features `match()`, `%in%`, `duplicated()`, `unique()`, and
`findInterval()` equivalents, row matching on several key columns, regex
detection, counting, extraction, and replacement, and dictionary lookup,
on all your cores. It is written in Rust. `par_match()`, `%par_in%`,
`par_duplicated()`, `par_unique()`, and `par_find_interval()` behave
exactly like base R, so they are drop-in replacements. `par_detect()` is `grepl()` with the syntax of Rust’s
[regex crate](https://docs.rs/regex/latest/regex/#syntax), which never
backtracks.

The gains are largest where base R is slow and the work is bound by CPU:
matching text against a large dictionary of terms, and looking up
millions of keys.

## Dictionary matching

Which of 100,000 strings mention any of 1,000 terms? A backtracking
engine tries each term at each position. `par_detect()` compiles the
alternation once, into an automaton that finds all terms in one pass,
and splits the strings across threads.

``` r
library(llutils)
set.seed(1)

word <- function(n) {
  vapply(
    seq_len(n),
    function(i) paste(sample(letters, 8, TRUE), collapse = ""),
    ""
  )
}

vocabulary <- word(1e4)

x <- paste(
  sample(vocabulary, 1e5, TRUE),
  sample(vocabulary, 1e5, TRUE),
  sample(999, 1e5, TRUE)
)

terms <- sample(vocabulary, 1000)
pattern <- paste(terms, collapse = "|")

hits <- par_detect(x, pattern)
head(x[hits], 3)
#> [1] "ezwyrblt xhvokbru 627" "xwyayuxt ntapthgt 259" "xomhfess pvcbbxzc 906"
mean(hits)
#> [1] 0.18929

results <- bench::mark(
  base = grepl(pattern, x, perl = TRUE),
  stringi = stringi::stri_detect_regex(x, pattern),
  llutils = par_detect(x, pattern),
  iterations = 10
)
results[, c("expression", "median")]
#> # A tibble: 3 × 2
#>   expression   median
#>   <bch:expr> <bch:tm>
#> 1 base         15.02s
#> 2 stringi       8.45s
#> 3 llutils     19.39ms
plot(results)
```

<img src="https://raw.githubusercontent.com/lhdjung/llutils/figures/man/figures/README-dictionary-1.png" alt="Timings on a log scale: llutils takes milliseconds, stringi and base R take seconds." width="100%" />

Wrap the alternation in `\b(` and `)\b` to match whole words only, or
use `par_match_term(whole_word = TRUE)` below. Terms that contain regex
metacharacters such as `.` or `(` need a backslash in front of them.
Avoid `(?i)` in front of a large alternation: it defeats the
optimization above, and the same search takes over a hundred times
longer. For a case-insensitive search, use `par_match_term()` below, or
lowercase both the strings and the terms.

### Which term?

`par_match_term()` says which term each string contains, as a position
in `terms`: `match()` for substrings. If a string contains several
terms, the one earliest in `terms` wins, so put specific terms such as
`"new york"` before general ones such as `"york"`. The terms are
literal, so they need no escaping, and `ignore_case = TRUE` ignores
ASCII case at full speed. `whole_word = TRUE` skips hits inside longer
words, so `"york"` matches `"york, uk"` but not `"yorkshire"`.

``` r
found <- par_match_term(x, terms)
head(terms[found][!is.na(found)], 3)
#> [1] "xhvokbru" "ntapthgt" "xomhfess"

# The same result with stringi: one search per term
stringi_loop <- function(x, terms) {
  out <- rep(NA_integer_, length(x))
  for (i in rev(seq_along(terms))) {
    out[stringi::stri_detect_fixed(x, terms[i])] <- i
  }
  out
}

upper <- toupper(x)

results <- bench::mark(
  stringi = stringi_loop(x, terms),
  llutils = par_match_term(x, terms),
  llutils_ignore_case = par_match_term(upper, terms, ignore_case = TRUE),
  iterations = 3,
  filter_gc = FALSE
)
results[, c("expression", "median")]
#> # A tibble: 3 × 2
#>   expression            median
#>   <bch:expr>          <bch:tm>
#> 1 stringi                4.86s
#> 2 llutils               2.75ms
#> 3 llutils_ignore_case   2.81ms
plot(results)
```

<img src="https://raw.githubusercontent.com/lhdjung/llutils/figures/man/figures/README-term-1.png" alt="Timings on a log scale: llutils takes milliseconds, a stringi loop over the terms takes seconds." width="100%" />

To count hits instead, `par_count_term()` gives the number of term
occurrences in each string, as for scoring text against a sentiment
lexicon, and `par_which_term()` gives every term each string contains,
for tagging. `par_count()` counts regex matches per string.

## General matching

``` r
keys <- sample.int(1e8, size = 1e6)
x <- sample.int(1e8, size = 1e7, replace = TRUE)

results <- bench::mark(
  base = match(x, keys),
  collapse = collapse::fmatch(x, keys),
  llutils = par_match(x, keys),
  iterations = 10,
  filter_gc = FALSE
)
results[, c("expression", "median")]
#> # A tibble: 3 × 2
#>   expression   median
#>   <bch:expr> <bch:tm>
#> 1 base        255.7ms
#> 2 collapse    178.2ms
#> 3 llutils      14.1ms
plot(results)
```

<img src="https://raw.githubusercontent.com/lhdjung/llutils/figures/man/figures/README-match-1.png" alt="Timings on a log scale: llutils is about ten times faster than collapse and base R." width="100%" />

`par_match()` handles integer, double, character, Date, and POSIXct
vectors, and factors by their levels. Other classes, and inputs below
10,000 elements, are handled by base `match()`. Downstream, `%par_in%`
replaces `%in%`, and `par_in()` is the same as a regular function.

Both sides are hashed in parallel, so a join-like lookup of 10 million
keys into 10 million others takes 0.1 seconds for integers and 0.2 for
strings. The numbers behind the comparison with collapse are in
`bench/`.

`par_duplicated()` is `duplicated()` on the same hash tables, built in
parallel, and compares the rows of data frames as base does;
`par_unique()` is `unique()` on top of it.
`par_match_rows()` matches rows on several key columns at once, as in a
join: it hashes whole rows in parallel, so a lookup of 10 million rows
into 10 million others takes a fraction of a second, where pasting the
columns into strings for `match()` takes seconds per million.

`par_find_interval()` is `findInterval()` with a binary search per
element on all cores, for binning values by sorted breaks: 10 million
values into 10,000 breaks take a few dozen milliseconds, where base
takes almost a second.

## Use cases

The [use-cases
article](https://github.com/lhdjung/llutils/blob/main/vignettes/articles/use-cases.Rmd)
shows each function at work: coding free text with a keyword table,
scoring text against a lexicon, filtering logs, validating identifiers,
extracting and rewriting parts of strings, lookups and joins on one or
several keys, finding duplicates, and binning.

## Threads

The default is all cores, or the `RAYON_NUM_THREADS` environment
variable. Every function takes `nthreads`, and
`options(llutils.threads = 2)` sets the default for a session. Inputs
too small to keep that many threads busy use fewer.

## Regex syntax

Most patterns mean the same in Rust’s regex crate and in PCRE. The
differences:

- No backreferences or lookaround. In return, matching time grows only
  linearly with the string.
- `\d`, `\w`, and `\s` match Unicode by default. Write `[0-9]` or
  `(?-u:\d)` for ASCII.
- Flags go in the pattern: `(?i)` ignores case.
- `NA` gives `NA`, where `grepl()` gives `FALSE`.

`par_extract()` gives the first match in each string, or a capture group
of it, which takes the place of lookaround. `par_replace()` replaces
every match, as `gsub()` does, with `$1` instead of `\\1` for a group.
Only R’s own thread can make R strings, so the more strings change, the
more of the time that takes: where a tenth of a million strings change,
it is about 8 times faster than `gsub(perl = TRUE)`, and where all of
them do, about twice as fast.

## Installation

When llutils is on CRAN, you will be able to install it as usual:

``` r
install.packages("llutils")
```

The development version requires the [Rust
toolchain](https://rust-lang.org/tools/install/). When Rust is
installed, run:

``` r
pak::pak("lhdjung/llutils")
# or devtools::install_github("lhdjung/llutils")
```
