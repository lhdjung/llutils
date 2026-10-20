
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parvenu

<!-- badges: start -->

<!-- badges: end -->

parvenu provides ultra-fast parallel replacements for common R utility
functions.

It features equivalents of `match()`, `%in%`, `duplicated()`,
`unique()`, `findInterval()`, `as.Date()`, `as.POSIXct()`, `rank()`,
`rowsum()`, and `approx()`, row matching on several key columns, regex
detection, counting, extraction, and replacement, and dictionary lookup
and replacement, on all your cores. It is written in Rust.

`par_match()`, `%par_in%`, `par_duplicated()`, `par_unique()`,
`par_find_interval()`, and the functions under [Dates, ranks, and
sums](#dates-ranks-and-sums) behave exactly like base R, so they are
drop-in replacements. `par_detect()` is `grepl()` with the syntax of
Rust’s [regex crate](https://docs.rs/regex/latest/regex/#syntax), which
never backtracks.

The gains are largest where base R is slow and the work is bound by CPU:
matching text against a large dictionary of terms, and looking up
millions of keys.

Timed on 8 cores, against base R (the sections below and `bench/` have
the details, and comparisons with collapse, stringi, and data.table):

| Function | Replaces | Task | Time | Base R | Speedup |
|----|----|----|----|----|----|
| `par_match_term()` | `grepl(perl = TRUE)` on an alternation | 1,000 terms in 100,000 strings | 2.7ms | 15s | 5500x |
| `par_detect()` | `grepl(perl = TRUE)` | a regex in 1 million strings | 54ms | 1.1s | 21x |
| `par_match()` | `match()` | 10 million strings in 10,000 keys | 11ms | 0.25s | 23x |
| `par_match()` | `match()` | 10 million integers in 10,000 keys | 8ms | 0.12s | 15x |
| `par_match_rows()` | `match()` on pasted columns | 1 million rows on two key columns | 18ms | 2.5s | 134x |
| `par_duplicated()` | `duplicated()` | 10 million integers | 42ms | 0.49s | 12x |
| `par_unique()` | `unique()` | 10 million integers | 51ms | 0.57s | 11x |
| `par_find_interval()` | `findInterval()` | 10 million values into 10,000 breaks | 34ms | 0.92s | 27x |

## Dictionary matching

Which of 100,000 strings mention any of 1,000 terms? A backtracking
engine tries each term at each position. `par_detect()` compiles the
alternation once, into an automaton that finds all terms in one pass,
and splits the strings across threads.

``` r
library(parvenu)

texts <- c("flights to new york", "a walk in yorkshire", "rain in paris", "a quiet day")
par_detect(texts, "york|paris")
#> [1]  TRUE  TRUE  TRUE FALSE
```

Timed on 100,000 strings and 1,000 terms, against base R and stringi:

<details>

<summary>

Show code
</summary>

``` r
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
results <- bench::mark(
  base = grepl(pattern, x, perl = TRUE),
  stringi = stringi::stri_detect_regex(x, pattern),
  parvenu = par_detect(x, pattern),
  iterations = 10
)
results[, c("expression", "median")]
plot(results)
```

</details>

    #> # A tibble: 3 × 2
    #>   expression   median
    #>   <bch:expr> <bch:tm>
    #> 1 base         14.91s
    #> 2 stringi       8.35s
    #> 3 parvenu     18.32ms

<img src="https://raw.githubusercontent.com/lhdjung/parvenu/figures/man/figures/README-dictionary-1.png" alt="Timings on a log scale: parvenu takes milliseconds, stringi and base R take seconds." width="100%" />

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
literal, so they need no escaping, and `ignore_case = TRUE` ignores case
in any script, at full speed for ASCII text. `whole_word = TRUE` skips
hits inside longer words, so `"york"` matches `"york, uk"` but not
`"yorkshire"`, and `whole_word = "start"` matches prefixes, such as
`"happ"` in `"happiness"`.

``` r
places <- c("new york", "york", "paris")
par_match_term(texts, places)
#> [1]  1  2  3 NA
par_match_term(texts, places, whole_word = TRUE)
#> [1]  1 NA  3 NA
```

Timed on the same data, against a stringi search per term:

<details>

<summary>

Show code
</summary>

``` r
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
  `parvenu (default)` = par_match_term(x, terms),
  `parvenu (ignore case)` = par_match_term(upper, terms, ignore_case = TRUE),
  iterations = 10,
  filter_gc = FALSE
)
results[, c("expression", "median")]
plot(results)
```

</details>

    #> # A tibble: 3 × 2
    #>   expression              median
    #>   <bch:expr>            <bch:tm>
    #> 1 stringi                  4.96s
    #> 2 parvenu (default)       2.65ms
    #> 3 parvenu (ignore case)   2.58ms

<img src="https://raw.githubusercontent.com/lhdjung/parvenu/figures/man/figures/README-term-1.png" alt="Timings on a log scale: parvenu takes milliseconds, a stringi loop over the terms takes seconds." width="100%" />

To count hits instead, `par_count_term()` gives the number of term
occurrences in each string, or with `weights`, their summed scores, as
for scoring text against a sentiment lexicon. `par_which_term()` gives
every term each string contains, for tagging, and `par_replace_term()`
replaces each term with its replacement: 1,000 terms in 100,000 strings
take 21ms, where stringi’s
`stri_replace_all_fixed(vectorize_all = FALSE)` takes 3.7s. With
`fixed = FALSE`, the terms of all four `_term` functions are regexes,
all searched in one pass: slower than literal terms, but several times
faster than one `par_detect()` per term.

## General matching

<details>

<summary>

Show code
</summary>

``` r
keys <- sample.int(1e8, size = 1e6)
x <- sample.int(1e8, size = 1e7, replace = TRUE)

results <- bench::mark(
  base = match(x, keys),
  collapse = collapse::fmatch(x, keys),
  parvenu = par_match(x, keys),
  iterations = 10,
  filter_gc = FALSE
)
results[, c("expression", "median")]
plot(results)
```

</details>

    #> # A tibble: 3 × 2
    #>   expression   median
    #>   <bch:expr> <bch:tm>
    #> 1 base        253.1ms
    #> 2 collapse    181.7ms
    #> 3 parvenu      19.1ms

<img src="https://raw.githubusercontent.com/lhdjung/parvenu/figures/man/figures/README-match-1.png" alt="Timings on a log scale: parvenu is about ten times faster than collapse and base R." width="100%" />

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
`par_unique()` is `unique()`, collecting the first occurrences in the
same pass. `par_match_rows()` matches rows on several key columns at
once, as in a join: it hashes whole rows in parallel, so a lookup of 10
million rows into 10 million others takes a fraction of a second, where
pasting the columns into strings for `match()` takes seconds per
million.

`par_find_interval()` bins values by sorted breaks, such as ages into
age groups, with a binary search per element on all cores. It is
`findInterval()` under a clearer name, with the same results: 10 million
values into 10,000 breaks take a few dozen milliseconds, where base
takes almost a second.

## Dates, ranks, and sums

Some of base R’s slowest functions are not about matching at all. These
give the same results, to the last bit, and fall back to base R for what
they don’t cover. At a million dates or ten million numbers, on 8 cores:

| Function | Replaces | Task | Time | Base R | Speedup |
|----|----|----|----|----|----|
| `par_as_date()` | `as.Date()` | 1 million dates | 7ms | 4.6s | 657x |
| `par_as_posixct(tz = "UTC")` | `as.POSIXct(tz = "UTC")` | 1 million times | 12ms | 15s | 1250x |
| `par_rank()` | `rank()` | 10 million doubles | 0.24s | 9.8s | 41x |
| `par_rowsum()` | `rowsum()` | 10 million values in 100,000 groups | 0.1s | 1.2s | 12x |
| `par_approx()` | `approx()` | 10 million points into 10,000 | 0.1s | 0.63s | 6x |

`par_as_posixct()` parses in Rust only in UTC; other time zones go to
base R. `par_rank()` beats `data.table::frank()` (0.61s), while collapse
sums groups faster than `par_rowsum()` (67ms).

## Use cases

The [use-cases
article](https://github.com/lhdjung/parvenu/blob/main/vignettes/articles/use-cases.Rmd)
shows each function at work: coding free text with a keyword table,
scoring text against a lexicon, filtering logs, validating identifiers,
extracting and rewriting parts of strings, lookups and joins on one or
several keys, finding duplicates, and binning.

## Threads

The default is all cores, or the `RAYON_NUM_THREADS` environment
variable. Every function takes `nthreads`, and
`options(parvenu.threads = 2)` sets the default for a session. Inputs
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

`fixed = TRUE` finds `pattern` as it is, as in `grepl(fixed = TRUE)`.
`pattern` can also have one element per string, as in stringi.

`par_extract()` gives the first match in each string, or a capture group
of it, which takes the place of lookaround, and `par_extract_all()`
every match. `par_locate()` gives where the first match starts and ends,
as `regexpr()` does. `par_replace()` replaces the first match, as
`sub()` does, and `par_replace_all()` every match, as `gsub()` does,
with `$1` instead of `\\1` for a group. Only R’s own thread can make R
strings, so the more strings change, the more of the time that takes:
where a tenth of a million strings change, it is about 6 times faster
than `gsub(perl = TRUE)`, and where all of them do, about twice as fast.

## Installation

When parvenu is on CRAN, you will be able to install it as usual:

``` r
install.packages("parvenu")
```

The development version requires the [Rust
toolchain](https://rust-lang.org/tools/install/). When Rust is
installed, run:

``` r
pak::pak("lhdjung/parvenu")
# or devtools::install_github("lhdjung/parvenu")
```
