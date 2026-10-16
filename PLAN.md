# llutils: plan

Multithreaded `match()` and `%in%` in Rust. Open work only. Started from
exhaustive's Rust plan (`git show 1b2678c:PLAN.md` in exhaustive).
Benchmarks are in `bench/`, run on 2026-10-16 with 8 cores.

## Positioning

- **The niche is threads plus results identical to base.** collapse's
  `fmatch()` is single-threaded, and threads are the whole gap: at 1e7
  lookups `par_match()` takes 11ms for integers and 22ms for strings, where
  collapse takes 88ms and 79ms. Being identical to base makes `par_match()`
  a safe drop-in for `match()`, so the tests compare against base, not
  against a spec.
- **Overlap with collapse only where llutils adds threads.** Copying
  collapse's single-threaded functions gains nothing: anyone who wants those
  uses collapse. Phases 2 and 4 of the old plan were dropped for that reason
  (see Skipped).
- **Be realistic about the audience.** For someone who already has
  collapse, llutils is a 2-8x speedup on lookups of 1e6 or more elements
  (Dates 2x, strings 4x, integers 8x), and a tie on factors.
  That's narrow, so its functions must cover every common type, and each
  new function must win by a similar margin against the fastest existing
  package, not just against base.
- **"Lightweight" is not the selling point, but holds.** No R package
  dependencies: extendr is compiled into the package, so `Imports` is empty
  (collapse imports Rcpp). CRAN's Windows and macOS binaries need no Rust.
  Building from source, the default on Linux unless users take binaries from
  Posit Package Manager or r2u, needs Cargo and rustc >= 1.65. The crates
  are vendored for CRAN, so the build needs no network.

## Release 0.1.0

- `rextendr::vendor_pkgs()`, so the build doesn't download crates. It is the
  only warning `R CMD check` gives today.
- CI as in exhaustive, then `R CMD check --as-cran` and submit.

## Next function: parallel `grepl()` (prototype, not started)

`bench/regex-prototype.R`, 1e6 strings: 21-33ms with Rust's `regex` crate
on 8 threads, against 53-246ms for `grepl(perl = TRUE)`, 264-986ms for
`grepl()`, and 225-381ms for stringi. The biggest win found so far,
because regex is limited by CPU, not memory.

Before building it:

- Its results can't be identical to base: Rust's regex syntax lacks
  backreferences and lookaround, and differs from PCRE and TRE in details.
  Name it for what it is, such as `detect_regex()`, and document the syntax
  as Rust's, rather than claim to replace `grepl()`.
- `NA`, and strings in latin1 or bytes encodings, which the prototype
  ignores. Translating them to UTF-8 is serial and needs R, so fall back or
  translate up front.
- A size threshold, as for `par_match()`.

## Open (start each when there's a concrete use)

- Matching on several columns at once, as in a join's key lookup. Base
  pastes the columns into strings, and `vctrs::vec_match()` handles data
  frames but is single-threaded.
- Parallel `unique()` and distinct counts. `fnunique()` takes 102ms at 1e7
  single-threaded, but a hash table shared across threads is real work, so
  this goes last.
- `match()`'s `incomparables` argument: not supported, rarely used.

## Known limits of `par_match()`

- FxHash (`rustc-hash`) is 2-3x faster than Rust's default hasher here, but
  doesn't resist inputs crafted to collide. Such inputs only slow it down.
- Other classes (difftime, integer64, POSIXlt, subclasses of Date) go to
  base. So do tables longer than 2^31 - 1, Dates beyond 1e10 days (base
  matches those by their printed numbers), and string tables with non-ASCII
  text in mixed or latin1/bytes encodings.
- Integer against double copies the integer side to double first.

## Skipped

- Phase 2 predicates: collapse already allocates nothing and runs at memory
  speed. `anyv()` takes 4.6ms (base `any(x == v)` 32ms), `allv()` stops at
  the first miss, `whichv()` takes 4.6ms (base 23ms), and between-checks via
  `frange()` take 18ms, as fast as `data.table::between()`.
- Phase 4 summaries: `frange()` takes 18ms (base `range()` 40ms), and
  `fnunique()` takes 102ms (data.table 155ms, base 376ms).
- A short-circuiting `any_in()`: `any(x %par_in% table)` covers it.
- Sorting and tabulating: data.table's sorting is already multithreaded,
  and collapse covers counting.
