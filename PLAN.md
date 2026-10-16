# llutils: plan

Multithreaded `match()`, `%in%`, and regex detection in Rust. Open work
only. Started from exhaustive's Rust plan (`git show 1b2678c:PLAN.md` in
exhaustive).
Benchmarks are in `bench/`, run on 2026-10-16 with 8 cores.

## Positioning

- **The niche is threads, plus results identical to base where possible.**
  collapse's `fmatch()` is single-threaded, and threads are the whole gap: at 1e7
  lookups `par_match()` takes 11ms for integers and 22ms for strings, where
  collapse takes 88ms and 79ms. Being identical to base makes `par_match()`
  a safe drop-in for `match()`, so the tests compare against base, not
  against a spec. `detect_regex()` can't be identical, since its syntax is
  Rust's, so it carries its own name instead of claiming to be `grepl()`.
- **Overlap with collapse only where llutils adds threads.** Copying
  collapse's single-threaded functions gains nothing: anyone who wants those
  uses collapse. Phases 2 and 4 of the old plan were dropped for that reason
  (see Skipped).
- **Be realistic about the audience.** For someone who already has
  collapse, llutils is a 2-8x speedup on lookups of 1e6 or more elements
  (Dates 2x, strings 4x, integers 8x), and a tie on factors. That's narrow,
  so its functions must cover every common type, and each new function
  must win by a similar margin against the fastest existing package, not
  just against base. Regex is the stronger case: it is limited by CPU, so
  threads scale, and at 1e6 strings `detect_regex()` takes 7-15ms against
  54-249ms for `grepl(perl = TRUE)` and 226-382ms for stringi.
- **"Lightweight" is not the selling point, but holds.** No R package
  dependencies: extendr is compiled into the package, so `Imports` is empty
  (collapse imports Rcpp). CRAN's Windows and macOS binaries need no Rust.
  Building from source, the default on Linux unless users take binaries from
  Posit Package Manager or r2u, needs Cargo and rustc >= 1.65. The crates
  are vendored for CRAN, so the build needs no network.

## Open (start each when there's a concrete use)

- Matching on several columns at once, as in a join's key lookup. Base
  pastes the columns into strings, and `vctrs::vec_match()` handles data
  frames but is single-threaded.
- Parallel `unique()` and distinct counts. `fnunique()` takes 102ms at 1e7
  single-threaded, but a hash table shared across threads is real work, so
  this goes last.
- `match()`'s `incomparables` argument: not supported, rarely used.
- `detect_regex()` siblings: literal (fixed) search, a vectorised
  `pattern`, extracting or replacing matches. Literal search is covered for
  now by escaping the pattern.

## Known limits

`par_match()`:

- FxHash (`rustc-hash`) is 2-3x faster than Rust's default hasher here, but
  doesn't resist inputs crafted to collide. Such inputs only slow it down.
- Other classes (difftime, integer64, POSIXlt, subclasses of Date) go to
  base. So do tables longer than 2^31 - 1, Dates beyond 1e10 days (base
  matches those by their printed numbers), and string tables with non-ASCII
  text in mixed or latin1/bytes encodings.

`detect_regex()`:

- Threads read string bytes through `R_CHAR()` and `LENGTH()`. Those only
  read memory, but R doesn't promise they are thread-safe; data.table relies
  on the same. Reading on R's thread instead made it 2-3x slower, because
  strings are scattered in memory.
- Each chunk clones the regex: threads sharing one contend for its cache.
- Compiling the regex costs about 70us, so below a few hundred strings it is
  slower than `grepl(perl = TRUE)` (100 strings: 102us against 32us).
  Below 5000 strings it runs on one thread.
- `enc2utf8()` runs serially first, about 4ms per 1e6 strings, and is most
  of the remaining time for fast patterns. bytes-encoded strings are matched
  byte by byte.
- The `regex` crate and its 4 dependencies add 5 of the package's 28
  crates, which adds to the vendored tarball.

## Release 0.1.0 (later)

Not before experimenting with and reviewing what is here.

- `rextendr::vendor_pkgs()`, so the build doesn't download crates. It is the
  only warning `R CMD check` gives today.
- CI as in exhaustive, then `R CMD check --as-cran` and submit.

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
