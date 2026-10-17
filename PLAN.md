# llutils: plan

Multithreaded `match()`, `%in%`, and regex detection in Rust. Open work
only. Started from exhaustive's Rust plan (`git show 1b2678c:PLAN.md` in
exhaustive).
Benchmarks are in `bench/`, run on 2026-10-17 with 8 cores. Measure after
`pkgbuild::compile_dll(debug = FALSE)` or an install: `rextendr::document()`
and `load_all()` build the Rust in debug mode, 5-20x slower.

## Positioning

- **The niche is threads, plus results identical to base where possible.**
  collapse's `fmatch()` is single-threaded, and threads are the whole gap: at 1e7
  lookups into 1e4 keys `par_match()` takes 8ms for integers and 11ms for
  strings, where collapse takes 88ms and 78ms. The table is hashed in
  parallel too, so a join-like lookup of 1e7 into 1e7 keys takes 98ms
  for integers (collapse 435ms) and 218ms for strings (collapse 430ms).
  Strings need a scan of one side for non-ASCII text, so on 2 threads
  they only tie with collapse (395ms). Being identical to base makes `par_match()`
  a safe drop-in for `match()`, so the tests compare against base, not
  against a spec. `par_detect()` can't be identical, since its syntax is
  Rust's, so it carries its own name instead of claiming to be `grepl()`.
- **Overlap with collapse only where llutils adds threads.** Copying
  collapse's single-threaded functions gains nothing: anyone who wants those
  uses collapse. Phases 2 and 4 of the old plan were dropped for that reason
  (see Skipped).
- **Be realistic about the audience.** For someone who already has
  collapse, llutils is a 2-11x speedup on lookups of 1e5 or more elements
  (Dates 4x, strings 7x, integers 11x), and 2x on factors; below 1e5 it
  beats base but not collapse. That's narrow,
  so its functions must cover every common type, and each new function
  must win by a similar margin against the fastest existing package, not
  just against base. Regex is the stronger case: it is limited by CPU, so
  threads scale, and at 1e6 strings `par_detect()` takes 7-16ms against
  53-247ms for `grepl(perl = TRUE)` and 221-378ms for stringi.
- **"Lightweight" is not the selling point, but holds.** No R package
  dependencies: extendr is compiled into the package, so `Imports` is empty
  (collapse imports Rcpp). CRAN's Windows and macOS binaries need no Rust.
  Building from source, the default on Linux unless users take binaries from
  Posit Package Manager or r2u, needs Cargo and rustc >= 1.83. The crates
  are vendored for CRAN, so the build needs no network.

## Open (start each when there's a concrete use)

- Matching on several columns at once, as in a join's key lookup. Base
  pastes the columns into strings, and `vctrs::vec_match()` handles data
  frames but is single-threaded.
- Parallel `unique()` and distinct counts. `fnunique()` takes 102ms at 1e7
  single-threaded, but a hash table shared across threads is real work, so
  this goes last.
- `par_detect()` siblings: a vectorised `pattern`, extracting or
  replacing matches.

## Known limits

`par_match()`:

- FxHash (`rustc-hash`) is 2-3x faster than Rust's default hasher here, but
  doesn't resist inputs crafted to collide. Such inputs only slow it down.
- Tables of 65536 or more elements are hashed in parallel: split by hash
  into up to 256 runs, one map each. The runs hold a key and position per
  element, 16 bytes, so a 1e7 table takes 160MB while it is built.
- Maps below 2^18 keys get twice the capacity they need: at hashbrown's
  7/8 load a miss probes several groups, 2-5x slower, and such maps fit in
  cache, so probes are the whole cost. Larger maps are bound by memory, so
  they get only enough room to stay under 2/3 full.
- An `x` at least 10x shorter than the table is hashed instead, and the
  table scanned in one chunk per thread, each holding a position per
  element of `x`. One element against 1e7 takes 3ms (base 8ms: it scans
  too, on one thread), where hashing the table took 30ms.
- String tables need a scan of the shorter side for non-ASCII text, one
  cache miss per string, which the hash build does not. At 2 threads that
  is most of the time for a large string table.
- Thread pools are kept between calls, one per `nthreads` value. Work is
  split so each thread gets at least 1e4 elements (`par_detect()`: 2500
  strings), so small inputs use fewer than `nthreads`. A forked child gets
  fresh pools.
- Other classes (difftime, integer64, POSIXlt, subclasses of Date) go to
  base. So do tables longer than 2^31 - 1, Dates beyond 1e10 days (base
  matches those by their printed numbers), and string tables with non-ASCII
  text in mixed or latin1/bytes encodings.

`par_detect()`:

- Threads read string bytes through `R_CHAR()` and `LENGTH()`. Those only
  read memory, but R doesn't promise they are thread-safe; data.table relies
  on the same. Reading on R's thread instead made it 2-3x slower, because
  strings are scattered in memory.
- Each chunk clones the regex: threads sharing one contend for its cache.
- Compiling the regex costs about 70us, so the first call on a pattern
  with fewer than a few hundred strings is slower than `grepl(perl = TRUE)`.
  The last compiled regex is kept, so repeats cost nothing (100 strings:
  17us against 32us). Below 5000 strings it runs on one thread.
- `enc2utf8()` runs serially first, about 4ms per 1e6 strings, and is most
  of the remaining time for fast patterns. bytes-encoded strings are matched
  byte by byte.
- The `regex` crate and its 4 dependencies add 5 of the package's 25
  crates, which adds to the vendored tarball.
- Rust 1.83 or later: `f64::from_bits` in a `const`, `div_ceil`, and the
  locked rayon need it. R 4.3 or later: base matches Dates by printed day only since
  `mtfrm()` arrived there.

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
