# llutils: plan

Multithreaded `match()`, `%in%`, `duplicated()`, `unique()`,
`findInterval()`, row matching, regex detection, counting, extraction, and
replacement, and dictionary lookup (`par_match_term()`, `par_count_term()`,
`par_which_term()`) in Rust. Open work
only. Started from exhaustive's Rust plan (`git show 1b2678c:PLAN.md` in
exhaustive).
Benchmarks are in `bench/`, run on 2026-10-17 with 8 cores. Measure after
`pkgbuild::compile_dll(debug = FALSE)` or an install: `rextendr::document()`
and `load_all()` build the Rust in debug mode, 5-20x slower.

Note: Much of this plan was written before seeting how incredibly fast
llutils functions got. Don't feel shackled by it! Revise it if needed.

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
- **A complete collection matters more than a wide margin everywhere.**
  For someone who already has collapse, llutils is a 2-11x speedup on
  lookups of 1e5 or more elements (Dates 4x, strings 7x, integers 11x), 2x
  on factors, `duplicated()`, and row matching, and close to a tie on a
  few cases, such as strings in `duplicated()`. Below 1e5 it beats base but not collapse.
  That's fine: a function belongs here if it gives base's results (or says
  how it differs), beats base clearly, and is about as fast as the fastest
  package or faster. Someone who needs a fast `duplicated()` or a lookup on
  two key columns shouldn't need another dependency for it, and the
  functions cover every common type. The old plan's phases 2 and 4 stay
  dropped because collapse runs them at memory speed, where threads add
  nothing (see Skipped). Regex is the strongest case: it is limited by
  CPU, so threads scale, and at 1e6 strings `par_detect()` takes 7-16ms
  against 53-247ms for `grepl(perl = TRUE)` and 221-378ms for stringi.
- **Dictionary lookup is the widest margin.** `par_match_term()` is
  `match()` for substrings: which of `terms` each string contains, earliest
  term winning.
  At 1e5 strings and 1000 terms it takes 3.5ms (2 threads: 9ms). The only
  exact R equivalent, a `stri_detect_fixed()` per term, takes 4.9s;
  extracting the leftmost hit with one alternation takes 15s (PCRE) to 29s
  (stringi), and AhoCorasickTrie 42s. It even beats `par_detect()` on the
  same alternation (25ms), which only says whether a term is there.
  `ignore_case = TRUE` keeps the speed (5.3ms), where `(?i)` sends
  `par_detect()` to 3.4s and PCRE to 15s. At 1e6 strings it takes 14ms for
  10 terms, 35ms for 1000, and 47ms for 1e4.
- **"Lightweight" is not the selling point, but holds.** No R package
  dependencies: extendr is compiled into the package, so `Imports` is empty
  (collapse imports Rcpp). CRAN's Windows and macOS binaries need no Rust.
  Building from source, the default on Linux unless users take binaries from
  Posit Package Manager or r2u, needs Cargo and rustc >= 1.83. The crates
  are vendored for CRAN, so the build needs no network.

## Next

Use cases for every function are collected in
`vignettes/articles/use-cases.Rmd`; add to it as new ones turn up.

1. **Experiment and review** what is here, then the release steps below.
   A first review on 2026-10-18 compared about 150 edge cases against base
   on the Rust path (NA/NaN, -0, encodings, factors, Dates, POSIXct, data
   frame columns, zero-length inputs) and found them identical. It fixed
   unbounded `nthreads`, inputs past 2^31 - 1 in `par_duplicated()` and
   `par_match_rows()`, and far Dates in `par_match_rows()`.

## Open (start each when there's a concrete use)

- `par_match_term()` with regex terms, through `regex::RegexSet`. Literal
  terms cover the dictionary case, and `ignore_case` (ASCII folding in
  aho-corasick, Unicode in regex) and `whole_word` would mean different
  things for regex terms, so this waits for a request.
- `par_unique()` in Rust, to catch collapse: collect each bucket's first
  occurrences while partitioning, instead of `x[!par_duplicated(x)]` (see
  Known limits).
- `sub()` (first match only) and all matches per string (a list, as
  `stri_extract_all_regex()`). Both fit what `par_replace()` and
  `par_which_term()` already do; wait for a use.

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
- `nthreads` is capped at the number of cores: each value keeps a pool,
  and uncapped values piled up threads until the system refused more.
- Thread pools are kept between calls, one per `nthreads` value. Work is
  split so each thread gets at least 1e4 elements (`par_detect()`: 2500
  strings), so small inputs use fewer than `nthreads`. A forked child gets
  fresh pools.
- Other classes (difftime, integer64, POSIXlt, subclasses of Date) go to
  base. So do tables longer than 2^31 - 1, Dates beyond 1e10 days (base
  matches those by their printed numbers), and string tables with non-ASCII
  text in mixed or latin1/bytes encodings.

`par_detect()`:

- Threads read string bytes through `R_CHAR()` and `LENGTH()` (and
  encodings through `Rf_getCharCE()`). Those only
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
- A factor with fewer levels than elements is matched by its levels, and
  the codes then index the result, as in `par_match()`: 50 levels in 1e7
  elements take 5ms, against 84ms as strings. `par_match_term()` does the
  same (11ms against 171ms).
- `pattern` can hold one pattern per string. The distinct ones are found
  by hashing their string pointers, serially, then compiled on the threads,
  and each chunk clones a regex the first time it meets it. With 10
  distinct patterns in 1e6 strings that takes 14ms (stringi 2.1s); with
  1e4, 76-92ms on 8 threads and 183ms on 2 (stringi 2.7s), though each of
  the 32 chunks clones most of the regexes. An earlier measurement of
  0.6-0.7s for 1e4 no longer holds. Sorting the strings by pattern first,
  so each regex builds its cache in about one chunk, was slower at every
  count from 10 to 1e4 distinct patterns (10: 34ms, 1e4: 110ms): the
  serial sort, gather, and scatter cost more than the clones. Sharing the
  regexes across threads took 410ms for 1e4 and doubled the time for 10,
  from contention on their caches.
- `par_count()` uses the same machinery and counts with `find_iter()`.
  Empty matches follow Rust, not `gregexpr()`, and the docs spell out
  the two differences: Rust counts one at the end of a non-empty string
  (`""` in `"abc"`: 4, `gregexpr()` 3) and skips one right after a
  non-empty match (`"a*"` in `"banana"`: 4, `gregexpr()` 6). stringi
  counts both (7). Copying `gregexpr()` isn't worth the code. At 1e6
  strings it takes 30ms against 232ms for `stri_count_regex()`.
- `par_extract()` and `par_replace()` find matches on the threads, and
  `par_replace()` builds the new strings there too, but only R's thread
  may make R strings (`Rf_mkCharLenCE()`, which hashes each into R's
  global string cache), so that part is serial. Making 1e6 strings takes
  about 0.4s on its own (`paste0(x, "_")`), which bounds both. Strings
  `par_replace()` leaves unchanged are reused, not made again, and so are
  matches that span the whole string in `par_extract()`. At 1e6 strings:
  extracting a match that 1 in 10 strings has takes 32ms (PCRE 380ms,
  stringi 520ms); extracting a group from every string, 0.2s (stringi
  4.8s, PCRE `regexec()` 16s); replacing in the 11% of strings that
  match, 55ms (PCRE 445ms, stringi 1.5s); replacing in all of them,
  0.23-0.4s (PCRE 460ms); swapping two groups in every string, 450ms,
  tied with PCRE (stringi 1.3s). Where every string changes, threads
  barely help.
- Results are marked UTF-8, as R passes `x` through `enc2utf8()`, except
  those made from bytes-encoded strings. `par_extract()` gives `NA` where
  nothing matches, as stringi does, not a shorter vector, as `regmatches()`
  does. A `group` beyond the pattern's groups is an error.
- The `regex` crate and its 4 dependencies add 5 of the package's 25
  crates, which adds to the vendored tarball.

`par_match_term()`:

- One `aho-corasick` automaton for all terms, shared by the threads: unlike
  a regex it has no cache to contend for. The crate came with `regex`, so
  it adds no crates.
- It reports every overlapping hit and keeps the lowest term index,
  stopping early only on the first term. So a string is always scanned to
  its end, unlike `par_detect()`, which can stop at the first hit.
- The last automaton is kept, as `regex()` keeps the last regex. Building
  one takes about 0.9ms for 1000 terms, so a repeat call on 100 strings
  takes 28us instead of 0.9ms (1e4 strings: 0.5ms instead of 1.5ms). The
  key is `terms`'s string pointers and `ignore_case`; `whole_word` filters
  hits afterwards, so it needs no automaton of its own. The cache holds a
  copy of the `terms` vector, which keeps its strings alive: otherwise R
  could free one and put a different string at its address. The R calls
  run before the cache's lock is taken, since an R error would skip its
  release and hang the next call.
- `par_count_term()` counts every hit `par_match_term()` loops over,
  overlapping ones included, so a term listed twice counts twice. At 1e5
  strings and 1000 terms it takes 3.1ms, against 4.9s for a
  `stri_count_fixed()` per term.
- `par_which_term()` collects each string's hits on the threads, sorted
  and deduplicated, into a `Vec` per string, then makes one integer vector
  per string on R's thread. At 1e5 strings of four words and 1000 terms it
  takes 7.6ms (2 threads: 20ms), against 7.9s for a `stri_detect_fixed()`
  per term and 61s for `stri_extract_all_regex()` on the alternation. At
  1e6 strings it takes 186ms, where `par_match_term()` takes 46ms: most of
  the difference is making the 1e6 vectors.
- `ignore_case` folds ASCII letters only, which is what aho-corasick offers.
  Lowercasing both sides with `tolower()` covers the rest.
- `whole_word` drops a hit if the character right before or after it is a
  letter or digit in any script, or `_` (Rust's `char::is_alphanumeric()`).
  That is PCRE's `(?<!\w)term(?!\w)`, not `\bterm\b`: the two differ only
  for terms that start or end with punctuation, where `"c++"` should not
  need a word character after it. It costs nothing measurable: 1e5
  strings and 1000 terms take 4.2ms, where a `\b` regex per term in
  stringi gives the same result in 15s, and PCRE takes 1.8s only to
  detect.

`par_duplicated()`:

- The table is split by hash into runs as in `par_match()`, and each
  bucket marks the duplicates among its keys as it fills its map, writing
  into the result through a shared pointer: each position is in one bucket
  only. So no second pass over `x` looks the elements up, as
  `par_match(x, x) != seq_along(x)` would. 1e7 elements with 1e6 distinct
  take 42ms for integers (collapse 85ms, data.table 165ms, base 450ms) and
  47-55ms for doubles (collapse 182-188ms). Strings take 112ms on a quiet
  machine (collapse 141ms), but 275ms (collapse 150ms) with the desktop
  busy: the scan for non-ASCII text, a cache miss per string, suffers most
  from busy cores. Rows of an integer, a double, and a string column take
  170-330ms (collapse 370-480ms).
- Dates and POSIXct compare by their numbers, as in base, not by printed
  day as in `match()`. A one-column data frame goes to the vector path, as
  base does; for more columns, rows are hashed as in `par_match_rows()`.
  Any class outside factor, Date, and POSIXct goes to base, since it may
  have its own `duplicated()` method; so do matrices.
- `par_unique()` is `x[!par_duplicated(x)]`, without names, as
  `unique()` returns it; data frames take rows as `unique.data.frame()`
  does, and matrices and other classes go to base. At 1e7 elements with
  1e6 distinct it takes 66ms for integers, 74ms for doubles, and 189ms
  for strings (base 0.44s, 0.70s, 1.7s; collapse `funique()` 35ms,
  163ms, 99ms): collapse wins for integers and strings by about 2x, where
  the subset after `par_duplicated()` is a third of the time.
  `sum(!par_duplicated(x))` counts distinct integers in 63ms (collapse
  `fnunique()` 105ms, data.table `uniqueN()` 184ms).
- Inputs longer than 2^31 - 1 go to base: positions are 32-bit.

`par_match_rows()`:

- Rows are keyed by their index, with a hash computed once from every
  column; two rows are equal if their hashes and then every column are.
  That check reads both rows' columns at random positions on every hit, so
  with many repeated rows it costs most of the time. For one or two
  columns the keys are stored in the hash table instead, in the same 24
  bytes a row reference takes: with many repeated rows that took the time
  from about 410ms to 245ms. 1e7 rows of two integer columns into 1e7 take
  160ms with few repeated rows and 245ms with many (collapse 294ms and
  424ms, vctrs 1-1.5s, data.table 1.9-3.4s). With a third, string column:
  180ms, collapse 450ms. Pasting the columns for `match()` takes 2.5s per
  1e6 rows, where this takes 18ms.
- Columns pair by position. Integer, double, string, Date, and POSIXct
  pairs, and factors (as strings), are hashed as they are; any other pair
  is first turned into positions of first occurrences in `table` by two
  `par_match()` calls, which keeps `match()`'s semantics for every type.
- Row indices are a `u32` vector as long as the longer side, 40MB for 1e7
  rows, so `par_match()` can take rows as slices. More than 2^31 - 1 rows
  is an error.
- A pair of Date columns with a value beyond 1e10 days is matched as
  positions from `par_match()`, as base matches those by printed number.

`par_find_interval()`:

- A binary search (`partition_point()`) per element, in parallel. 1e7
  unsorted values take 18ms into 100 breaks, 34ms into 1e4, and 96ms into
  1e6, where base takes 0.4s, 0.9s, and 2.9s, and a data.table rolling join
  about 2s. For sorted `x` base is already fast, since each search starts
  from the last (1e4 breaks: base 25ms, llutils 28ms).
- `vec` is checked in R, serially, for `NA` and order, and anything Rust
  doesn't take goes to base, which gives base's errors.

Versions:

- Rust 1.83 or later: `f64::from_bits` in a `const`, `div_ceil`, and the
  locked rayon need it. R 4.3 or later: base matches Dates by printed day
  only since `mtfrm()` arrived there.

## Release 0.1.0 (later)

Not before experimenting with and reviewing what is here.

- Before each build for CRAN, `rextendr::vendor_crates()` writes
  `src/rust/vendor.tar.xz` (1.7MB, ignored by git), so the build needs no
  network. With it, `R CMD check --as-cran` gives only the NOTE on the
  development version number.
- CI (Linux, macOS, Windows, and R 4.3 with Rust 1.83) is in place; check
  that it passes once pushed, then set the version to 0.1.0 and submit.

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
- Fuzzy matching: `stringdist::amatch()` already runs on several threads
  (OpenMP).
- Threaded `tolower()`, `nchar()`, `startsWith()`: bound by memory, or by
  building strings on R's thread, so base is close enough.
