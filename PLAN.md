# llutils: plan

Multithreaded `match()`, `%in%`, regex detection, and dictionary lookup
(`par_match_term()`) in Rust. Open work
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
`vignettes/articles/use-cases.Rmd`; add to it as new ones turn up. In
order:

1. **README: point to the use-cases article, and mention `whole_word`** in
   the "Which term?" section. Do it with the next knit, which also needs a
   push of the `figures` branch (recipe in README.Rmd).
2. **`par_count()`**, the next `par_detect()` sibling. It returns integers,
   so the threads never build strings on R's thread, and it should scale
   like `par_detect()`. Take `pattern` (a regex) as `par_detect()` does;
   a `terms` version, counting dictionary hits per string, covers lexicon
   scoring (sentiment, topic word counts) and can reuse
   `par_match_term_rs()`'s loop over overlapping hits, `whole_word` and all.
3. **Keep the last automaton**, as `regex()` keeps the last regex, keyed by
   `terms`, `ignore_case`, and `whole_word`. Building takes about 0.9ms for
   1000 terms, most of a call below 1e4 strings, so repeated small calls
   (a Shiny app, a loop over groups) pay it every time. Comparing the
   `terms` vector costs a pass over it; compare the pointers of its CHARSXPs,
   as `par_match_chr()` does, not the bytes.
4. **A vectorised `pattern` for `par_detect()`**: one pattern per string,
   as in `stringi::stri_detect_regex()`. Compile each distinct pattern once
   (most vectors repeat a few), then match in parallel. Measure against
   stringi, which recycles the same way on one thread.
5. **`par_find_interval()`**, a threaded `findInterval()`: binary search
   of each value in sorted breaks, for binning and range joins. Each search
   is a few cache misses, as `par_match()`'s probes are, so threads should
   win by a similar margin. Base runs on one thread and collapse has no
   threaded version; measure against both, and against data.table's
   rolling joins, before committing to it.
6. **Matching on several columns at once**, as in a join's key lookup.
   Base pastes the columns into strings, and `vctrs::vec_match()` handles
   data frames but is single-threaded. data.table's joins are the real
   competitor, so measure against them, not against vctrs.

## Open (start each when there's a concrete use)

- All terms per string, not just the first: a list of integer vectors, for
  tagging (an abstract with every drug it names). Wait for a request; the
  first-term contract covers coding, which assigns one category.
- Parallel `unique()` and distinct counts. `fnunique()` takes 102ms at 1e7
  single-threaded, but a hash table shared across threads is real work, so
  this goes last.
- Extracting or replacing regex matches. Both build strings, which only
  R's thread can do, so measure how much of the regex speedup survives.
- `par_match_term()` with regex terms, through `regex::RegexSet`. Literal
  terms cover the dictionary case, so this waits for a request.

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
- A factor with fewer levels than elements is matched by its levels, and
  the codes then index the result, as in `par_match()`: 50 levels in 1e7
  elements take 5ms, against 84ms as strings. `par_match_term()` does the
  same (11ms against 171ms).
- The `regex` crate and its 4 dependencies add 5 of the package's 25
  crates, which adds to the vendored tarball.

`par_match_term()`:

- One `aho-corasick` automaton for all terms, shared by the threads: unlike
  a regex it has no cache to contend for. The crate came with `regex`, so
  it adds no crates.
- It reports every overlapping hit and keeps the lowest term index,
  stopping early only on the first term. So a string is always scanned to
  its end, unlike `par_detect()`, which can stop at the first hit.
- The automaton is built on every call, not kept like the last regex. For
  1000 terms that takes about 0.9ms, most of the time below 1e4 strings
  (100 strings: 0.9ms, 1e4: 1.5ms; stringi takes 27ms and 2.9s). Keep the
  last automaton, as `regex()` does, if small repeated calls turn up.
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

Versions:

- Rust 1.83 or later: `f64::from_bits` in a `const`, `div_ceil`, and the
  locked rayon need it. R 4.3 or later: base matches Dates by printed day
  only since `mtfrm()` arrived there.

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
- A `par_duplicated()` or `par_unique()` on `par_match()`'s hash table:
  `par_match(x, x) != seq_along(x)` already is `duplicated(x)`, and the
  use-cases article shows it. A lookup of 1e7 into 1e7 takes 98ms, about
  as long as `fnunique()` (102ms), so a dedicated function would win little.
- Fuzzy matching: `stringdist::amatch()` already runs on several threads
  (OpenMP).
- Threaded `tolower()`, `nchar()`, `startsWith()`: bound by memory, or by
  building strings on R's thread, so base is close enough.
