# parvenu: plan

Multithreaded `match()`, `%in%`, `duplicated()`, `unique()`,
`findInterval()`, row matching, regex detection, counting, location,
extraction, and replacement, and dictionary lookup and replacement
(`par_match_term()`, `par_count_term()`, `par_which_term()`,
`par_replace_term()`) in Rust. Open work
only. Started from exhaustive's Rust plan (`git show 1b2678c:PLAN.md` in
exhaustive).
Benchmarks are in `bench/`, run on 2026-10-17 with 8 cores. Measure after
`pkgbuild::compile_dll(debug = FALSE)` or an install: `rextendr::document()`
and `load_all()` build the Rust in debug mode, 5-20x slower.

Note: Much of this plan was written before seeing how incredibly fast
parvenu functions got. Don't feel shackled by it! Revise it if needed.

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
  For someone who already has collapse, parvenu is a 2-11x speedup on
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

No release yet: the aim for now is a package in a good state, through
experimenting, reviewing, and settling the interface while renaming is
still free. The release steps at the end wait until then.

1. **Experiment and review** what is here.
   A first review on 2026-10-18 compared about 150 edge cases against base
   on the Rust path (NA/NaN, -0, encodings, factors, Dates, POSIXct, data
   frame columns, zero-length inputs) and found them identical. It fixed
   unbounded `nthreads`, inputs past 2^31 - 1 in `par_duplicated()` and
   `par_match_rows()`, and far Dates in `par_match_rows()`. A second review
   the same day, of `par_unique()`, `par_which_term()`, `par_extract()`,
   and `par_replace_all()`, compared about 90 more cases (bytes and mixed
   encodings, empty matches, groups, replacement syntax, factor and data
   frame classes). It fixed empty matches between the bytes of one
   character in `par_replace_all()` and `par_count()` (`""` in `"é"` gave
   invalid UTF-8), subclassed factors and `contrasts` in `par_unique()`,
   and names in `par_replace_all()`, which `gsub()` keeps.
   A third review on 2026-10-19, of `par_unique()` in Rust,
   `par_replace()`, `par_extract_all()`, and regex terms, compared about
   60 cases (thresholds at 1e4 and 65536 elements, one thread, attributes
   and classes, NaN payloads, mixed encodings, bytes, empty and optional
   groups, empty terms, 1e4 and 5e4 regex terms). `par_unique()` agreed
   with `unique()` everywhere. It fixed regex terms with unbalanced
   parentheses, which escaped the `whole_word` wrapper (`x)|(y` matched
   `x` anywhere instead of being an error), and `(?x)` terms ending in a
   comment, which swallowed it. `par_extract()` and `par_extract_all()`
   now keep names, as `regmatches()` does.
2. **Names of the string functions.** `par_replace()` became
   `par_replace_all()` on 2026-10-19: in stringr `str_replace()` replaces
   the first match only, and base `replace()` is unrelated, while `par_`
   plus a base name promises base's results. The verbs follow stringr
   (`detect`, `count`, `extract`, `replace_all`), which marks them as string
   operations without a prefix. `par_str_` was rejected, as it would
   promise stringr's results (ICU syntax differs from Rust's); revisit
   `par_regex_` if the regex functions grow to many more.

## Open

Add ideas here when a use turns up. Candidates beyond strings, measured on
2026-10-20 (8 cores, base R, one thread):

- **Parsing dates and times**: `as.Date()` takes 4.6s for 1e6 ISO strings,
  `as.POSIXct()` 15s. Bound by CPU, and the result is numbers, so no
  strings need making on R's thread. Formats beyond ISO 8601 and time zones
  other than UTC are the hard part.
- **`rank()`**: 12s for 1e7 doubles (`order()` 0.32s), where ties make it
  slow. `data.table::frank()` is fast, but a dependency.
- **`approx()`**: 0.63s for 1e7 points into 1e4; a binary search per point,
  as in `par_find_interval()`.
- **Factor pairs in `par_match_rows()`**: 191ms for 1e7 rows into 1e7 with
  two factor columns, where collapse takes 90ms, probably by indexing the
  codes directly instead of hashing them.

Measured and not worth it: `order()` and `sort()` (0.3-0.5s, and
data.table's `forder()` is no faster here), `as.numeric()` on strings (78ms
per 1e6), `dist()` (144ms for 5000 rows of 10).

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
  base. So do tables longer than 2^31 - 1, Dates beyond 1e10 days before
  R 4.6.0 (base matches those by their printed numbers), and string tables
  with non-ASCII
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
- Strings marked latin1 are converted to UTF-8 on the threads, as
  `enc2utf8()` converts them: R reads latin1 as Windows-1252, and writes
  the 5 bytes that has no character for as `<81>` and the like. R ran
  `enc2utf8()` first, which took 5-6ms per 1e6 strings on one thread even
  where it changed nothing, about half the time for fast patterns: at 1e6
  strings `par_detect()` went from 11ms to 5ms, `par_match_term()` from
  12ms to 7ms. R still runs it in a locale that isn't UTF-8, where native
  strings need converting. bytes-encoded strings are matched byte by byte.
- A factor with fewer levels than elements is matched by its levels, and
  the codes then index the result, as in `par_match()`: 50 levels in 1e7
  elements take 5ms, against 84ms as strings. `par_match_term()` does the
  same (11ms against 171ms).
- `pattern` can hold one pattern per string. The distinct ones are found
  by hashing their string pointers, serially, then compiled on the threads,
  and each chunk clones a regex the first time it meets it. Each clone
  builds its own cache, so with many distinct patterns most of the time
  went into caches: 1e4 patterns in 1e6 strings took 0.63s on 8 threads
  (stringi 2.7s), slower than 0.53s on 2, which have fewer chunks. From 256
  distinct patterns, the strings are now sorted by pattern first (a
  counting sort, then a parallel gather and scatter), so each chunk meets
  only a few: 1e4 patterns take 0.1s on 8 threads and 0.24s on 2. Sorting
  costs about 15ms per 1e6 strings, which doubles the time for 10 patterns
  (14ms to 29ms); the two tie near 300. An earlier measurement that found
  sorting slower at every count was wrong: its unsorted path gave every
  string the first pattern. Sharing the regexes across threads instead
  took 410ms for 1e4 and doubled the time for 10, from contention on their
  caches.
- `par_count()` uses the same machinery and counts with `find_iter()`.
  Empty matches follow Rust, not `gregexpr()`, and the docs spell out
  the two differences: Rust counts one at the end of a non-empty string
  (`""` in `"abc"`: 4, `gregexpr()` 3) and skips one right after a
  non-empty match (`"a*"` in `"banana"`: 4, `gregexpr()` 6). stringi
  counts both (7). Copying `gregexpr()` isn't worth the code. At 1e6
  strings it takes 30ms against 232ms for `stri_count_regex()`.
- `par_extract()` and `par_replace_all()` find matches on the threads, and
  `par_replace_all()` builds the new strings there too, but only R's thread
  may make R strings (`Rf_mkCharLenCE()`, which hashes each into R's
  global string cache), so that part is serial. Making 1e6 strings takes
  about 0.4s on its own (`paste0(x, "_")`), which bounds both. Strings
  `par_replace_all()` leaves unchanged are reused, not made again, and so are
  matches that span the whole string in `par_extract()`. At 1e6 strings:
  extracting a match that 1 in 10 strings has takes 28ms (PCRE 300ms,
  stringi 520ms); extracting a group from every string, 0.2s (stringi
  4.8s, PCRE `regexec()` 15s); replacing in the 11% of strings that
  match, 48ms (PCRE 345ms, stringi 1.1s); replacing in all of them,
  0.23-0.4s (PCRE 445ms); swapping two groups in every string, 0.45-0.6s,
  about tied with PCRE (460ms; stringi 1.2s). Where every string changes, threads
  barely help.
- `par_replace()` is `par_replace_all()` stopped after the first match.
  At 1e6 strings it takes 48ms where 11% of strings match (PCRE `sub()`
  336ms, stringi 1s) and 0.26-0.28s where every string changes (PCRE
  403ms).
- `par_extract_all()` finds the matches `par_count()` counts, so the
  lengths of its result are the counts, and gives `character(0)` where
  there is none, as `regmatches()` and stringr do (stringi gives `NA`).
  Each string's character vector is put into the list before its strings
  are made, which keeps it from the garbage collector without a
  `PROTECT()` per string. At 1e6 strings with 3 matches each it takes
  0.21s (2 threads: 0.24s), against 0.43s for `stri_extract_all_regex()`
  and 11s for `regmatches(x, gregexpr(perl = TRUE))`: making the 3e6
  strings is most of the time.
- Results are marked UTF-8, except those made from bytes-encoded strings.
  A latin1 string is converted again on R's thread when a result is made
  from it. `par_extract()` gives `NA` where
  nothing matches, as stringi does, not a shorter vector, as `regmatches()`
  does. A `group` beyond the pattern's groups is an error.
- The `regex` crate and its 4 dependencies add 5 of the package's 25
  crates, which adds to the vendored tarball.
- A pattern that matches single bytes, such as `(?-u:.)`, can cut a
  character in two, and `par_extract()` and `par_extract_all()` then give
  invalid UTF-8, as `useBytes = TRUE` can in base.

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
  takes 7.6ms (2 threads: 18ms), against 7.8s for a `stri_detect_fixed()`
  per term and 60s for `stri_extract_all_regex()` on the alternation. At
  1e6 strings it takes 120-190ms, where `par_match_term()` takes 45-70ms:
  most of the difference is making the 1e6 vectors.
- `ignore_case` folds ASCII letters in the automaton, which is what
  aho-corasick offers. Terms and strings with other characters are
  lowercased first (`char::to_lowercase()`, with final sigma as σ), each
  string on its thread into a buffer, with a map back to its own positions
  for `par_replace_term()`. Lowercase isn't quite case folding: `"ß"` and
  `"ss"` stay apart, as in `(?i)`, but `"İ"` becomes two characters. At
  1e5 strings and 1000 terms: 3ms for ASCII, 8ms where every string has a
  non-ASCII character, against 73ms for `tolower()` first.
- `par_count_term()` with `weights` adds a double per hit instead of 1, at
  the same speed (3ms at 1e5 strings and 1000 terms).
- `par_replace_term()` collects every hit, sorts them by start and then
  term, and replaces each that starts after the last one replaced ended,
  so the leftmost wins and, at the same start, the earliest term. Regex
  terms expand `$1` by running the term's regex again from the hit
  (`captures_at()`). Empty literal terms are an error: they would hit
  between every two characters. At 1e5 strings and 1000 terms it takes
  21ms, where `stri_replace_all_fixed(vectorize_all = FALSE)` takes 3.7s
  for the same result.
- `fixed = FALSE` takes regex terms, compiled into one `regex::RegexSet`,
  which says which terms match a string but not where or how often. So
  `par_count_term()` runs each matching term's own regex, counting as
  `par_count()` does (no overlaps). `ignore_case` there folds all of
  Unicode, as `(?i)` does, and `whole_word` wraps each term in
  `\b{start-half}(?:...)\b{end-half}`, which is `(?<!\w)...(?!\w)`: the
  same rule as for literal terms, but the regex finds a match that passes
  it, where the automaton drops hits that fail. Both differ from the
  literal case, which is why it is an option, not the default.
- Most of the time for many regex terms is building the set's lazy DFA,
  in a cache per thread. At the default 2MB cache 1000 terms in 1e6
  strings took 57s, refilling it again and again; at 16MB, 1.3s (64MB
  saves 15% more, at 4x the memory). Each pool thread gets its own clone
  of the set, kept with the terms, so its cache stays built between
  calls: a clone per chunk built it again in each chunk (2-3x slower for
  1000 terms), and one set shared by all threads contended for its caches
  (2x slower for 10). The cache is still built once per thread, so for
  1000 terms in 1e5 strings 2 threads take 0.37s and 8 take 0.7s (first
  call 0.95s), against 2.1s for a `par_detect()` per term and 14s for a
  `stri_detect_regex()` per term. For 10 and 100 terms it takes 5ms and
  15-19ms (`par_detect()` per term 11ms and 150ms, stringi 0.14s and
  1.6s); literal terms take 2-4ms at any count.
- The set clones keep their caches while the terms stay the same, up to
  16MB per core. More than about 1e4 regex terms exceed the regex
  crate's 10MB limit on a compiled regex, which is an error; 1e4 take
  0.3s to build.
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
  day as in `match()` before R 4.6.0. A one-column data frame goes to the vector path, as
  base does; for more columns, rows are hashed as in `par_match_rows()`.
  Any class outside factor, Date, and POSIXct goes to base, since it may
  have its own `duplicated()` method; so do matrices.
- `par_unique()` marks first occurrences in a byte per element, by the
  same buckets as `par_duplicated()`, then gathers the marked elements in
  parallel; it was `x[!par_duplicated(x)]`, where the subset in R was a
  third of the time. Sorting each bucket's first positions instead was
  slower where most elements are distinct (1e7 distinct integers: 190ms
  against 118ms for the subset). Names are dropped, as `unique()` drops
  them; data frames take rows as `unique.data.frame()` does, and matrices
  and other classes go to base. Plain data frames are rebuilt from their
  subset columns, since `[.data.frame` spent 60% of its time on
  `anyDuplicated()` of the new row names, which kept rows can't have: 1e7
  rows of three columns took 627ms, now 324ms (collapse `funique()` 532ms,
  data.table 1.07s). Subclasses keep `[`: a tibble's takes 120ms, about
  what the rebuild would. Gathering the columns in Rust would save about
  35ms. At 1e7 elements with 1e6 distinct it
  takes 34-51ms for integers, 57-61ms for doubles, and 144ms for strings
  (was 66ms, 74ms, 189ms; base 0.49s, 0.73s, 1.8s; collapse `funique()`
  36ms, 155ms, 111ms), measured on 2026-10-19 with the machine under load
  (load average 4-5). Strings make R's thread set each element of the
  result, and need the scan for non-ASCII text. `length(par_unique(x))`
  counts distinct integers in 32ms, faster than `sum(!par_duplicated(x))`
  (72ms; collapse `fnunique()` 113ms, data.table `uniqueN()` 136ms).
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
  pairs are hashed as they are. A pair of factors is hashed by codes, with
  `x`'s mapped to `table`'s levels (`index_codes()`): as strings, each
  column took a 54ms `as.character()`, and 1e7 rows of two factors into
  1e7 took 444ms, now 191ms (collapse 90ms). A factor paired with a string
  column is hashed as strings; any other pair
  is first turned into positions of first occurrences in `table` by two
  `par_match()` calls, which keeps `match()`'s semantics for every type.
- Row indices are a `u32` vector as long as the longer side, 40MB for 1e7
  rows, so `par_match()` can take rows as slices. More than 2^31 - 1 rows
  is an error.
- Before R 4.6.0, a pair of Date columns with a value beyond 1e10 days is
  matched as positions from `par_match()`, as base matches those by
  printed number.

`par_locate()`:

- `par_extract()`'s search, returning the first and last character of the
  match, counted from its bytes (UTF-8 continuation bytes don't count).
  Returns a matrix, as stringi's `stri_locate_first_regex()` does; a
  factor goes through `as.character()`, since a matrix can't be indexed by
  levels. At 1e6 strings: 18ms, against 80ms for `regexpr(perl = TRUE)`
  and 138ms for stringi. No `par_locate_all()` yet: it would make a matrix
  per string on R's thread.

`par_as_date()` and `par_as_posixct()`:

- A parser of `%Y`, `%m`, `%d`, `%H`, `%M`, `%S`, and `%OS`, written from
  R's behaviour, which fuzzing against base checks: numbers may follow
  spaces and take every digit their width allows, even past their range
  (day "84" is `NA`, not day 8); text after the format is ignored;
  `"24:00:00"` is midnight of the next day, even on February 30; `%OS`
  leaves the seconds at 0 where it reads no number from 0 to 61, and reads
  signs, exponents, and NaN as `R_strtod()` does. Formats with other
  specifiers, or without a year, month, and day, go to base.
- Fractional seconds are computed as `R_strtod()` does on doubles: digits
  accumulated, then divided by a power of 10. Where R is built with long
  doubles (x86-64), R divides in 80 bits and rounds twice, so the fraction
  can differ in its last bit; that shows only for times within seconds of
  1970. More than 15 digits, an exponent beyond 22, or hexadecimal go to
  base, where long doubles could round differently.
- `tryFormats` work as in base: for dates, the first format that reads the
  first string that isn't `NA` or empty; for times, the first that reads
  every string that isn't `NA`. Where none fits, base runs to give its
  error, or `NA`s with `optional = TRUE`.
- Times are parsed only in UTC (`"UTC"` or `"GMT"`): other zones need the
  time zone database, and go to base.
- At 1e6 strings: dates 7ms (base 4.6s, `data.table::as.IDate()` 4.6s, as
  it calls base), times 12ms (base 15s).

`par_rank()`:

- The elements that aren't `NA` are sorted with their positions, as one
  key, by rayon's parallel sort, which needs no stable sort since
  positions break ties. Runs of equal elements then get their ranks on the
  threads, each chunk starting where a run does. Doubles sort by their bits,
  with -0 as 0, which ties them as `rank()` does.
- 1e7 doubles take 0.24s (base 9.8s, `data.table::frank()` 0.61s), 1e7
  integers with many ties 0.16s (base 7.3s, `frank()` 0.22s).
- `"random"` goes to base, which draws from R's random numbers.

`par_find_interval()`:

- A binary search (`partition_point()`) per element, in parallel. 1e7
  unsorted values take 18ms into 100 breaks, 34ms into 1e4, and 96ms into
  1e6, where base takes 0.4s, 0.9s, and 2.9s, and a data.table rolling join
  about 2s. For sorted `x` base is already fast, since each search starts
  from the last (1e4 breaks: base 25ms, parvenu 28ms).
- `vec` is checked in R, serially, for `NA` and order, and anything Rust
  doesn't take goes to base, which gives base's errors.

Versions:

- Rust 1.83 or later: `f64::from_bits` in a `const`, `div_ceil`, and the
  locked rayon need it. R 4.3 or later: base matches Dates by printed day
  only since `mtfrm()` arrived there. R 4.6.0 added `mtfrm.Date()`, which
  matches them by number, so `par_match()` and `par_match_rows()` check
  the R version when called (`dates_by_day()`). `findInterval()` has
  `checkSorted` and `checkNA` only from R 4.5.0, so the fallback to base
  passes them only from there. CI runs R 4.3 and the current release;
  locally, check both rules when R changes how it matches a class.

## Release 0.1.0 (not yet)

Only once the package is in a good state (see Next), not as soon as
checks pass. The mechanics, for when that time comes:

- Before each build for CRAN, `rextendr::vendor_crates()` writes
  `src/rust/vendor.tar.xz` (1.7MB, ignored by git), so the build needs no
  network. With it, `R CMD check --as-cran` gives only the NOTE on the
  development version number.
- CI (Linux, macOS, Windows, and R 4.3 with Rust 1.83) is in place and
  passes. Then set the version to 0.1.0, add a NEWS.md, and submit.

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
