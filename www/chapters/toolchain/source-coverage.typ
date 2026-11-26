#import "/book.typ": book-page

#show: book-page.with(title: "Source-Based Code Coverage")

= Source-Based Code Coverage

Source-based code coverage shows which parts of your `.rr` source files a
program executes, and how many times. Reussir writes the coverage data in the
LLVM format that clang (`-fprofile-instr-generate -fcoverage-mapping`) and
rustc (`-C instrument-coverage`) also use. Thus you read the data with the
LLVM tools `llvm-profdata` and `llvm-cov`.

A fuzz build uses SanitizerCoverage to guide libFuzzer. SanitizerCoverage does
not show which source lines a corpus executes. Source-based code coverage
shows this.

== How It Works

A coverage build and its report have these steps:

+ The compiler adds a counter at the entry of each function and at the start
  of each branch, for example each branch of an `if`.
+ The compiler writes a coverage mapping into the object file. The mapping
  gives the source region of each counter.
+ At exit, the program writes its counters to a raw profile (a `.profraw`
  file).
+ `llvm-profdata merge` merges the raw profiles into an indexed profile (a
  `.profdata` file).
+ `llvm-cov` reads the indexed profile and the coverage mapping of the
  executable. Then it writes the report.

== Compile with `rrc`

Give `--instrument-coverage` to `rrc`. These commands compile and run a
program, and then write a summary:

```sh
rrc main.rr --emit executable --relocation-mode pic --instrument-coverage -o main
LLVM_PROFILE_FILE=main.profraw ./main
llvm-profdata merge -sparse main.profraw -o main.profdata
llvm-cov report ./main -instr-profile=main.profdata -Xdemangler=llvm-cxxfilt
```

`LLVM_PROFILE_FILE` sets the path of the raw profile. If you do not set it,
the program writes `default.profraw` in the current directory.

`rrc` links the profile runtime of the Rust toolchain, `profiler_builtins`,
into the executable. The profile runtime writes the raw profile. Without this
runtime, `rrc` stops with an error.

Reussir symbols use the Rust v0 mangling. `-Xdemangler=llvm-cxxfilt` shows
the function names in the report as Reussir paths.

== Build with `rene`

`rene build --instrument-coverage` compiles every package with
`rrc --instrument-coverage`. The profile field `instrument_coverage = true` in
`rene.ncl` has the same effect for each build of that profile. A coverage
build puts its products in `<build-dir>/<profile>/coverage/`. Thus it does not
replace the products of the normal build of the same profile.

== Report the Coverage of a Fuzz Target

`rene coverage` builds a fuzz target with coverage, runs it over a corpus,
and writes the report to stdout:

```
rene coverage [OPTIONS] <TARGET> [CORPUS]... -- [LLVM_COV_ARGS]...
```

`TARGET` is a fuzz target: an executable target of the manifest, with
`fuzzer = true` in the target or in the profile. `CORPUS` is a list of corpus
directories or input files. The command does these steps:

+ It builds the target as `rene build --instrument-coverage --bin <TARGET>`
  does.
+ It runs the target one time over each corpus input (`-runs=0`). The output
  of the target goes to stderr.
+ It merges the raw profiles with `llvm-profdata merge -sparse`.
+ It writes the report with `llvm-cov`.

`--profile`, `--linker`, `-j`, `--manifest-path` and `--build-dir` have the
same function as for `rene build`. `--llvm-cov` selects the `llvm-cov`
command:

/ `report`: A summary for each source file. This is the default.
/ `show`: Each source line with its execution count.
/ `export`: The coverage data as JSON.

The arguments after `--` go to `llvm-cov`. For example:

```sh
rene coverage suite corpus
rene coverage suite corpus --llvm-cov show -- -format=html -output-dir=coverage-html
rene coverage suite corpus --llvm-cov export -- -format=lcov > suite.lcov
```

The merged profile stays beside the executable, as `<executable>.profdata`
(for example `reussir-build/dev/coverage/suite.profdata`). You can give it to
`llvm-cov` again with other options. Each run removes the raw profiles of the
previous run. Thus the report shows only the corpus of the last run.

`rene` finds `llvm-profdata`, `llvm-cov` and `llvm-cxxfilt` on `PATH`. To use
other tools, set `REUSSIR_LLVM_PROFDATA`, `REUSSIR_LLVM_COV` and
`REUSSIR_LLVM_CXXFILT` to their paths. `rene` gives `llvm-cxxfilt` to
`llvm-cov` as the demangler of the function names. `rene` finds the tools
before the build. If a tool is missing, the command stops before it compiles.

== Read the Report

`llvm-cov report` writes a table with one row for each source file and a
`TOTAL` row. For regions, functions, lines and branches, the table gives the
number, the number that did not execute (the `Missed` column), and the
percentage that executed (the `Cover` column, or `Executed` for functions).

`llvm-cov show` writes each source line with its execution count. In this
part of a `show` report, the corpus runs `small_label` three times, and it
does not run `large_label`:

```
   11|      3|fn small_label(byte: u8) -> u64 {
   12|      3|    (byte as u64) + 1
   13|      3|}
   14|       |
   15|      0|fn large_label(byte: u8) -> u64 {
   16|      0|    (byte as u64) * 2
   17|      0|}
```

The first column is the line number. The second column is the execution
count. A count of `0` shows a line that did not execute. An empty count shows
a line with no code, for example an empty line or a comment.

== Coverage Scope

The `rrc` options `--sancov-include` and `--sancov-exclude` set the coverage
scope. The scope applies to SanitizerCoverage and to source-based code
coverage. A function outside the scope gets no counters, and the report does
not show it. Each scope is a prefix of demangled paths: a package, a module or
a function. A fuzz build (`--fuzzer`) excludes the `test` package by default.
An explicit `--sancov-include` replaces this default.

In `rene.ncl`, give the scope to `rrc` with `extra_flags`:

```
profiles.fuzz = {
  fuzzer = true,
  extra_flags = ["--sancov-include", "my_package"],
},
```

== Limits

- A function with no source location gets no counters.
- The counters make a coverage build slower and larger than a normal build.
- Use `llvm-profdata` and `llvm-cov` of the LLVM version that Reussir uses.
  Tools of a different version can fail to read the raw profile or the
  coverage mapping.
- If the fuzz target fails on a corpus input, `rene coverage` stops before the
  merge and writes no report.
