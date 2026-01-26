---
name: integration-test
description: Run integration tests for Reussir.
license: MPL-2.0
---

To execute the integration tests, the simplest way is to run `ninja check` in the
build directory. This will execute all the integration tests.

The trailing lines should look like this:
```bash
PASS: Reussir :: basic/failure/rc_invalid_cap.mlir (119 of 123)
PASS: Reussir :: basic/failure/region_vtable_invalid_drop.mlir (120 of 123)
PASS: Reussir :: basic/failure/record_tag_invalid_type.mlir (121 of 123)
PASS: Reussir :: basic/failure/record_tag_incomplete_record.mlir (122 of 123)
PASS: Reussir :: conversion/nested_polyffi.mlir (123 of 123)

Testing Time: 0.41s

Total Discovered Tests: 123
  Passed: 123 (100.00%)
```

Additionally, the `lit` command line provides options to customize the test run.

You can invoke the help to see all the options:

```bash
python3 tests/integration/lit build/tests/integration/ --help

usage: lit [-h] [--version] [-j N] [--config-prefix NAME] [-D NAME=VAL] [-q] [-s] [-v] [-vv] [-a] [-o PATH] [--no-progress-bar] [--show-excluded]
           [--show-skipped] [--show-unsupported] [--show-pass] [--show-flakypass] [--show-xfail] [--gtest-sharding] [--no-gtest-sharding] [--path PATH]
           [--vg] [--vg-leak] [--vg-arg ARG] [--time-tests] [--no-execute] [--xunit-xml-output XUNIT_XML_OUTPUT] [--resultdb-output RESULTDB_OUTPUT]
           [--time-trace-output TIME_TRACE_OUTPUT] [--timeout MAXINDIVIDUALTESTTIME] [--max-failures MAX_FAILURES] [--allow-empty-runs]
           [--per-test-coverage] [--ignore-fail] [--max-tests N] [--max-time N] [--order {lexical,random,smart}] [--shuffle] [-i] [--filter REGEX]
           [--filter-out REGEX] [--xfail LIST] [--xfail-not LIST] [--num-shards M] [--run-shard N] [--debug] [--show-suites] [--show-tests]
           [--show-used-features]
           TEST_PATH [TEST_PATH ...]

positional arguments:
  TEST_PATH             File or path to include in the test suite

options:
  -h, --help            show this help message and exit
  --version             show program's version number and exit
  -j N, --threads N, --workers N
                        Number of workers used for testing
  --config-prefix NAME  Prefix for 'lit' config files
  -D NAME=VAL, --param NAME=VAL
                        Add 'NAME' = 'VAL' to the user defined parameters

Output Format:
  -q, --quiet           Suppress no error output
  -s, --succinct        Reduce amount of output. Additionally, show a progress bar, unless --no-progress-bar is specified.
  -v, --verbose         For failed tests, show all output. For example, each command is printed before it is executed, so the last printed command is
                        the one that failed.
  -vv, --echo-all-commands
                        Deprecated alias for -v.
  -a, --show-all        Enable -v, but for all tests not just failed tests.
  -o PATH, --output PATH
                        Write test results to the provided path
  --no-progress-bar     Do not use curses based progress bar
  --show-excluded       Show excluded tests (EXCLUDED)
  --show-skipped        Show skipped tests (SKIPPED)
  --show-unsupported    Show unsupported tests (UNSUPPORTED)
  --show-pass           Show passed tests (PASS)
  --show-flakypass      Show passed with retry tests (FLAKYPASS)
  --show-xfail          Show expectedly failed tests (XFAIL)

Test Execution:
  --gtest-sharding      Enable sharding for GoogleTest format
  --no-gtest-sharding   Disable sharding for GoogleTest format
  --path PATH           Additional paths to add to testing environment
  --vg                  Run tests under valgrind
  --vg-leak             Check for memory leaks under valgrind
  --vg-arg ARG          Specify an extra argument for valgrind
  --time-tests          Track elapsed wall time for each test
  --no-execute          Don't execute any tests (assume PASS)
  --xunit-xml-output XUNIT_XML_OUTPUT
                        Write XUnit-compatible XML test reports to the specified file
  --resultdb-output RESULTDB_OUTPUT
                        Write LuCI ResuldDB compatible JSON to the specified file
  --time-trace-output TIME_TRACE_OUTPUT
                        Write Chrome tracing compatible JSON to the specified file
  --timeout MAXINDIVIDUALTESTTIME
                        Maximum time to spend running a single test (in seconds). 0 means no time limit. [Default: 0]
  --max-failures MAX_FAILURES
                        Stop execution after the given number of failures.
  --allow-empty-runs    Do not fail the run if all tests are filtered out
  --per-test-coverage   Enable individual test case coverage
  --ignore-fail         Exit with status zero even if some tests fail

Test Selection:
  --max-tests N         Maximum number of tests to run
  --max-time N          Maximum time to spend testing (in seconds)
  --order {lexical,random,smart}
                        Test order to use (default: smart)
  --shuffle             Run tests in random order (DEPRECATED: use --order=random)
  -i, --incremental     Run failed tests first (DEPRECATED: use --order=smart)
  --filter REGEX        Only run tests with paths matching the given regular expression
  --filter-out REGEX    Filter out tests with paths matching the given regular expression
  --xfail LIST          XFAIL tests with paths in the semicolon separated list
  --xfail-not LIST      do not XFAIL tests with paths in the semicolon separated list
  --num-shards M        Split testsuite into M pieces and only run one
  --run-shard N         Run shard #N of the testsuite

Debug and Experimental Options:
  --debug               Enable debugging (for 'lit' development)
  --show-suites         Show discovered test suites and exit
  --show-tests          Show all discovered tests and exit
  --show-used-features  Show all features used in the test suite (in XFAIL, UNSUPPORTED and REQUIRES) and exit
```
