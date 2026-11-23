# Regex library integration tests

Each top-level `.rr` file is both a lit test and the crate root of one
executable target in `rene.ncl`. Every test invokes the same package against
`%regex_rene_build`; rene's build-directory lock queues concurrent lit
workers, so the first invocation builds the dependencies and the rest reuse
them.
