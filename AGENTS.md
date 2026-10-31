# Agent instructions

- For the NixOS runtime, avoid running in the sandbox, where execution is
  expected to fail and waste tokens.
- Never skip sccache when the remote server is online. Spend time probing the
  cache and avoid repeated builds instead of disabling caching.
- All serialization in Reussir must support a complete round trip. Serialized
  representations, including MIR and HIR, must carry all information needed
  to resume exactly; never skip or omit information.
- When working with a maintainer, do not block their review by running excessive
  tests. Always push and create a draft PR first, then test and fix forward. Keep
  the reviewer's ability to work in parallel in mind.
- Never bump an ABI or cache version within a development cycle. Pause and ask
  for permission if you think a bump is needed.
- Avoid paranoid descriptions and invented jargon in documentation, READMEs,
  and code comments. Keep code comments concise; they must not inflate patches.
- Do not introduce security or compatibility concerns that have not been
  discussed in context. Pause and ask before implementing a mitigation.
- Never reinvent the wheel. Reuse utilities already in the codebase and
  analyses provided by LLVM or MLIR; for example, use `mlir::Liveness` instead
  of implementing a separate, less efficient analysis for the same purpose.
  Prefer popular, maintained crates for required features over handwritten code,
  and use well-integrated approaches, such as deriving command-line handling from
  a struct with a procedural macro instead of assembling it with low-level APIs.
- Prefer modern coding practices. For C++, consider `std::generator`,
  `std::span`, and views; for Rust, consider recent language features; and for
  MLIR and LLVM, follow current practices.
- Do not introduce deferred tasks or workarounds that are not in context. If a
  new change is needed outside the initial scope, pause and report it. Prefer to
  fix a small bug directly, or at least locate its root cause.
- Do not eagerly update the changelog or the maintainer's design documents. Ask
  first if you think an update is important.
- Never refer to a conversation in a code comment. Code comments may refer to
  PRs or issues, but design documents must not. Write for future developers and
  users who have no access to the conversation context.
- The comprehensive benchmark suite is at `reussir-lang/benchmark`. Report
  performance regressions when a patch affects code generation. Run relevant
  suites rather than making yourself or other developers wait for an excessive
  full suite, and keep review parallelism in mind.
- During multi-agent fuzzing, distinguish upstream bugs from downstream bugs.
  List upstream bugs, but do not let them divert the effort.
- Design as an expert engineer and meet the standards of the Rust, C++, LLVM,
  and MLIR communities. Consider production user experience, portability, and
  future extensibility. Avoid spawning external processes without good reason
  and carrying ad hoc scripting habits into production software.
- Use the `[scope] description` format for commit messages and PR titles.
- Format your code and run Clippy and clang-tidy as applicable.
- Check skills for existing tools. When implementing Reussir data structures or
  libraries, pay attention to LLVM remarks and Reussir's own remarks, and use
  them to produce high-quality code.
