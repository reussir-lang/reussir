{
  package = { name = "source_coverage", version = "0.1.0" },
  dependencies = {
    std = { path = "../../../../library/std", version = "^0.1" },
    test = { path = "../../../../library/test", version = "^0.1" },
  },
  profiles.fuzz = {
    opt = "aggressive",
    fuzzer = true,
    extra_flags = ["--sancov-include", "source_coverage"],
  },
  targets.suite = { kind = 'executable, path = "suite.rr", fuzzer = true },
}
