{
  package = { name = "coverage_scope", version = "0.1.0" },
  dependencies = {
    std = { path = "../../../../library/std", version = "^0.1" },
    test = { path = "../../../../library/test", version = "^0.1" },
  },
  profiles.fuzz = {
    opt = "aggressive",
    fuzzer = true,
    extra_flags = ["--sancov-include", "coverage_scope"],
  },
  targets.noop = { kind = 'executable, path = "noop.rr", fuzzer = true },
}
