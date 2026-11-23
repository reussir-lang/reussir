{
  package = {
    name = "regex",
    version = "0.1.0",
  },
  dependencies = {
    std = { path = "../std", version = "^0.1" },
  },
  targets.regex_static = { kind = 'staticlib },
}
