{
  package = {
    name = "test",
    version = "0.1.0",
  },
  dependencies = {
    std = { path = "../std", version = "^0.1" },
  },
  targets.test_static = { kind = 'staticlib },
}
