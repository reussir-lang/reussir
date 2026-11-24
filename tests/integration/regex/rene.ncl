{
  package = {
    name = "regex_integration",
    version = "0.1.0",
  },
  dependencies = {
    std = { path = "../../../library/std", version = "^0.1" },
    regex = { path = "../../../library/regex", version = "^0.1" },
    test = { path = "../../../library/test", version = "^0.1" },
  },
  profiles = {
    fuzz = {
      opt = "aggressive",
      fuzzer = true,
    },
  },
  targets = {
    alphabet = { kind = 'executable, path = "alphabet.rr", fuzzer = true },
    automaton = { kind = 'executable, path = "automaton.rr", fuzzer = true },
    lexer = { kind = 'executable, path = "lexer.rr", fuzzer = true },
    node = { kind = 'executable, path = "node.rr", fuzzer = true },
    tree = { kind = 'executable, path = "tree.rr", fuzzer = true },
    utf8 = { kind = 'executable, path = "utf8.rr", fuzzer = true },
  },
}
