# A JSON formatter: the tokens come from a lexer of `regex::lexer`,
# a recursive-descent parser makes a tree of them, and `std::format` lays
# the tree out.
{
  package = {
    name | String = "jsonfmt",
    version = "0.1.0",
  },
  dependencies = {
    std = { path = "../../../../library/std", version = "^0.1" },
    regex = { path = "../../../../library/regex", version = "^0.1" },
  },
  targets = {
    jsonfmt = { kind = 'executable },
  },
}
