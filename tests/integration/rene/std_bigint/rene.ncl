# The package checked by ../std_bigint.rr: decimal text and hashing of
# `Nat` and `Int` through `std::bigint`.
{
  package = {
    name | String = "std_bigint",
    version = "0.1.0",
  },
  dependencies = {
    std = { path = "../../../../library/std", version = "^0.1" },
  },
  targets = {
    std_bigint = { kind = 'executable },
  },
}
