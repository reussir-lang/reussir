# The package checked by ../std_bigint_disabled.rr: a call into
# `std::bigint` without naming `Nat`.
{
  package = {
    name | String = "std_bigint_disabled",
    version = "0.1.0",
  },
  dependencies = {
    std = { path = "../../../../library/std", version = "^0.1" },
  },
  targets = {
    archive = { kind = 'staticlib },
  },
}
