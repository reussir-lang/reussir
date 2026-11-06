"""Verify `std::text::Text` and `std::env::{args, var}` across process launches."""

import subprocess
from format_test_support import ENV, EXE, POSIX_TARGET


def main():
    env = ENV.copy()
    env.pop("REUSSIR_STD_ENV_MISSING", None)
    env["REUSSIR_STD_ENV_PRESENT"] = "value_λ猫"
    env["REUSSIR_STD_ENV_EMPTY"] = ""

    result = subprocess.run(
        [EXE, "alpha", "", "λ猫"],
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=True,
        timeout=15,
    )
    assert result.stdout == "alpha||λ猫", repr(result.stdout)

    if POSIX_TARGET:
        raw_env = {k.encode(): v.encode() for k, v in env.items()}
        raw_env[b"REUSSIR_STD_ENV_CHECK_NON_UTF8"] = b"1"
        raw_env[b"REUSSIR_STD_ENV_BAD_UTF8"] = b"\xff\xfe"
        bad = subprocess.run(
            [EXE, "ok"],
            env=raw_env,
            capture_output=True,
            check=True,
            timeout=15,
        )
        assert bad.stdout == b"ok", repr(bad.stdout)


if __name__ == "__main__":
    main()
