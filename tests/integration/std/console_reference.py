"""Exercise stream detection and ANSI output through pipes, files, and PTYs."""

import os
from pathlib import Path
import subprocess
import sys
import tempfile


RED = b"\x1b[31mred\x1b[0m"
EXE = str(Path(sys.argv[1]).resolve())


def piped(mode):
    return subprocess.run([EXE], input=f"{mode}\n".encode(),
                          capture_output=True, check=True, timeout=15)


def terminal(mode, input_tty=False, output_tty=False, error_tty=False):
    import errno
    import pty
    import select
    import tty

    master, slave = pty.openpty()
    try:
        tty.setraw(slave)
        data = f"{mode}\n".encode()
        if input_tty:
            os.write(master, data)
        result = subprocess.run(
            [EXE], stdin=slave if input_tty else None,
            input=None if input_tty else data,
            stdout=slave if output_tty else subprocess.PIPE,
            stderr=slave if error_tty else subprocess.PIPE,
            check=True, timeout=15)
        os.close(slave)
        slave = None
        output = bytearray()
        while True:
            ready, _, _ = select.select([master], [], [], 5)
            assert ready, "PTY did not reach EOF"
            try:
                chunk = os.read(master, 4096)
            except OSError as error:
                if error.errno == errno.EIO:
                    break
                raise
            if not chunk:
                break
            output.extend(chunk)
        return result, bytes(output)
    finally:
        if slave is not None:
            os.close(slave)
        os.close(master)


def main():
    assert piped(-4).stdout == b"false/false/false"
    for mode, expected in [(-5, b"red"), (-7, RED), (-8, b"red")]:
        result = piped(mode)
        assert (result.stdout, result.stderr) == (expected, b"")
    for mode, expected in [(-6, b"red"), (-9, RED), (-10, b"red")]:
        result = piped(mode)
        assert (result.stdout, result.stderr) == (b"", expected)
    for mode, expected in [(-4, b"false/false/false"), (-5, b"red")]:
        with tempfile.TemporaryFile() as output:
            subprocess.run([EXE], input=f"{mode}\n".encode(), stdout=output,
                           stderr=subprocess.PIPE, check=True, timeout=15)
            output.seek(0)
            assert output.read() == expected
    if os.name == "posix":
        result, output = terminal(-4, input_tty=True)
        assert result.stdout == b"true/false/false" and output == b""
        _, output = terminal(-4, output_tty=True)
        assert output == b"false/true/false"
        result, output = terminal(-4, error_tty=True)
        assert result.stdout == b"false/false/true" and output == b""
        _, output = terminal(-4, input_tty=True, output_tty=True, error_tty=True)
        assert output == b"true/true/true"
        for mode, expected in [(-5, RED), (-7, RED), (-8, b"red")]:
            result, output = terminal(mode, output_tty=True)
            assert output == expected and result.stderr == b""
        for mode, expected in [(-6, RED), (-9, RED), (-10, b"red")]:
            result, output = terminal(mode, error_tty=True)
            assert output == expected and result.stdout == b""
        # A terminal on the other stream must not enable ANSI on a pipe.
        result, output = terminal(-5, error_tty=True)
        assert result.stdout == b"red" and output == b""
        result, output = terminal(-6, output_tty=True)
        assert result.stderr == b"red" and output == b""
    print("Console detection and ANSI policies passed (pipes, files, and available PTYs)")


if __name__ == "__main__":
    main()
