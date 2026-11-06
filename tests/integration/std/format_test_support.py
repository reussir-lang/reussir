"""Process settings shared by the formatting integration drivers."""

import os
from pathlib import Path
import sys


EXE = str(Path(sys.argv[1]).resolve())
WINDOWS_TARGET = os.name == "nt" or Path(EXE).suffix.lower() == ".exe"
POSIX_TARGET = os.name == "posix" and not WINDOWS_TARGET
ENV = os.environ.copy()
if WINDOWS_TARGET and os.name == "posix":
    # Wine's own diagnostics share stderr with the program under test.
    ENV["WINEDEBUG"] = "-all"
