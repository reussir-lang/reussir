"""Compare native formatting with a small, deliberately unoptimized model.

The model recursively computes flat layouts at each group. The production
renderer instead measures a reversed token stream once.
"""

import os
import random
import subprocess
from format_test_support import ENV, EXE, POSIX_TARGET


ATOMS = ["a", "bb", "ccc", "λ猫", ""]


def cat(*docs):
    result = ("text", "")
    for doc in docs:
        result = ("append", result, doc)
    return result


def expand(doc):
    if doc[0] == "tuple":
        items = doc[1]
        if not items:
            return ("text", "()")
        body = items[0]
        for item in items[1:]:
            body = cat(body, ("text", ","), ("line",), item)
        singleton = len(items) == 1
        if singleton:
            body = cat(body, ("text", ","))
        return ("group", cat(("text", "("),
                            ("nest", 4, cat(("soft",), body)),
                            ("closing", "" if singleton else ","),
                            ("text", ")")))
    return doc


def flat(doc):
    doc = expand(doc)
    kind = doc[0]
    if kind == "text":
        return doc[1]
    if kind == "hard":
        return None
    if kind == "line":
        return " "
    if kind in ("soft", "closing"):
        return ""
    if kind == "append":
        left, right = flat(doc[1]), flat(doc[2])
        return None if left is None or right is None else left + right
    return flat(doc[-1])


def render(doc, width):
    output = []
    column = 0
    pending = 0

    def text(value):
        nonlocal column, pending
        if value:
            output.extend((" " * pending, value))
            pending = 0
            column += len(value)

    def scope(doc, indent):
        doc = expand(doc)
        if doc[0] == "append":
            return scope(doc[1], indent) + scope(doc[2], indent)
        if doc[0] == "ansi":
            return scope(doc[1], indent)
        if doc[0] == "nest":
            return scope(doc[2], indent + doc[1])
        return [(doc, indent)]

    def emit_fill(doc, indent):
        events = scope(doc, indent)
        for i, (event, level) in enumerate(events):
            if event[0] not in ("line", "soft", "closing"):
                emit(event, level, False)
                continue
            needed = 1 if event[0] == "line" else 0
            available = True
            for following, _ in events[i + 1:]:
                if following[0] in ("line", "soft", "closing"):
                    if following[0] == "closing":
                        needed += len(following[1])
                    break
                candidate = flat(following)
                if candidate is None:
                    available = False
                    break
                needed += len(candidate)
            emit(event, level, available and column + needed <= width)

    def emit(doc, indent, flattened):
        nonlocal column, pending
        doc = expand(doc)
        kind = doc[0]
        if kind == "text":
            text(doc[1])
        elif kind == "ansi":
            emit(doc[1], indent, flattened)
        elif kind == "append":
            emit(doc[1], indent, flattened)
            emit(doc[2], indent, flattened)
        elif kind == "nest":
            emit(doc[2], indent + doc[1], flattened)
        elif kind in ("group", "fill"):
            candidate = flat(doc[1])
            fits = candidate is not None and column + len(candidate) <= width
            if kind == "fill" and not (flattened or fits):
                emit_fill(doc[1], indent)
            else:
                emit(doc[1], indent, flattened or fits)
        elif kind != "hard" and flattened:
            if kind == "line":
                pending += 1
                column += 1
        else:
            if kind == "closing":
                text(doc[1])
            output.append("\n")
            pending = indent
            column = indent

    emit(doc, 0, False)
    return "".join(output)


def commands(doc):
    kind = doc[0]
    if kind == "text":
        return [[0, ATOMS.index(doc[1])]]
    if kind in ("line", "soft", "hard"):
        return [[{"line": 1, "soft": 2, "hard": 3}[kind]]]
    if kind == "ansi":
        return commands(doc[1]) + [[9]]
    if kind == "append":
        return commands(doc[1]) + commands(doc[2]) + [[4]]
    if kind == "nest":
        return commands(doc[2]) + [[5, doc[1]]]
    if kind in ("group", "fill"):
        return commands(doc[1]) + [[6 if kind == "group" else 8]]
    return [op for item in doc[1] for op in commands(item)] + [[7, len(doc[1])]]


def generate(rng, depth):
    doc = generate_body(rng, depth)
    return ("ansi", doc) if rng.randrange(4) == 0 else doc


def generate_body(rng, depth):
    if depth == 0:
        return ("text", rng.choice(ATOMS))
    choice = rng.randrange(6)
    if choice == 0:
        return ("tuple", [generate(rng, depth - 1) for _ in range(rng.randrange(4))])
    if choice == 1:
        return ("nest", rng.randrange(5), generate(rng, depth - 1))
    body = cat(generate(rng, depth - 1), (rng.choice(["line", "soft", "hard"]),),
               generate(rng, depth - 1))
    return ("group", body) if choice <= 3 else (("fill", body) if choice == 4 else body)


def main():
    rng = random.Random(924)
    cases = []
    for _ in range(120):
        doc = ("group", generate(rng, 3))
        for width in [0, 1, 4, 8, 16, 32, 80]:
            cases.append((doc, width))
    data = [str(len(cases))]
    expected = []
    for doc, width in cases:
        ops = commands(doc)
        data.extend([str(width), str(len(ops))])
        data.extend(str(value) for op in ops for value in op)
        expected.append(render(doc, width))
    result = subprocess.run([EXE], input="\n".join(data) + "\n", text=True, encoding="utf-8", env=ENV,
                            capture_output=True, timeout=90, check=True)
    actual = result.stdout.split("\n===\n")
    assert actual[-1] == "", repr(result.stdout[-100:])
    assert len(actual) == len(expected) + 1
    for i, (got, want) in enumerate(zip(actual, expected)):
        assert got == want, (i, cases[i], repr(got), repr(want))
    for mode, stdout, stderr in [(-1, "stdout λ", ""), (-2, "", "stderr 猫")]:
        printed = subprocess.run([EXE], input=f"{mode}\n", text=True, encoding="utf-8", env=ENV,
                                 capture_output=True, timeout=10, check=True)
        assert (printed.stdout, printed.stderr) == (stdout, stderr), (mode, printed)
    if POSIX_TARGET:
        read_fd, write_fd = os.pipe()
        os.close(read_fd)
        try:
            # Keep Python's ignored SIGPIPE disposition so the sink observes EPIPE.
            failed = subprocess.run([EXE], input="-3\n", text=True, encoding="utf-8", env=ENV,
                                    stdout=write_fd, stderr=subprocess.PIPE,
                                    restore_signals=False, timeout=10, check=True)
        finally:
            os.close(write_fd)
        assert failed.stderr == "broken pipe", failed.stderr
    print(f"{len(cases)} reference layouts and output sinks passed")


if __name__ == "__main__":
    main()
