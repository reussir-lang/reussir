"""Check runtime image dimensions and last-iteration escapes against a scalar orbit."""

import math
import subprocess
import sys


def reference(rows, columns, limit):
    palette = " .,:;irsXA253hMHGS#9B&"
    lines = []
    for row in range(rows):
        line = []
        for column in range(columns):
            c = complex(
                (column + 0.5) * 3 / columns - 2.2,
                (row + 0.5) * 6 / columns - 3 * rows / columns,
            )
            z = 0j
            for count in range(1, limit + 1):
                z = z * z + c
                if z.real * z.real + z.imag * z.imag > 4:
                    index = int(math.log2(count + 1) / math.log2(limit + 1) * 20)
                    line.append(palette[index])
                    break
            else:
                line.append("@")
        lines.append("".join(line))
    return "\n".join(lines) + "\n"


# Use odd rectangular images. Low iteration limits avoid fractal-boundary
# sensitivity to CPU/GPU floating-point contraction in this exact comparison.
for rows, columns, limit in [(17, 129, 1), (19, 131, 4)]:
    result = subprocess.run(
        [sys.argv[1]], input=f"{rows}\n{columns}\n{limit}\n",
        text=True, capture_output=True, check=True,
    )
    assert result.stdout == reference(rows, columns, limit), (
        f"incorrect {rows}x{columns} image with {limit} iterations"
    )
