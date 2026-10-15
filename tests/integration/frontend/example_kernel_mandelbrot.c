#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define WIDTH 1024
#define HEIGHT 1024
#define ITERATIONS 128
#define PIXELS ((uint64_t)WIDTH * HEIGHT)

extern uint64_t render_mandelbrot(void);
static uint8_t image[WIDTH * HEIGHT];
static uint64_t received, interior, quick_escape, total_iterations;

// Compute independent scalar orbits at coordinates well away from the fractal
// boundary. No exact whole-image hash: fused GPU arithmetic can legitimately
// change escape times very close to that boundary.
static int reference(unsigned row, unsigned column) {
  const double cr = (double)column * (3.0 / WIDTH) - 2.0;
  const double ci = (double)row * (3.0 / HEIGHT) - 1.5;
  double zr = 0.0, zi = 0.0;
  for (int count = 1; count <= ITERATIONS; ++count) {
    const double next_r = zr * zr - zi * zi + cr;
    const double next_i = 2.0 * zr * zi + ci;
    zr = next_r;
    zi = next_i;
    if (zr * zr + zi * zi > 4.0)
      return count;
  }
  return ITERATIONS;
}

void mandelbrot_pixel(uint64_t index, int32_t value) {
  assert(index == received && index < PIXELS);
  assert(value >= 1 && value <= ITERATIONS);
  ++received;
  interior += value == ITERATIONS;
  quick_escape += value <= 4;
  total_iterations += (uint64_t)value;
  image[index] = (uint8_t)value;
}

int main(int argc, char **argv) {
  const uint64_t count = render_mandelbrot();
  assert(count == PIXELS && received == PIXELS);
  assert(interior > PIXELS / 10 && interior < PIXELS / 4);
  assert(quick_escape > PIXELS / 3 && quick_escape < 9 * PIXELS / 10);
  // Dyadic grid coordinates make these stable samples exactly representable.
  // The final three samples add five/eight-step escapes and an off-axis
  // interior point, each constant across its 3x3 pixel neighborhood.
  const unsigned samples[][2] = {
      {0, 0},   {0, 512},    {0, 1023},  {256, 0},    {256, 1023},
      {512, 0}, {512, 256},  {512, 512}, {512, 640},  {512, 896},
      {768, 0}, {768, 1023}, {1023, 0},  {1023, 512}, {1023, 1023},
      {128, 640}, {240, 576}, {384, 640}};
  for (unsigned i = 0; i < sizeof(samples) / sizeof(samples[0]); ++i) {
    const unsigned row = samples[i][0], col = samples[i][1];
    assert(image[row * WIDTH + col] == reference(row, col));
  }
  printf("Mandelbrot %dx%d, %d iterations: interior=%" PRIu64
         ", quick_escape=%" PRIu64 ", iteration_sum=%" PRIu64 "\n",
         WIDTH, HEIGHT, ITERATIONS, interior, quick_escape, total_iterations);
  if (argc > 1) {
    FILE *out = fopen(argv[1], "wb");
    assert(out);
    assert(fprintf(out, "P5\n%d %d\n255\n", WIDTH, HEIGHT) > 0);
    for (uint64_t i = 0; i < PIXELS; ++i) {
      // Black interior, bright escape bands, without changing verified data.
      const unsigned char shade =
          image[i] == ITERATIONS ? 0
                                 : (unsigned char)(255 - (image[i] * 11) % 224);
      assert(fwrite(&shade, 1, 1, out) == 1);
    }
    assert(fclose(out) == 0);
    printf("Wrote %s\n", argv[1]);
  }
  return 0;
}
