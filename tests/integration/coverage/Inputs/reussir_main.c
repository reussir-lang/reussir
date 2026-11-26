// Calls the `#[main]` function of a Reussir object, as the launcher of
// `rrc --emit executable` does.
void __reussir_main(void);

int main(void) {
  __reussir_main();
  return 0;
}
