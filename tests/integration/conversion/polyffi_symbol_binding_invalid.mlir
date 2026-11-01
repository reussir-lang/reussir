// RUN: %reussir-opt %s --verify-diagnostics
module {
  // expected-error @+1 {{substitution requires a function symbol}}
  reussir.polyffi texture("[:missing:]") substitutions({missing = @absent})
}
