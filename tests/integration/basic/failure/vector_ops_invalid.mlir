// RUN: %reussir-opt %s -verify-diagnostics -split-input-file

func.func @cttz_elts_of_integers(%v: vector<16xi8>) -> i64 {
  // expected-error @+1 {{operand #0 must be}}
  %n = reussir.vector.cttz_elts %v : vector<16xi8> -> i64
  return %n : i64
}

// -----

func.func @cttz_elts_of_two_dimensions(%m: vector<2x8xi1>) -> i64 {
  // expected-error @+1 {{operand #0 must be}}
  %n = reussir.vector.cttz_elts %m : vector<2x8xi1> -> i64
  return %n : i64
}

// -----

func.func @cttz_elts_into_float(%m: vector<16xi1>) -> f64 {
  // expected-error @+1 {{result #0 must be signless integer, but got 'f64'}}
  %n = reussir.vector.cttz_elts %m : vector<16xi1> -> f64
  return %n : f64
}
