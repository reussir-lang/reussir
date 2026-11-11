import sys

count = int(sys.argv[1])
print('!nil = !reussir.record<compound "Nil" [value] {}>')
print('!payload = !reussir.record<compound "Payload" [value] {i64}>')
print('!t = !reussir.record<variant "Boundary" {' + ', '.join(['!nil'] + ['!payload'] * (count - 1)) + '}>')
print('module attributes { reussir.special_ptr_tag = "tbi", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {')
print('''func.func @tag(%ref: !reussir.ref<!t shared>) -> index {
  %tag = reussir.record.tag(%ref : !reussir.ref<!t shared>) : index
  return %tag : index
}
func.func @nil() -> !reussir.rc<!t> {
  %nil = reussir.record.compound : !nil
  %t = reussir.record.variant [0] (%nil : !nil) : !t
  %rc = reussir.rc.create value(%t : !t) : !reussir.rc<!t>
  return %rc : !reussir.rc<!t>
}
''')
print(f'''func.func @last(%v: i64) -> !reussir.rc<!t> {{
  %payload = reussir.record.compound(%v : i64) : !payload
  %t = reussir.record.variant [{count - 1}] (%payload : !payload) : !t
  %rc = reussir.rc.create value(%t : !t) : !reussir.rc<!t>
  return %rc : !reussir.rc<!t>
}}
}}''')
