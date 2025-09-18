!node = !reussir.record<compound "Node" { [field] !reussir.record<compound "Node">, i64, [field] !reussir.record<compound "Node"> }>

module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func @test() {
    %res = reussir.region.run -> !reussir.rc<!node rigid> {
      ^bb0(%reg: !reussir.region):
      %token = reussir.token.alloc : !reussir.token<align: 8, size: 48>
      %c42 = arith.constant 42 : i64
      %null = reussir.nullable.create : !reussir.nullable<!reussir.rc<!node flex>>
      %rec = reussir.record.compound (
        %null, %c42, %null : 
          !reussir.nullable<!reussir.rc<!node flex>>, i64, !reussir.nullable<!reussir.rc<!node flex>>
      ) : !node
      %rc = reussir.rc.create 
        value(%rec : !node) 
        token(%token : !reussir.token<align: 8, size: 48>)
        region(%reg : !reussir.region) : !reussir.rc<!node flex>
      reussir.region.yield %rc : !reussir.rc<!node flex>
    }
    reussir.rc.dec (%res : !reussir.rc<!node rigid>)
    return
  }
}