
#import "@preview/shiroa:0.3.0": *

#show: book

#book-meta(title: "Reussir Book", summary: [
  #prefix-chapter("chapters/about.typ")[About]
  = Language Reference
  - #chapter("chapters/language/syntax.typ")[Syntax]
  - #chapter("chapters/language/reference-capabilities.typ")[Reference Capabilities]
  = Development Guide
  - #chapter("chapters/development/compilation-process.typ")[Compilation Process]
  - #chapter("chapters/development/closures.typ")[Closures]
  - #chapter("chapters/development/regions.typ")[Regions]
  - #chapter("chapters/development/polymorphic-ffi.typ")[Polymorphic FFI]
])

// re-export page template
#import "/templates/page.typ": project
#let book-page = project
