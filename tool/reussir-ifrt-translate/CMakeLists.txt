add_mlir_tool(reussir-ifrt-translate main.cpp)
target_link_libraries(reussir-ifrt-translate PRIVATE
  $<LINK_LIBRARY:WHOLE_ARCHIVE,ReussirIfrtSerdes>
  MLIRTranslateLib
)
