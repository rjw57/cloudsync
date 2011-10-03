find_package(Doxygen)

if(DOXYGEN_FOUND)
  set(DOC_TARGET_NAME "docs")
  set(DOC_ALWAYS_BUILD NO)
  set(DOC_INPUT "${CMAKE_SOURCE_DIR}/include")
  include(UseDoxygen)
endif(DOXYGEN_FOUND)
