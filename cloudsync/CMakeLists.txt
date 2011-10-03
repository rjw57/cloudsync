include_directories(${CMAKE_SOURCE_DIR}/third-party/hashstream)

add_library(cloudsync
	tree.cpp
)

target_link_libraries(cloudsync
	hashstream
	${Boost_FILESYSTEM_LIBRARY}
	${Boost_SYSTEM_LIBRARY}
)
