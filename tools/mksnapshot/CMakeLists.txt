include_directories(${CMAKE_SOURCE_DIR}/third-party/hashstream)

add_executable(mksnapshot
	mksnapshot.cpp
	snapshot.cpp
)

target_link_libraries(mksnapshot
	hashstream
	${Boost_FILESYSTEM_LIBRARY}
	${Boost_SYSTEM_LIBRARY}
)
