add_executable(scantree
	scantree.cpp
)

target_link_libraries(scantree
	cloudsync

	${Boost_FILESYSTEM_LIBRARY}
	${Boost_PROGRAM_OPTIONS_LIBRARY}
	${Boost_SYSTEM_LIBRARY}
)
