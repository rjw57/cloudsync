project(cloudsync CXX)
cmake_minimum_required(VERSION 2.8)

# Update the module path to include any extra CMake modules we might ship.
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

# Find the Boost libraries
find_package(Boost REQUIRED COMPONENTS
	filesystem
	program_options
	system
)
include_directories(${Boost_INCLUDE_DIR})
link_directories(${Boost_LIBRARY_DIRS})

# Third-party libraries
add_subdirectory(third-party)

# Add our include directory to the search path
include_directories(include)

# The cloudsync library itself
add_subdirectory(cloudsync)

# Tool programs
add_subdirectory(tools)

# Documentation
add_subdirectory(docs)
