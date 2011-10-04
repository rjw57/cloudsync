#ifndef CS_TREE_HPP_INCLUDED
#define CS_TREE_HPP_INCLUDED

#include <ostream>
#include <string>
#include <boost/exception/all.hpp>
#include <boost/filesystem.hpp>
#include <boost/shared_ptr.hpp>

namespace cs { namespace tree {

typedef boost::error_info<struct scan_error_info_tag, std::string> scan_error_info;
struct scan_error : virtual boost::exception, virtual std::exception {
	virtual const char* what() const throw();
};

/// @brief An entry within a tree.
struct entry
{
	typedef std::deque<entry> entry_collection_t;

	entry();
	entry(const boost::filesystem::directory_entry& de);

	/// @brief The name of this entry.
	std::string filename;

	/// @brief A hash of it's contents (if it is a normal file).
	std::string hash;

	/// @brief A pointer to a sub-entry is this represents a directory.
	boost::shared_ptr<entry_collection_t> sub_entry;

	/// @brief The size of the file in bytes.
	uintmax_t file_size;

	/// @brief The time the file was last modified.
	std::time_t last_modified_time;
};

std::ostream& operator << (std::ostream& os, const entry& e);

template<typename OutputIterator>
void scan_directory(
		const boost::filesystem::path& dirpath,
		OutputIterator out);

} }

#define INSIDE_CS_TREE_HPP
#include "inline/tree.tcc"
#undef INSIDE_CS_TREE_HPP

#endif // CS_TREE_HPP_INCLUDED
