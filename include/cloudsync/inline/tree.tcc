#ifndef INSIDE_CS_TREE_HPP
#error "This file must only be included from within tree.hpp"
#endif

#include <utility>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

namespace cs { namespace tree {

namespace fs = boost::filesystem;

template<typename OutputIterator>
void scan_directory(
		const boost::filesystem::path& dirpath,
		OutputIterator out)
{

	// check that what we've been passed actually _is_ a directory
	if(!fs::is_directory(dirpath))
	{
		BOOST_THROW_EXCEPTION(scan_error() << scan_error_info("not a directory"));
	}

	// scan each entry in the directory
	BOOST_FOREACH(const fs::directory_entry& de,
			std::make_pair(fs::directory_iterator(dirpath), fs::directory_iterator()))
	{
		*out++ = entry(de);
	}
}

} }
