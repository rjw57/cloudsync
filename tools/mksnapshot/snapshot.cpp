#include <list>
#include <sstream>
#include <utility>
#include <boost/foreach.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <hashstream/hashstream.hpp>

#include "snapshot.hpp"

namespace fs = boost::filesystem;
namespace pt = boost::property_tree;

namespace cs
{

const std::string file_object_type("file");
const std::string tree_object_type("tree");

object_name compute_hash(const fs::path& p)
{
	if(!fs::is_regular_file(p))
	{
		throw snapshot_error("Not a regular file: " + p.generic_string());
	}

	return hashstream::hex_digest(hashstream::SHA256, fs::ifstream(p));
}

object_name compute_hash(const std::string& s)
{
	return hashstream::hex_digest(hashstream::SHA256, s);
}

snapshot_dir::snapshot_dir()
{ }

snapshot_dir::snapshot_dir(const fs::path& root)
{
	set_root(root);
}

snapshot_dir::~snapshot_dir()
{ }

void snapshot_dir::set_root(const fs::path& base)
{
	fs::path new_root(base / ".cs");

	// root must be absolute
	if(!new_root.has_root_path())
	{
		throw snapshot_error("Snapshot root path is not absolute: " + new_root.generic_string());
	}

	// try to create the root if it doesn't exist
	if(!fs::exists(new_root))
	{
		if(!fs::create_directory(new_root))
		{
			throw snapshot_error("Failed to create directory: " + new_root.generic_string());
		}
	}

	// ensure root is a directory
	if(!fs::is_directory(new_root))
	{
		throw snapshot_error("Snapshot root is not a directory: " + new_root.generic_string());
	}

	root_ = new_root;
}

const fs::path snapshot_dir::resolve(const fs::path& p) const
{
	// path must be relative to snapshot root
	if(p.has_root_path())
	{
		throw snapshot_error("Path is not relative: " + p.generic_string());
	}

	// check the root directory exists
	if(!fs::is_directory(root_))
	{
		throw snapshot_error(
				"Snapshot root is not a directory or doesn't exist: " +
				root_.generic_string());
	}

	// make sure each subpath exists
	fs::path subpath(root_);
	BOOST_FOREACH(const fs::path& element, p.parent_path())
	{
		subpath /= element;

		if(!fs::exists(subpath))
		{
			if(!fs::create_directory(subpath))
			{
				throw snapshot_error("Failed to create directory: " + subpath.generic_string());
			}
		}

		if(!fs::is_directory(subpath))
		{
			throw snapshot_error("Path component not a directory: " + subpath.generic_string());
		}
	}

	return root_ / p;
}

object_store_location snapshot_dir::resolve_object(const object_name& hash) const
{
	// split the hash into prefixes and a remainder.
	std::string prefix1, prefix2, remainder(hash);

	// split off prefix 1
	if(remainder.size() > 2)
	{
		prefix1 = remainder.substr(0, 2);
		remainder = remainder.substr(2);
	}

	// split off prefix 2
	if(remainder.size() > 2)
	{
		prefix2 = remainder.substr(0, 2);
		remainder = remainder.substr(2);
	}

	// check we could split both prefixes off
	if((prefix1.size() == 0) || (prefix2.size() == 0))
	{
		throw snapshot_error("Blob hash must have >4 characters in it");
	}

	// compute the object-store path
	object_store_location loc;
	fs::path base(fs::path("objects") / prefix1 / prefix2);
	loc.contents = resolve(base / (remainder + ".contents"));
	loc.header = resolve(base / (remainder + ".header"));

	return loc;
}

bool snapshot_dir::object_is_valid(const object_name& name) const
{
	std::string tmp;
	return object_is_valid(name, tmp);
}

bool snapshot_dir::object_is_valid(const object_name& name, std::string& why) const
{
	// get the object store location for this object
	object_store_location loc(resolve_object(name));

	if(!fs::exists(loc.contents) || !fs::is_regular_file(loc.contents))
	{
		why = loc.contents.generic_string() +
			": object contents do not exist or is not a file.";
		return false;
	}

	if(!fs::exists(loc.header) || !fs::is_regular_file(loc.header))
	{
		why = loc.header.generic_string() +
			": object header does not exist or is not a file.";
		return false;
	}

	std::string type;
	uintmax_t contents_size;

	fs::ifstream ifs(loc.header);
	ifs >> type >> contents_size;

	if(contents_size != fs::file_size(loc.contents))
	{
		why = loc.contents.generic_string() + " is wrong size.";
		return false;
	}

	if(compute_hash(loc.contents) != name)
	{
		why = name + ": checksum validation failed.";
		return false;
	}

	return true;
}

object_name snapshot_dir::add_object(const object_type& type, const fs::path& path) const
{
	// compute the name
	object_name name(compute_hash(path));

	// get the object store location for this object
	object_store_location loc(resolve_object(name));

	// create a link to the contents if necessary
	if(!fs::exists(loc.contents))
	{
		boost::system::error_code ec;
		fs::create_hard_link(path, loc.contents, ec);
		if(0 != ec)
		{
			throw fs::filesystem_error(
					path.generic_string() + " -> " +
					loc.contents.generic_string() + ": Failed to create hard link", ec);
		}
	}

	// write the header
	uintmax_t contents_size(fs::file_size(path));
	fs::ofstream ofs(loc.header);
	ofs << type << '\n' << contents_size;
	ofs.close();

	// check the object
	std::string why;
	if(!object_is_valid(name, why))
	{
		throw snapshot_error("Object verification failed: " + why);
	}

	return name;
}

object_name snapshot_dir::add_object(const object_type& type, const std::string& contents) const
{
	// compute the name
	object_name name(compute_hash(contents));

	// get the object store location for this object
	object_store_location loc(resolve_object(name));

	// write contents if necessary
	if(!fs::exists(loc.contents))
	{
		// write the file
		fs::ofstream ofs(loc.contents);
		ofs << contents;
		ofs.close();
	}

	// write the header
	uintmax_t contents_size(contents.size());
	fs::ofstream ofs(loc.header);
	ofs << type << ' ' << contents_size;
	ofs.close();

	// check the object
	std::string why;
	if(!object_is_valid(name, why))
	{
		throw snapshot_error("Object verification failed: " + why);
	}

	return name;
}

struct tree_entry
{
	std::string filename;
	std::string object_name;
};

// return true iff a < b
inline bool cmp_entry_(const tree_entry& a, const tree_entry& b)
{
	return a.object_name < b.object_name;
}

object_name snapshot_dir::add_tree_from_dir(const fs::path& dir) const
{
	if(!fs::is_directory(dir))
	{
		throw snapshot_error(dir.generic_string() + ": Not a directory");
	}

	// Create a list of entries. This will be used later to create the tree data structure.
	std::list<tree_entry> entries;

	BOOST_FOREACH(const fs::directory_entry& entry,
			std::make_pair(fs::directory_iterator(dir), fs::directory_iterator()))
	{
		object_name entry_hash;
		const fs::path entry_path(entry.path());
		tree_entry entry;

		entry.filename = entry_path.filename().generic_string();

		if(fs::is_directory(entry_path))
		{
			// special case: skip cloud sync snapshot dir
			if(entry_path.filename() == ".cs")
			{
				continue;
			}

			entry.object_name = add_tree_from_dir(entry_path);
		}
		else if(fs::is_regular_file(entry_path))
		{
			entry.object_name = add_object(file_object_type, entry_path);
		}
		else
		{
			// hmm... some interesting file or a symlink, skip it
			continue;
		}

		entries.push_back(entry);
	}

	// sort the entries by object name
	entries.sort(cmp_entry_);

	// output the entries to a tree object
	std::stringstream ss;

	BOOST_FOREACH(const tree_entry& entry, entries)
	{
		ss << entry.object_name << ' ' << entry.filename << std::endl;
	}

	// add the object to the object store
	return add_object(tree_object_type, ss.str());
}

}
