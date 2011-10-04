#ifndef CS_SNAPSHOT_HPP_INCLUDED
#define CS_SNAPSHOT_HPP_INCLUDED

#include <istream>
#include <stdexcept>
#include <boost/exception/all.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

namespace cs
{

namespace fs = boost::filesystem;

typedef std::string object_name;
typedef std::string object_type;

// common object types
extern const std::string file_object_type;
extern const std::string tree_object_type;

object_name compute_hash(const boost::filesystem::path& p);
object_name compute_hash(const std::string& s);

class snapshot_error : public std::runtime_error
{
	public:

	snapshot_error(const std::string& s) : std::runtime_error(s)
	{ }

	~snapshot_error() throw()
	{ }

	protected:

	std::string what_;
};

struct object_store_location
{
	fs::path contents;
	fs::path header;
};

class snapshot_dir
{
	public:

	snapshot_dir();
	snapshot_dir(const fs::path& root);
	~snapshot_dir();

	const fs::path& root() const { return root_; }
	void set_root(const fs::path& new_root);

	/// @brief Add the contents of a path to the snapshot as a blob.
	///
	/// @param type The type of this object (e.g. file_object_type).
	/// @param path The location on disk with the contents of the blob.
	///
	/// @throw snapshot_error if the blob cannot be added.
	///
	/// @return This object's name.
	object_name add_object(const object_type& type, const fs::path& path) const;

	/// @brief Add the contents of a string to the blob store.
	///
	/// @param type The type of this object (e.g. file_object_type).
	/// @param contents The contents of this object.
	///
	/// @throw snapshot_error if the blob cannot be added.
	///
	/// @return This blob's hash.
	object_name add_object(const object_type& type, const std::string& contents) const;

	/// @brief Return the path to the directory in the object store where hash may be found.
	///
	/// The path returned is relative to the root directory.
	///
	/// If the object store directory does not exist, it is created.
	///
	/// @param name
	///
	/// @return The path where the hash object should be located.
	object_store_location resolve_object(const object_name& name) const;

	object_name add_tree_from_dir(const fs::path& dir) const;

	bool object_is_valid(const object_name& name) const;
	bool object_is_valid(const object_name& name, std::string& why) const;

	protected:

	fs::path root_;

	/// @brief Resolve a relative path relative to the root and ensure all sub-directory
	/// components exist.
	///
	/// @param p
	///
	/// @throw snapshot_error if the path could not be successfully resolved.
	///
	/// @return root() / \param p.
	const fs::path resolve(const fs::path& p) const;
};

}

#endif // CS_SNAPSHOT_HPP_INCLUDED
