#include <ctime>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem/fstream.hpp>
#include <hashstream/hashstream.hpp>
#include <cloudsync/tree.hpp>

namespace cs { namespace tree {

namespace fs = boost::filesystem;

const char* scan_error::what() const throw()
{
	const std::string* s(boost::get_error_info<scan_error_info>(*this));

	if(NULL != s) {
		return s->c_str();
	} else {
		return "unknown scan error";
	}
}

entry::entry()
{ }

entry::entry(const fs::directory_entry& de)
	: filename(de.path().filename().generic_string())
	, hash("")
	, file_size(0)
	, last_modified_time(fs::last_write_time(de.path()))
{
	if(de.status().type() == fs::regular_file)
	{
		hash = hashstream::hex_digest(hashstream::SHA256, fs::ifstream(de.path()));
		file_size = fs::file_size(de.path());
	}
}

std::ostream& operator << (std::ostream& os, const entry& e)
{
	boost::posix_time::ptime pt = boost::posix_time::from_time_t(e.last_modified_time);
	os << std::setw(64) << e.hash << ' ' << std::setw(8) << e.file_size
		<< ' ' << pt << ' ' << e.filename;

	return os;
}

} }
