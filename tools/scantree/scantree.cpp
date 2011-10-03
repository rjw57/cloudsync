#include <error.h>
#include <deque>
#include <iterator>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

#include <cloudsync/tree.hpp>

#include "scantree.hpp"

namespace po = boost::program_options;

namespace scantree
{

int scantree::main(int argc, char** argv)
{
	scantree st;
	return st.run(argc, argv);
}

scantree::scantree()
{ }

scantree::~scantree()
{ }

int scantree::run(int argc, char** argv)
{
	po::variables_map vm;

	// build the command line options description
	po::options_description generic("Generic options");
	generic.add_options()
		("help,h", "output a brief usage description")
	;

	po::options_description hidden("Hidden options");
	hidden.add_options()
		("directory", po::value< std::vector<std::string> >(), "directory to scan")
	;

	// the different classes of options
	po::options_description cmd_line;
	cmd_line.add(generic).add(hidden);
	po::options_description visible;
	visible.add(generic);

	// positional arguments
	po::positional_options_description positional;
	positional.add("directory", -1);

	// do the parsing
	try
	{
		po::store(po::command_line_parser(argc, argv).
				options(cmd_line).
				positional(positional).
				run(), vm);
		po::notify(vm);
	}
	catch (const std::exception& e)
	{
		error(0, 0, "%s", e.what());
		return 1;
	}

	if(vm.count("help"))
	{
		std::cout << "Usage: " << argv[0] << " [options] [dir1 [dir2 ...]]\n";
		std::cout << visible << std::endl;
		return 0;
	}

	BOOST_FOREACH(const std::string& dirpath, vm["directory"].as<std::vector<std::string> >())
	{
		try
		{
			std::deque<cs::tree::entry> entries;
			cs::tree::scan_directory(dirpath, std::back_inserter(entries));

			BOOST_FOREACH(const cs::tree::entry& entry, entries)
			{
				std::cout << entry << '\n';
			}
		}
		catch (const std::exception& e)
		{
			error(1, 0, "could not scan directory '%s': %s", dirpath.c_str(), e.what());
		}
	}

	return 0;
}

}

int main(int argc, char **argv)
{
	return scantree::scantree::main(argc, argv);
}
