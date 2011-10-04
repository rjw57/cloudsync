#include <error.h>
#include <iostream>
#include <stdexcept>

#include "snapshot.hpp"

using namespace cs;

int main(int argc, char** argv)
{
	try
	{
		snapshot_dir s(fs::current_path());
		std::cout << s.add_tree_from_dir(".") << '\n';
	}
	catch (const std::exception& e)
	{
		error(1, 0, "%s", e.what());
	}

	return 0;
}
