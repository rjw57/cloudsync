#ifndef INCLUDED_SCANTREE_HPP
#define INCLUDED_SCANTREE_HPP

#include <boost/utility.hpp>

namespace scantree
{

class scantree : boost::noncopyable
{
	public:

	static int main(int argc, char** argv);

	protected:

	scantree();
	~scantree();

	int run(int argc, char** argv);
};

}

#endif // INCLUDED_SCANTREE_HPP
