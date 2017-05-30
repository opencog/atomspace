/*
 * PrimitiveExample.cc
 *
 * Example code showing how declare a C++ method so that it can
 * be called from scheme.
 *
 * Copyright (C) 2009 Linas Vepstas
 */


#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/guile/SchemePrimitive.h>

using namespace opencog;

// Some example class
class MyTestClass
{
	private:
		AtomSpace *_as;
		int _id;  // some value in the instance
	public:

		MyTestClass(AtomSpace* as, int id) : _as(as), _id(id) {}

		// An example method -- accepts a handle, and wraps it
		// with a ListLink.
		Handle my_func(Handle h)
		{
			Handle hlist;
			Type t = h->getType();
			if (classserver().isA(t, NODE))
			{
				NodePtr n = NodeCast(h);
				std::string name = n->getName();
				printf("Info: my_func instance %d received the node: %s\n",
				       _id, name.c_str());
				hlist = _as->add_link(LIST_LINK, h);
			}
			else
			{
				printf("Warning: my_func instance %d called with invalid handle\n", _id);
			}
			return hlist;
		}

		Handle my_other_func(Handle h)
		{
			throw (RuntimeException(TRACE_INFO, "I threw an exception %d", _id));
			return Handle::UNDEFINED;
		}
};

int main ()
{
	// Need to access the atomspace to get it to initialize itself.
	AtomSpace* as = new AtomSpace();

	// Do this early, so that guile is initialized.
	SchemeEval* eval = new SchemeEval(as);

	printf("\nInfo: Start creating a scheme call into C++\n");

	// Create the example class, and define a scheme function,
	// named "bingo", that will call one of its methods
	MyTestClass *mtc = new MyTestClass(as, 42);
	define_scheme_primitive("bingo", &MyTestClass::my_func, mtc);

	// Now, call bingo, with a reasonable argument. Since
	// MyTestClass::my_func is expecting a handle, we better pass
	// bingo a handle.
	eval->eval("(define nnn (cog-new-node 'ConceptNode \"Hello World!\"))");
	std::string rslt = eval->eval("(bingo nnn)");
	if (eval->eval_error())
	{
		printf("Error: failed evaluation\n");
	}

	// Print the result of calling MyTestClass::my_func
	printf("Info: Result of scheme evaluation is %s", rslt.c_str());

	// Now try throwing an exception.
	define_scheme_primitive("whoops", &MyTestClass::my_other_func, mtc);

	rslt = eval->eval("(whoops nnn)");
	if (!eval->eval_error())
	{
		printf("XXX ERROR XXX: an error should have been thrown, but wasn't!\n");
	}

	// Print the result of calling MyTestClass::my_func
	printf("Info: Intentional throw gave the following output:\n%s", rslt.c_str());

	delete eval;
	printf("\nInfo: We are done, bye!\n");
	return  0;
}
