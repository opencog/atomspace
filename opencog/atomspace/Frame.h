/*
 * opencog/atomspace/Frame.h
 *
 * Copyright (c) 2022 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_FRAME_H
#define _OPENCOG_FRAME_H

#include <opencog/atoms/base/Atom.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * This class provides infrastructure for frames.  These are atoms
 * with both a name and an outgoing set. Experimental.
 */
class Frame : public Atom
{
private:
	void init();

protected:
	HandleSeq _outgoing;
	std::string _name;
	virtual void install();
	virtual void remove();

public:
	/**
	 * Constructor and destructor for this class.
	 */
	Frame(Type t)
		: Atom(t)
	{
		init();
	}
	Frame(Type t, const HandleSeq oset)
		: Atom(t), _outgoing(std::move(oset))
	{
		init();
	}
	virtual ~Frame();

	bool is_atom(void) const { return true; }
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FRAME_H
