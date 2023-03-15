/*
 * opencog/atomspace/version.h
 *
 * Copyright (C) 2015 by OpenCog Foundation
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

#ifndef _OPENCOG_ATOMSPACE_VER_H
#define _OPENCOG_ATOMSPACE_VER_H

#define ATOMSPACE_MAJOR_VERSION 5
#define ATOMSPACE_MINOR_VERSION 0
#define ATOMSPACE_MICRO_VERSION 4

#ifndef GIT_COMMIT_HASH
	#define ATOMSPACE_VERSION_STRING "5.0.4"
#else
	#define ATOMSPACE_VERSION_STRING "5.0.4-" GIT_COMMIT_HASH
#endif


#endif // _OPENCOG_ATOMSPACE_VER_H
