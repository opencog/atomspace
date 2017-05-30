/*
 * FUNCTION:
 * Base class for SQL-backed persistent storage.
 *
 * HISTORY:
 * Copyright (c) 2016 Linas Vepstas <linasvepstas@gmail.com> except for
 * MurmurHash2 written by Austin Appleby which is separately placed into
 * the public domain.
 *
 * LICENSE:
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

#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspaceutils/TLB.h>

#include "OutgoingHash.h"

//-----------------------------------------------------------------------------
// MurmurHash2 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.

// Note - This code makes a few assumptions about how your machine behaves -

// 1. We can read a 4-byte value from any address without crashing
// 2. sizeof(int) == 4

// And it has a few limitations -

// 1. It will not work incrementally.
// 2. It will not produce the same results on little-endian and big-endian
//    machines.


//-----------------------------------------------------------------------------
// MurmurHash2, 64-bit versions, by Austin Appleby

// The same caveats as 32-bit MurmurHash2 apply here - beware of alignment 
// and endian-ness issues if used across multiple platforms.

// 64-bit hash for 64-bit platforms

#define BIG_CONSTANT(x) (x##LLU)

uint64_t MurmurHash64A ( const void * key, int len, uint64_t seed )
{
    const uint64_t m = BIG_CONSTANT(0xc6a4a7935bd1e995);
    const int r = 47;

    uint64_t h = seed ^ (len * m);

    const uint64_t * data = (const uint64_t *)key;
    const uint64_t * end = data + (len/8);

    while(data != end)
    {
        uint64_t k = *data++;

        k *= m; 
        k ^= k >> r; 
        k *= m; 

        h ^= k;
        h *= m; 
    }

    const unsigned char * data2 = (const unsigned char*)data;

    switch(len & 7)
    {
        case 7: h ^= uint64_t(data2[6]) << 48;
        case 6: h ^= uint64_t(data2[5]) << 40;
        case 5: h ^= uint64_t(data2[4]) << 32;
        case 4: h ^= uint64_t(data2[3]) << 24;
        case 3: h ^= uint64_t(data2[2]) << 16;
        case 2: h ^= uint64_t(data2[1]) << 8;
        case 1: h ^= uint64_t(data2[0]);
            h *= m;
        };

    h ^= h >> r;
    h *= m;
    h ^= h >> r;

    return h;
}

/*
 * BEGIN PORTION:
 * Copyright (c) 2016 Linas Vepstas <linasvepstas@gmail.com> except for
 * MurmurHash2 written by Austin Appleby which is separately placed into
 * the public domain.
 */

using namespace opencog;

int64_t opencog::hash_outgoing(const HandleSeq& outgoing, uint64_t seed)
{
    std::vector<UUID>   uuids;
    size_t              handle_count = outgoing.size();
    uint64_t            hash;

    // Reserve the full size.
    uuids.reserve(handle_count);

    // Now copy the UUIDs into the vector.
    int position = 0;
    for (auto handle : outgoing)
    {
        if (handle == NULL)
        {
            throw RuntimeException(TRACE_INFO, "Fatal Error: hash_outgoing - "
                    "NULL handle in outgoing set\n");
        }
        UUID uuid = TLB::addAtom(handle, TLB::INVALID_UUID);
        uuids[position++] = uuid;
    }

    // Hash the vector in place.
    hash = MurmurHash64A(uuids.data(), handle_count * sizeof(UUID), seed);
    
    // Return the hash type-casted to be compatible with PostgreSQL's BIGINT
    // which is signed.
    return (int64_t) hash;
}
