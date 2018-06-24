#ifndef _OPENCOG_HASH_H
#define _OPENCOG_HASH_H

#include <opencog/atoms/base/Handle.h>

namespace opencog {

/* ================================================================= */

// Fowler–Noll–Vo hash function
// Parameters are taken from author's page
// http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1a
const size_t FNV_32_PRIME = 0x01000193;
const size_t FNV_32_OFFSET = 0x811c9dc5;
const size_t FNV_64_PRIME = 0x100000001b3;
const size_t FNV_64_OFFSET = 0xcbf29ce484222325;

template <unsigned n>
constexpr size_t get_fvna_prime(){
	return FNV_32_PRIME;
}

template <>
constexpr size_t get_fvna_prime<8>(){
	return FNV_64_PRIME;
}

template <unsigned n>
constexpr size_t get_fvna_offset(){
	return FNV_32_OFFSET;
}

template <>
constexpr size_t get_fvna_offset<8>(){
	return FNV_64_OFFSET;
}

template<typename T>
ContentHash fnv1a_hash (ContentHash & hval, T buf_t)
{
	size_t size = sizeof(buf_t);
	const char * buf = (const char *)&buf_t;
	size_t count = 0;
	while (count < size)
	{
		hval ^= (ContentHash) (*(buf+count));
		hval *= (ContentHash) get_fvna_prime<sizeof(ContentHash)>();
		count ++;
	}
	return hval;
}

} // namespace opencog

#endif // _OPENCOG_HASH_H
