#ifndef _OPENCOG_HASH_H
#define _OPENCOG_HASH_H

#include <opencog/atoms/base/Handle.h>

namespace opencog {

// Fowler–Noll–Vo hash function
// Parameters are taken from author's page
// http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-1a
const size_t FNV_32_PRIME = 0x01000193;
const size_t FNV_32_OFFSET = 0x811c9dc5;
const uint64_t FNV_64_PRIME = 0x100000001b3ULL;
const uint64_t FNV_64_OFFSET = 0xcbf29ce484222325ULL;

template <unsigned n>
constexpr uint64_t get_fvna_prime(){
	return FNV_32_PRIME;
}

template <>
constexpr uint64_t get_fvna_prime<8>(){
	return FNV_64_PRIME;
}

template <unsigned n>
constexpr uint64_t get_fvna_offset(){
	return FNV_32_OFFSET;
}

template <>
constexpr uint64_t get_fvna_offset<8>(){
	return FNV_64_OFFSET;
}

template<typename T>
typename std::enable_if<sizeof(T) <= sizeof(ContentHash), ContentHash>::type fnv1a_hash (ContentHash & hval, T buf_t)
{
	hval ^= (ContentHash) (buf_t);
	hval *= (ContentHash) get_fvna_prime<sizeof(ContentHash)>();
	return hval;
}

template<typename T, typename ChunkType=uint32_t>
typename std::enable_if<sizeof(ContentHash) < sizeof(T), ContentHash>::type fnv1a_hash (ContentHash & hval, T buf_t)
{
	constexpr const size_t size = sizeof(buf_t);
	static_assert(sizeof(ChunkType) <= sizeof(T), "sizeof(ChunkType) <= sizeof(T)");
	static_assert(sizeof(T) % sizeof(ChunkType) == 0, "sizeof(T) is not divisible by "
							  "sizeof(ChunkType)");
	static_assert(sizeof(T) <= 65535, "the implementation can't handle more than 65535 bytes");
	const ChunkType * buf = (const ChunkType *)&buf_t;
	unsigned short count = 0;
	constexpr const unsigned short num_iter = (size / sizeof(ChunkType));
	while (count < num_iter)
	{
		hval ^= (ContentHash) (*(buf+count));
		hval *= (ContentHash) get_fvna_prime<sizeof(ContentHash)>();
		count ++;
	}
	return hval;
}

} // namespace opencog

#endif // _OPENCOG_HASH_H
