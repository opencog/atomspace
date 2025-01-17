
#if defined(cl_khr_fp64)
#  pragma OPENCL EXTENSION cl_khr_fp64: enable
#elif defined(cl_amd_fp64)
#  pragma OPENCL EXTENSION cl_amd_fp64: enable
#else
#  error double precision is not supported
#endif

kernel void vec_mult(global double *prod,
                     global const double *a,
                     global const double *b,
                     const unsigned long sz)
{
	size_t i = get_global_id(0);

	// Scalar product of two vectors.
	if (i < sz)
		prod[i] = a[i] * b[i];
}
