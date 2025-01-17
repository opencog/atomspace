/**
 * OpenCL float-point math demo.
 *
 * Simple demo of performing float point math on GPU hardware.
 *
 * Copyright (c) 2025 Linas Vepstas
 */

#include "scaffolding.h"

// Declare two floating point vectors and multiply them together.
// Wait for results, and print them.
void run_vec_mult(cl::Device ocldev, cl::Context context, cl::Program program)
{
	size_t vec_dim = 64;
	std::vector<double> a(vec_dim);
	std::vector<double> b(vec_dim);
	std::vector<double> prod(vec_dim);

	// Product will be triangle numbers.
	for (size_t i=0; i<vec_dim; i++)
	{
		a[i] = (double) i;
		b[i] = 0.5 * (double) i+1;
		prod[i] = 0.0;
	}

	size_t vec_bytes = vec_dim * sizeof(double);

	// Buffers holding data that will go to the GPU's
	cl::Buffer veca(context,
		CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, vec_bytes, a.data());

	cl::Buffer vecb(context,
		CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, vec_bytes, b.data());

	cl::Buffer vecprod(context,
		CL_MEM_READ_WRITE, vec_bytes);

	// The program to run on the GPU, and the arguments it takes.
	int err;
	cl::Kernel kernel(program, "vec_mult", &err);
	kernel.setArg(0, vecprod);
	kernel.setArg(1, veca);
	kernel.setArg(2, vecb);
	kernel.setArg(3, vec_dim);

	// Launch
	cl::CommandQueue queue(context, ocldev);

	cl::Event event_handler;
	queue.enqueueNDRangeKernel(kernel,
		cl::NullRange,
		cl::NDRange(vec_dim),
		cl::NullRange,
		nullptr, &event_handler);

	event_handler.wait();
	fprintf(stderr, "Done waiting on exec\n");

	queue.enqueueReadBuffer(vecprod, CL_TRUE, 0, vec_bytes, prod.data(),
		nullptr, &event_handler);
	event_handler.wait();
	fprintf(stderr, "Done waiting on result read\n");

	printf("The triangle numbers are:\n");
	for (size_t i=0; i<vec_dim; i++)
	{
		printf("%ld * %ld / 2 = %f\n", i, i+1, prod[i]);
	}
}

// Run code on the GPU's.
int main(int argc, char* argv[])
{
	cl::Device ocldev = find_device("", "AMD");
	std::string dname = ocldev.getInfo<CL_DEVICE_NAME>();
	printf("Will use: %s\n", dname.c_str());

	cl::Context ctxt;
	cl::Program prog;
	build_kernel(ocldev, "vec-mult.cl", ctxt, prog);
	run_vec_mult(ocldev, ctxt, prog);
}
