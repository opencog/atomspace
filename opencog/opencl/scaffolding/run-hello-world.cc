/**
 * OpenCL scaffolding.
 *
 * This provides minimalistic scaffolding to allow OpenCL experiments
 * to take place.
 *
 * Copyright (c) 2025 Linas Vepstas
 */

#include "scaffolding.h"

void run_hello(cl::Device ocldev, cl::Context context, cl::Program program)
{
	// Set up I/O
	char buf[256];

	// CL_MEM_USE_HOST_PTR
	cl::Buffer memBuf(context,
		CL_MEM_WRITE_ONLY | CL_MEM_HOST_READ_ONLY,
		sizeof(buf));

	int err;
	cl::Kernel kernel(program, "HelloWorld", &err);
	kernel.setArg(0, memBuf);

	// Launch
	cl::CommandQueue queue(context, ocldev);

	cl::Event event_handler;
	queue.enqueueNDRangeKernel(kernel,
		cl::NullRange,
		cl::NDRange(sizeof(buf)),
		cl::NullRange,
		nullptr, &event_handler);

	event_handler.wait();
	fprintf(stderr, "Done waiting on exec\n");

	queue.enqueueReadBuffer(memBuf, CL_TRUE, 0, sizeof(buf), buf,
		nullptr, &event_handler);
	event_handler.wait();
	fprintf(stderr, "Done waiting on result read\n");

	printf("Get result >>%s<<\n", buf);
}

int main(int argc, char* argv[])
{
	cl::Device ocldev = find_device("", "AMD");
	std::string dname = ocldev.getInfo<CL_DEVICE_NAME>();
	printf("Will use: %s\n", dname.c_str());

	cl::Context ctxt;
	cl::Program prog;
	build_kernel(ocldev, "hello.cl", ctxt, prog);
	run_hello(ocldev, ctxt, prog);
}
