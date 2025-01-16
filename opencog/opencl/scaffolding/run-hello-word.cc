/**
 * OpenCL scaffolding.
 *
 * This provides minimalistic scaffolding to allow OpenCL experiments
 * to take place.
 *
 * Copyright (c) 2025 Linas Vepstas
 */

#define CL_HPP_MINIMUM_OPENCL_VERSION 200
#define CL_HPP_TARGET_OPENCL_VERSION 300
#define CL_HPP_ENABLE_EXCEPTIONS

#include <CL/opencl.hpp>
#include <iostream>
#include <fstream>

/// Build kernel froum source file, return context.
void build_kernel(cl::Device ocldev, const char* srcfile,
                  cl::Context& ctxt, cl::Program& prog)
{
	// Copy in source code. Must be a better way!?
	fprintf(stderr, "Reading sourcefile %s\n", srcfile);
	std::ifstream srcfm(srcfile);
	// std::stringstream buffer;
	// buffer << srcfm.rdbuf();
	std::string src(std::istreambuf_iterator<char>(srcfm),
		(std::istreambuf_iterator<char>()));

	if (0 == src.size())
	{
		fprintf(stderr, "Error: Could not find file %s\n", srcfile);
		exit(1);
	}

	cl::Program::Sources sources;
	sources.push_back(src);

	cl::Context context(ocldev);
	cl::Program program(context, sources);

	// Compile
	fprintf(stderr, "Compiling sourcefile %s\n", srcfile);
	try
	{
		// Specifying flags causes exception.
		// program.build("-cl-std=CL1.2");
		program.build("");
	}
	catch (const cl::Error& e)
	{
		printf("Compile failed! %s\n", e.what());
		printf("Log >>%s<<\n",
			program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(ocldev).c_str());
		exit(1);
	}

	ctxt = context;
	prog = program;
}

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

void report_hardware(void);

cl::Device find_device(const char* platsubstr, const char* devsubstr);
int main(int argc, char* argv[])
{
	report_hardware();

	cl::Device ocldev = find_device("", "AMD");
	std::string dname = ocldev.getInfo<CL_DEVICE_NAME>();
	printf("Will use: %s\n", dname.c_str());

	cl::Context ctxt;
	cl::Program prog;
	build_kernel(ocldev, "hello.cl", ctxt, prog);
	run_hello(ocldev, ctxt, prog);
}
