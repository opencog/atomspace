/**
 * scaffolding.h
 * OpenCL scaffolding.
 *
 * This provides minimalistic scaffolding to allow OpenCL experiments
 * to take place.
 *
 * Copyright (c) 2025 Linas Vepstas
 */

#ifndef __ATOMESE_OPENCL_SCAFFOLDING_H__
#define __ATOMESE_OPENCL_SCAFFOLDING_H__

#define CL_HPP_MINIMUM_OPENCL_VERSION 200
#define CL_HPP_TARGET_OPENCL_VERSION 300
#define CL_HPP_ENABLE_EXCEPTIONS

#include <CL/opencl.hpp>

/// Print rudimentary report of available OpenCL hardware.
void report_hardware(void);

/// Return the first device that has platsubstr and devsubstr as
/// substrings in the platform and device name.
cl::Device find_device(const char* platsubstr, const char* devsubstr);

/// Build kernel froum source file, return context.
void build_kernel(cl::Device ocldev, const char* srcfile,
                  cl::Context& ctxt, cl::Program& prog);

#endif /* __ATOMESE_OPENCL_SCAFFOLDING_H__ */
