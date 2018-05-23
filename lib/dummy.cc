// This file is used for generation of proxy libraries,
// which use --no-as-needed flag to link with the actual implementations.
// The client code needs to be linked with the proxy libraries.
// That ensures, that the constructors, which register Atoms within ClassServer
// are executed properly. Otherwise the execution
// of the client code might fail in the unpredictable manner.

