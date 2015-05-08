from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)

# The above addresses the issues that came up in
# https://github.com/opencog/atomspace/issues/20
# Basically, the various python modules are split over multiple
# directories, including the build directories. See this:
# http://stackoverflow.com/questions/2699287/what-is-path-useful-for
# for an explanation.
