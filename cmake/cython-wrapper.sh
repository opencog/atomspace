#!/bin/bash
#
# Hack around cython insanity: newer versions of cython are not
# compatible with older versions of cython. The new version spew
# the following error:
#
#     The keyword 'nogil' should appear at the end of the function signature line. Placing it before 'except' or 'noexcept' will be disallowed in a future version of Cython.
#
# Attempting to "fix" the above results in code that does not even
# compile with older versions of cython. Meanwhile, this warning is
# spewed hundreds of times, and not just for the AtomSpace, but any
# and every project that uses the AtomSpace. So, ugly as sin, and all
# the users complain. So just silence this warning. I don't know what
# else to do.
#
# Of course, "-X warn.legacy_directives=False" doesn't work, because
# old versions of cython don't know about this.

"$@" 2>&1 | grep -v "The keyword 'nogil' should appear at the end of the function signature line"
exit ${PIPESTATUS[0]}
