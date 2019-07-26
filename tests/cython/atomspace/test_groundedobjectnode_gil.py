import time
import unittest

from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from opencog.scheme_wrapper import scheme_eval


class Point:

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def get_x(self):
        return self.x

    def get_y(self):
        return self.x

    def move(self, x, y):
        self.x += x
        self.y += y

    def __str__(self):
        return "Point(%d, %d)" % (self.x, self.y)


class GroundedObjectNodeGilTest(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)

    def tearDown(self):
        finalize_opencog()
        del self.atomspace

    def test_call_grounded_object_call(self):
        x = 2
        y = 3
        point = Point(x, y)

        GroundedObjectNode("point", point, unwrap_args=True)

        move_x = 3
        move_y = 4
        GroundedObjectNode("x", move_x)
        GroundedObjectNode("y", move_y)

        iterations = 3
        async_calls = 5
        for _ in range(iterations):
            self.call_apply_link_async_in_schema(async_calls)
            time.sleep(0.1)

        self.assertEqual(x + move_x * iterations * async_calls, point.x)
        self.assertEqual(y + move_y * iterations * async_calls, point.y)

    def call_apply_link_async_in_schema(self, times):
        scheme_eval(self.atomspace,
                    '''
                    (use-modules
                     (opencog)
                     (opencog exec))

                    (define (async-call f n)
                     (call-with-new-thread f)
                     (if (> n 1)
                      (async-call f (- n 1))
                     ))
                    (async-call (lambda ()
                      (begin
                        (define apply-link
                            (ApplyLink
                              (MethodOfLink
                                (GroundedObjectNode "point")
                                (ConceptNode "move"))
                              (ListLink
                                (GroundedObjectNode "x")
                                (GroundedObjectNode "y"))))
                        (cog-execute! apply-link)
                        )) %d)

                    (usleep 100)
                    ''' % times)


if __name__ == '__main__':
    unittest.main()
