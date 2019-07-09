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

    @unittest.skip("Skip until further investigation")
    def test_call_grounded_object_call(self):
        point = Point(2, 3)

        GroundedObjectNode("point", point, unwrap_args=True)
        GroundedObjectNode("x", 3)
        GroundedObjectNode("y", 4)

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
                        )) 10)

                    (usleep 500)
                    ''')

        time.sleep(0.2)

        N = 10
        self.assertEqual(2 + 3 * N, point.x)
        self.assertEqual(3 + 4 * N, point.y)


if __name__ == '__main__':
    unittest.main()
