import doctest
import unittest
import facile

if __name__ == '__main__':
    suite = doctest.DocTestSuite(facile)
    unittest.TextTestRunner().run(suite)
