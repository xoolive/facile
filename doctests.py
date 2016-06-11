import doctest
import unittest
import facile

suite = doctest.DocTestSuite(facile)
unittest.TextTestRunner().run(suite)
