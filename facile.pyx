# distutils: language = c
# -*- coding: utf-8 -*-

from interface cimport *

cimport cpython
import numpy as np
cimport numpy as cnp

# Initialize OCaml
init()

class Heuristic:
    No = 0
    Min_size = 1
    Min_value = 2
    Min_min = 3


cdef class Variable(object):

    cdef long mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__ (self, value):
        if value == 0:  # May happen on Cstr.boolean
            raise TypeError("Non reifiable constraint")
        self.mlvalue = value

    def __repr__(self):
        cdef int vmin, vmax
        val_minmax(self.mlvalue, &vmin, &vmax)
        if (val_isbound(self.mlvalue) == 1) :
            return " = %d" % (vmin)
        else:
            return "%s in [%d,%d]" % (<bytes> val_name(self.mlvalue), vmin, vmax)

    def __getval(self):
        return self.mlvalue

    def value(self):
        """ Return the numerical value of the variable.
        Return None if no solution has been found. """
        cdef int vmin, vmax
        val_minmax(self.mlvalue, &vmin, &vmax)
        if (val_isbound(self.mlvalue)==1):
            return vmin
        else:
            return None

    def in_interval(self, int inf, int sup):
        cdef long res
        res = interval_ismember(self.mlvalue, inf, sup)
        return Variable(res)

    def __richcmp__(self, value, op):
    # < 0 # <= 1 # == 2 # != 3 # > 4 # >= 5
        if op == 0:
           return self.__lt(value)
        if op == 1:
            return self.__le(value)
        if op == 2:
            return self.__eq(value)
        if op == 3:
            return self.__ne(value)
        if op == 4:
            return self.__gt(value)
        if op == 5:
            return self.__ge(value)
        return None

    def __lt(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_lt(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c)
        if isinstance(fd, Arith):
            c = cstr_lt(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_lt(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_lt(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __le(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_le(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c)
        if isinstance(fd, Arith):
            c = cstr_le(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_le(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_le(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __eq(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_eq(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c)
        if isinstance(fd, Arith):
            c = cstr_eq(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_eq(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_eq(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __ne(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_ne(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c)
        if isinstance(fd, Arith):
            c = cstr_ne(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_ne(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_ne(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __gt(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_gt(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c)
        if isinstance(fd, Arith):
            c = cstr_gt(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_gt(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_gt(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __ge(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_ge(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c)
        if isinstance(fd, Arith):
            c = cstr_ge(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_ge(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_ge(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __add__(a, b):
        if isinstance(a, int):
            c = arith_add(i2e(a), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, Arith):
            c = arith_add(fd2e(a.__getval()), b.__getval())
            return Arith(c)
        if isinstance(b, Variable):
            c = arith_add(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, int):
            c = arith_add(fd2e(a.__getval()), i2e(b))
            return Arith(c)
        if isinstance(b, Cstr):
            return a + Variable(cstr_boolean(b.__getval()))
        raise TypeError("Expressions of incompatible types")

    def __sub__(a, b):
        if isinstance(a, int):
            c = arith_sub(i2e(a), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, Arith):
            c = arith_sub(fd2e(a.__getval()), b.__getval())
            return Arith(c)
        if isinstance(b, Variable):
            c = arith_sub(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, int):
            c = arith_sub(fd2e(a.__getval()), i2e(b))
            return Arith(c)
        raise TypeError("Expressions of incompatible types")

    def __mul__(a, b):
        if isinstance(a, int):
            c = arith_mul(i2e(a), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, Arith):
            c = arith_mul(fd2e(a.__getval()), b.__getval())
            return Arith(c)
        if isinstance(b, Variable):
            c = arith_mul(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, int):
            c = arith_mul(fd2e(a.__getval()), i2e(b))
            return Arith(c)
        raise TypeError("Expressions of incompatible types")

    def __pos__(a):
        return a

    def __neg__(a):
        return 0 - a

    def __abs__(a):
        c = arith_abs(fd2e(a.__getval()))
        return Arith(c)


cdef class Arith(object):

    cdef long mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value):
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def variable(self):
        return Variable(e2fd(self.mlvalue))

    def __repr__(self):
        return self.variable().__repr__()

    def __richcmp__(self, value, op):
    # < 0 # <= 1 # == 2 # != 3 # > 4 # >= 5
        if op == 0:
            return self.__lt(value)
        if op == 1:
            return self.__le(value)
        if op == 2:
            return self.__eq(value)
        if op == 3:
            return self.__ne(value)
        if op == 4:
            return self.__gt(value)
        if op == 5:
            return self.__ge(value)
        return None

    def __lt(self, value):
        if isinstance(value, Cstr):
            c = cstr_lt(self.__getval(), value.__abs__().__getval())
            return Cstr(c)
        if isinstance(value, Arith):
            c = cstr_lt(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_lt(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_lt(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __le(self, value):
        if isinstance(value, Cstr):
            c = cstr_le(self.__getval(), value.__abs__().__getval())
            return Cstr(c)
        if isinstance(value, Arith):
            c = cstr_le(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_le(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_le(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __eq(self, value):
        if isinstance(value, Cstr):
            c = cstr_eq(self.__getval(), value.__abs__().__getval())
            return Cstr(c)
        if isinstance(value, Arith):
            c = cstr_eq(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_eq(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_eq(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __ne(self, value):
        if isinstance(value, Cstr):
            c = cstr_ne(self.__getval(), value.__abs__().__getval())
            return Cstr(c)
        if isinstance(value, Arith):
            c = cstr_ne(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_ne(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_ne(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __gt(self, value):
        if isinstance(value, Cstr):
            c = cstr_gt(self.__getval(), value.__abs__().__getval())
            return Cstr(c)
        if isinstance(value, Arith):
            c = cstr_gt(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_gt(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_gt(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __ge(self, value):
        if isinstance(value, Cstr):
            c = cstr_ge(self.__getval(), value.__abs__().__getval())
            return Cstr(c)
        if isinstance(value, Arith):
            c = cstr_ge(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_ge(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_ge(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError("Expressions of incompatible types")

    def __add__(a, b):
        if isinstance(a, int):
            c = arith_add(i2e(a), b.__getval())
            return Arith(c)
        if isinstance(b, Arith):
            c = arith_add(a.__getval(), b.__getval())
            return Arith(c)
        if isinstance(b, Variable):
            c = arith_add(a.__getval(), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, int):
            c = arith_add(a.__getval(), i2e(b))
            return Arith(c)
        if isinstance(b, Cstr):
            return a + Variable(cstr_boolean(b.__getval()))
        raise TypeError("Expressions of incompatible types")

    def __sub__(a, b):
        if isinstance(a, int):
            c = arith_sub(i2e(a), b.__getval())
            return Arith(c)
        if isinstance(b, Arith):
            c = arith_sub(a.__getval(), b.__getval())
            return Arith(c)
        if isinstance(b, Variable):
            c = arith_sub(a.__getval(), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, int):
            c = arith_sub(a.__getval(), i2e(b))
            return Arith(c)
        raise TypeError("Expressions of incompatible types")

    def __mul__(a, b):
        if isinstance(a, int):
            c = arith_mul(i2e(a), b.__getval())
            return Arith(c)
        if isinstance(b, Arith):
            c = arith_mul(a.__getval(), b.__getval())
            return Arith(c)
        if isinstance(b, Variable):
            c = arith_mul(a.__getval(), fd2e(b.__getval()))
            return Arith(c)
        if isinstance(b, int):
            c = arith_mul(a.__getval(), i2e(b))
            return Arith(c)
        raise TypeError("Expressions of incompatible types")

    def __pos__(a):
        return a

    def __neg__(a):
        return 0 - a

    def __abs__(a):
        c = arith_abs(a.__getval())
        return Arith(c)

cdef class Cstr(object):

    cdef long mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value):
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def __repr__(self):
        return (<bytes> cstr_name(self.__getval()))

    def __richcmp__(self, value, op):
    # < 0 # <= 1 # == 2 # != 3 # > 4 # >= 5
        if op == 0:
            return self.__abs__().__lt(value)
        if op == 1:
            return self.__abs__().__le(value)
        if op == 2:
            return self.__abs__().__eq(value)
        if op == 3:
            return self.__abs__().__ne(value)
        if op == 4:
            return self.__abs__().__gt(value)
        if op == 5:
            return self.__abs__().__ge(value)
        return None

    def __and__(Cstr c1, Cstr c2):
        return Cstr(cstr_and(c1.__getval(), c2.__getval()))

    def __or__(Cstr c1, Cstr c2):
        return Cstr(cstr_or(c1.__getval(), c2.__getval()))

    def __add__(c1, c2):
        if isinstance(c2, Cstr):
            return c1 + Variable(cstr_boolean(c2.__getval()))
        if isinstance(c1, Cstr):
            return Variable(cstr_boolean(c1.__getval())) + c2
        raise TypeError("Expressions of incompatible types")

    def __sub__(c1, c2):
        if isinstance(c2, Cstr):
            return c1 - Variable(cstr_boolean(c2.__getval()))
        if isinstance(c1, Cstr):
            return Variable(cstr_boolean(c1.__getval())) - c2
        raise TypeError("Expressions of incompatible types")

    def __mul__(c1, c2):
        if isinstance(c2, Cstr):
            return c1 * Variable(cstr_boolean(c2.__getval()))
        if isinstance(c1, Cstr):
            return Variable(cstr_boolean(c1.__getval())) * c2
        raise TypeError("Expressions of incompatible types")

    def __pos__(a):
        return Variable(cstr_boolean(a.__getval()))

    def __neg__(a):
        return 0 - Variable(cstr_boolean(a.__getval()))

    def __abs__(a):
        return Variable(cstr_boolean(a.__getval()))

    def post(self):
        if cstr_post(self.__getval()) == 1:
            raise ValueError("The problem is overconstrained")

    # For Python 2.x
    def __nonzero__(self):
        print (array.__doc__)
        raise SyntaxError("This operation is not allowed. Check facile.array()")

    # For Python 3.x
    def __bool__(self):
        print (array.__doc__)
        raise SyntaxError("This operation is not allowed. Check facile.array()")

cdef class Array(object):
    """
    This structure facilitates the manipulation of arrays of variables and/or
    expressions.

    - It can be indexed by variables, expressions or integers.
    - You can compare two arrays of the same length and get an iterable with
      the corresponding constraints.
    - You can access its max() or min().
    - You can access its sorted version: sort()
    - You can apply a Global Cardinality Constraint: gcc()

    Use `array(iterable)` to create such a structure.
    """

    cdef long mlvalue
    cdef long length

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value, length):
        self.mlvalue = value
        self.length = length

    def __getval(self):
        return self.mlvalue

    def __len__(self):
        return self.length

    def __iter__(self):
        values = np.empty(self.length, dtype=long)
        pt_vars = <long*> cnp.PyArray_DATA(values)
        fdarray_read(self.mlvalue, pt_vars)
        for i in range(self.length):
            yield Variable(values[i])

    def __repr__(self):
        array = [x for x in self]
        return array.__repr__()

    def __getitem__(self, key):
        """ Return self[key].
        key can be a variable, an expression or an integer.
        """
        cdef long value
        if isinstance(key, Variable):
            value = fdarray_get(self.mlvalue, key.__getval())
        elif isinstance(key, Arith):
            value = fdarray_get(self.mlvalue, e2fd(key.__getval()))
        elif isinstance(key, int):
            value = fdarray_get(self.mlvalue, e2fd(i2e(key)))
        else:
            raise TypeError("Index should be integer, variable or expression")
        if value == 0:
            raise IndexError("Index out of bounds")
        return Variable(value)

    def __richcmp__(a, b, op):
        # < 0 # <= 1 # == 2 # != 3 # > 4 # >= 5
        if not isinstance(a, Array) or not isinstance(b, Array):
            raise SyntaxError("Arrays can only be compared with arrays")
        if len(a) != len(b):
            raise IndexError("The two arrays must be of same length")
        if op == 0:
            return [x < y for (x, y) in zip(a, b)]
        if op == 1:
            return [x <= y for (x, y) in zip(a, b)]
        if op == 2:
            return [x == y for (x, y) in zip(a, b)]
        if op == 3:
            return [x != y for (x, y) in zip(a, b)]
        if op == 4:
            return [x > y for (x, y) in zip(a, b)]
        if op == 5:
            return [x >= y for (x, y) in zip(a, b)]

    def value(self):
        """Apply value() to all elements in Array."""
        return [x.value() for x in self]

    def max(self):
        """Return a new variable that is the max of all expressions in self."""
        cdef long value
        value = fdarray_max(self.mlvalue)
        if value == 0:
            raise IndexError("Empty list")
        return Variable(value)

    def min(self):
        """Return a new variable that is the min of all expressions in self."""
        cdef long value
        value = fdarray_min(self.mlvalue)
        if value == 0:
            raise IndexError("Empty list")
        return Variable(value)

    def sort(self):
        """Return an array of variables sorted in increasing order."""
        cdef long value
        value = sorting_sort(self.mlvalue)
        return Array(value, self.length)

    def gcc(self, distribution):
        """Return a Global Cardinality Constraint w.r.t distribution.

        For each pair (c, v) in distribution, c variables in the array will be
        instantiated to v.
        Also, the sum of the cardinals will be equal to the number of variables
        in the array (the corresponding constraint is automatically posted).
        >>> a = array([facile.variable(0,3) for i in range(5)])
        >>> constraint(a.gcc([(1, 3), (1, 2), (3, 1)]))

        Note that this constraint is not reifiable.

        If you need a simpler cardinality constraint, you can write:
        >>> c1 = sum([p == value for p in array]) == cardinal
        """

        cdef long value
        cdef long l = len(distribution)
        cdef long card = 0

        cards = []
        values = []
        for (c, v) in distribution:
            if isinstance(c, int):
                card = i2e(c)
            if isinstance(c, Arith):
                card = c.__getval()
            if isinstance(c, Variable):
                card = fd2e(c.__getval())
            if card == 0:
                raise TypeError(
                        "Cardinals must be integers, variables or expressions")
            if not isinstance(v, int):
                raise TypeError("Values must be integers")
            cards.append(card)
            values.append(v)
            card = 0

        np_cards = np.array(cards)
        pt_cards = cnp.PyArray_DATA(np_cards)
        np_values = np.array(values)
        pt_values = cnp.PyArray_DATA(np_values)

        value = gcc_cstr(self.mlvalue, <long*> pt_cards, <long*> pt_values, l)
        return Cstr(value)

def solve(variables, backtrack=False, heuristic=Heuristic.No):
    """
    solve(variables, backtrack=False, heuristic=Heuristic.No)

    The `solve` function solves the problem defined by all posted constraints
    on variables passed in parameter.

    It returns True if the problem has a solution and False otherwise.

    If `backtrack` is set to True, it returns two arguments: the first one is
    the boolean introduced above, the second is the number of backtracks made
    until the first solution was hit.

    The last `heuristic` argument let you choose between four strategies for
    selecting the next variable to explore:
    - by default, `Heuristic.No` is chosen;
    - `Heuristic.Min_size` chooses the variable with the smallest domain;
    - `Heuristic.Min_value` chooses the variable with a minimal smallest
    value in its domain;
    - `Heuristic.Min_min` combines the hereabove strategies.

    The `solve` function raises TypeError if `variables` is not iterable.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)
    >>> solve([a, b])
    True
    >>> a.value(), b.value()
    0, 1
    """
    cdef long length
    cdef long bt
    if cpython.PySequence_Check(variables):
        variables = [x for x in variables] ## quickfix for segfault with Array
        for x in variables:
            assert isinstance(x, Variable), "All arguments must be variables"
        npvars = np.array([v.__getval() for v in variables])
        pt_vars = cnp.PyArray_DATA(npvars)
        length = len(variables)
        if length < 1:
            return TypeError("The argument list must be non empty")
        if (not backtrack):
            return goals_array_solve(<long*> pt_vars, length, heuristic) == 1
        else:
            res = goals_array_solve_bt(<long*> pt_vars, length, heuristic, &bt)
            return (res, bt)
    raise TypeError("The argument must be iterable")

def solve_all(variables):
    """
    solve_all(variables)

    The `solve_all` function solves the problem defined by all posted
    constraints on variables passed in parameter.

    It returns all possible solutions to the problem.

    The `solve_all` function raises TypeError if `variables` is not
    iterable.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)
    >>> solve_all([a, b])
    [[0, 1], [1, 0]]
    """
    cdef long length
    cdef long res
    cdef long* pt_res_i
    if cpython.PySequence_Check(variables):
        variables = [x for x in variables] ## quickfix for segfault with Array
        npvars = np.array([v.__getval() for v in variables])
        pt_vars = cnp.PyArray_DATA(npvars)
        length = len(variables)
        if length < 1:
            return TypeError("The argument list must be non empty")
        res = goals_array_solve_all(<long*> pt_vars, length)
        sols = []
        res_np = np.empty(length, dtype=int)
        pt_res_np = <long*> cnp.PyArray_DATA(res_np)
        res = parse_array(res, pt_res_np)
        while res != 0:
            # Avoid returning np.int64 type objects
            sols.insert(0, [int(x) for x in res_np])
            res = parse_array(res, pt_res_np)
        return sols
    raise TypeError("The argument must be iterable")

def minimize(variables, expr):
    """
    minimize(variables, expression)

    The `minimize` function solves the problem defined by all posted
    constraints on variables passed in parameter, and minimizes the
    expression passed in parameter.

    It returns an empty list if the problem has no solution, and a pair
    `(optimal, values)` with `value`s appearing in the same order as
    `variables`.

    The `minimize` function raises TypeError if `variables` is not
    iterable or if expression is not valid.

    >>> a = variable(0, 10)
    >>> b = variable(0, 10)
    >>> constraint(a + b == 10)
    >>> minimize([a, b], a*a + b*b)
    (55, array([5, 5]))
    """
    cdef long length
    cdef long optimal
    if isinstance(expr, Variable):
        expr = Arith(fd2e(expr.__getval()))
    if not isinstance(expr, Arith):
        raise SyntaxError
    if cpython.PySequence_Check(variables):
        variables = [x for x in variables] ## quickfix for segfault with Array
        npvars = np.array([v.__getval() for v in variables])
        pt_vars = cnp.PyArray_DATA(npvars)
        length = len(variables)
        if length < 1:
            return TypeError("The argument list must be non empty")
        sol = np.zeros(length, long)
        pt_sol = cnp.PyArray_DATA(sol)
        if goals_minimize(<long*> pt_vars, length, expr.__getval(),
                          <long*> pt_sol, &optimal) == 1 :
            return (optimal, sol)
        else:
            return []
    raise TypeError("The argument must be iterable")


def constraint(cstr):
    """
    constraint(cstr)

    The `constraint` function defines a constraint and posts it to the
    solver. The constraint can be expressed in an intuitive manner, based
    on expressions on variables.

    `constraint` raises `TypeError` if the parameter is not a constraint.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)
    """

    if not isinstance(cstr, Cstr):
        raise TypeError("The argument must be a (non-reified) constraint")
    cstr.post()


def alldifferent(variables):
    """
    alldifferent(variables)

    The `alldifferent` function defines a global constraint to be later posted
    to the solver.

    `alldifferent` raises a TypeError if `variables` is not iterable and if
    variables does not contain more than two variables.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> alldifferent([a, b])
    """

    cdef long length
    cdef long* pt_vars
    if cpython.PySequence_Check(variables):
        variables = [x for x in variables] ## quickfix for segfault with Array
        length = len(variables)
        if length < 2:
            raise TypeError("The argument list must be non empty")
        npvars = np.empty(length, dtype=long)
        for i in range(length):
            if isinstance(variables[i], Variable):
                npvars[i] = variables[i].__getval()
            elif isinstance(variables[i], Arith):
                npvars[i] = e2fd(variables[i].__getval())
            else:
                raise TypeError("Arguments must be variables or expressions")
        pt_vars = <long*> cnp.PyArray_DATA(npvars)
        length = len(variables)
        return Cstr(cstr_alldiff(pt_vars, length))
    raise TypeError("The argument must be iterable")

def variable(a, b):
    """
    variable(min, max)

    The `variable` function creates a variable on a discrete interval, with
    min/max as bounds.

    >>> variable(0, 2)
    _0 in [0,2]
    """
    cdef long value = val_interval(a, b)
    return Variable(value)

def array(variables):
    """
    array(iterable)

    This structure facilitates the manipulation of arrays of variables and/or
    expressions.

    - It can be indexed by variables, expressions or integers.
    - You can compare two arrays of the same length and get an iterable with
      the corresponding constraints.
    - You can access its max() or min().
    - You can access its sorted version: sort()
    - You can apply a Global Cardinality Constraint: gcc()

    >>> a = array([variable(0, 1), variable(0, 3), variable(0, 5)])
    """
    cdef long length
    cdef long* pt_vars
    cdef long value
    cdef Variable v
    if cpython.PySequence_Check(variables):
        length = len(variables)
        if length < 1:
            raise TypeError("The argument list must be non-empty")
        npvars = np.empty(length, dtype=long)
        for i in range(length):
            if isinstance(variables[i], Variable):
                npvars[i] = variables[i].__getval()
            elif isinstance(variables[i], Arith):
                npvars[i] = e2fd(variables[i].__getval())
            elif isinstance(variables[i], int):
                npvars[i] = e2fd(i2e(variables[i]))
            else:
                raise TypeError("The arguments must be variables or expressions")
        pt_vars = <long*> cnp.PyArray_DATA(npvars)
        value = fdarray_create(pt_vars, length)
        return Array(value, length)
    raise TypeError("The argument must be iterable")

