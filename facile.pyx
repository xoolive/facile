# distutils: language = c
# cython: embedsignature=True

"""Facile stands for Functional Constraint Library.

It uses a solver written in OCaml to find solutions to problems expressed as
Constraint Programming problems.

Example: Find two variables a and b taking values on {0, 1} so that their
values are different.
>>> a = variable([0, 1])
>>> b = variable([0, 1])
>>> constraint(a != b)
>>> assert solve([a, b])
>>> a.value(), b.value()
(0, 1)

Some global constraints, like alldifferent, are also implemented.
>>> a = variable(range(3))
>>> b = variable(range(3))
>>> c = variable(range(3))
>>> constraint(alldifferent([a, b, c]))
>>> constraint(a + b <= 2 * c)
>>> assert solve([a, b, c])
>>> a.value(), b.value(), c.value()
(0, 1, 2)

See also (definition):
    - alldifferent
    - array
    - constraint
    - variable

See also (resolution):
    - minimize
    - solve
    - solve_all

"""

import array
import heapq
import types
import numbers
import reprlib
import itertools
import collections

cimport cpython

from interface cimport *
from cpython cimport array

# Initialize OCaml
init()


class gc_list(object):

    def __init__(self, h):
        self._h = h
        # Starting from 0 seems confusing sometimes when debugging
        self._c = iter(itertools.count(1))

    def push(self, p):
        heapq.heappush(self._h, p)

    def pop(self):
        """Return the smallest number in h or a bigger number"""
        try:
            return heapq.heappop(self._h)
        except IndexError:
            return next(self._c)


class callback(object):

    h = gc_list([])

    def __init__(self, f):
        self.f = f
        self.id = self.h.pop()
        if self.id >= 1024:
            raise RuntimeError("Too many callbacks built and not released")
        # minimalistic @functools.wraps
        self.__doc__ = f.__doc__

    def __call__(self, *args, **kwargs):
        return self.f(*args, **kwargs)

    def __del__(self):
        self.h.push(self.id)


# Prevent end-user using constructors
cdef object __SECRET__ = object()

cdef class Domain(object):

    cdef long mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__ (self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid secret")
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def __repr__(self):
        values = self.values()
        domain = reprlib.repr(values)
        find = domain.find("...")
        if find == -1:
            return domain
        # remove the ', ...' if you missed only one element
        plus = -2 if len(values) == reprlib.aRepr.maxlist + 1 else 3
        # add last value
        domain = domain[:find + plus] + ", {}]".format(values[-1])
        return domain

    def __getitem__(self, i):
        return self.values()[i]

    def __len__(self):
        return len(self.values())

    def __str__(self):
        return repr(self.values())

    def size(self):
        return domain_size(self.mlvalue)

    def values(self):
        cdef array.array values = array.array('i', range(self.size()))
        cdef int* pt_vals = values.data.as_ints
        domain_values(self.mlvalue, pt_vals)
        return values.tolist()

    @classmethod
    def create(cls, p):
        cdef array.array values = array.array('i', p)
        cdef int* pt_vals = values.data.as_ints
        cdef long value = domain_create(pt_vals, len(values))
        return cls(value, __SECRET__)

from collections import defaultdict
name_register = defaultdict(str)
name_counter = defaultdict(int)

cdef class Variable(object):
    """The Variable is the core element for CSP problems.

    This class **shall not be directly instanciated**, but through the
    `variable` function instead.
    >>> a = variable([0, 1])

    Variables may be summed, subtracted, multiplied with other variables or
    integers. They may also be compounded into all kind of expressions
    >>> c = a * a + 2 * a + 1
    """

    cdef long mlvalue
    cdef object name

    def __dealloc__(self):
        mlname = self.mlname()
        name_counter[mlname] -= 1
        if name_counter[mlname] == 0:
            del name_counter[mlname]
            del name_register[mlname]
        fcl_destroy(self.mlvalue)

    def __cinit__ (self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Use facile.variable function")
        if value == 0:
            raise ValueError("Non reifiable constraint")
        self.mlvalue = value
        self.name = name_register[self.mlname()]
        name_counter[self.mlname()] += 1

    def __repr__(self):
        cdef int vmin, vmax
        fmt = "<Variable {} ({})>"
        return fmt.format(self.name, self.mlname())

    def __getval(self):
        return self.mlvalue

    def set_name(self, n):
        self.name = n
        name_register[self.mlname()] = n

    @property
    def name(self):
        return self.name

    def mlname(self):
        return (<bytes> val_name(self.mlvalue)).decode()

    def value(self):
        """ Return the numerical value of the variable.
        Return None if no solution has been found. """
        if (val_isbound(self.mlvalue) == 1):
            return self.domain()[0]
        else:
            return None

    def domain(self):
        return Domain(val_domain(self.mlvalue), __SECRET__)

    def in_interval(self, int inf, int sup):
        cdef long res
        res = interval_ismember(self.mlvalue, inf, sup)
        return Variable(res, __SECRET__)

    @classmethod
    def interval(cls, int min_val, int max_val, name=None):
        cdef long value = val_interval(min_val, max_val)
        val = cls(value, __SECRET__)
        if name is not None:
            val.set_name(name)
        return val

    @classmethod
    def create(cls, values, name=None):
        if isinstance(values, Arith):
            return cls(e2fd(values.__getval()), __SECRET__)
        if isinstance(values, Cstr):
            return +values
        domain = Domain.create(values)
        val = cls(val_create(domain.__getval()), __SECRET__)
        if name is not None:
            val.set_name(name)
        return val

    @classmethod
    def binary(cls, *args, **kwargs):
        return Variable.create(range(2), *args, **kwargs)

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
            return Cstr(c, __SECRET__)
        if isinstance(fd, Arith):
            c = cstr_lt(fd2e(self.__getval()), fd.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Variable):
            c = cstr_lt(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(fd, numbers.Integral):
            c = cstr_lt(fd2e(self.__getval()), i2e(fd))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __le(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_le(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Arith):
            c = cstr_le(fd2e(self.__getval()), fd.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Variable):
            c = cstr_le(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(fd, numbers.Integral):
            c = cstr_le(fd2e(self.__getval()), i2e(fd))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __eq(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_eq(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Arith):
            c = cstr_eq(fd2e(self.__getval()), fd.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Variable):
            c = cstr_eq(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(fd, numbers.Integral):
            c = cstr_eq(fd2e(self.__getval()), i2e(fd))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __ne(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_ne(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Arith):
            c = cstr_ne(fd2e(self.__getval()), fd.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Variable):
            c = cstr_ne(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(fd, numbers.Integral):
            c = cstr_ne(fd2e(self.__getval()), i2e(fd))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __gt(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_gt(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Arith):
            c = cstr_gt(fd2e(self.__getval()), fd.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Variable):
            c = cstr_gt(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(fd, numbers.Integral):
            c = cstr_gt(fd2e(self.__getval()), i2e(fd))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __ge(self, fd):
        if isinstance(fd, Cstr):
            c = cstr_ge(fd2e(self.__getval()), fd.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Arith):
            c = cstr_ge(fd2e(self.__getval()), fd.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(fd, Variable):
            c = cstr_ge(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(fd, numbers.Integral):
            c = cstr_ge(fd2e(self.__getval()), i2e(fd))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __add__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_add(i2e(a), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_add(fd2e(a.__getval()), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_add(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_add(fd2e(a.__getval()), i2e(b))
            return Arith(c, __SECRET__)
        if isinstance(b, Cstr):
            return a + Variable(cstr_boolean(b.__getval()), __SECRET__)
        return NotImplemented

    def __sub__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_sub(i2e(a), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_sub(fd2e(a.__getval()), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_sub(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_sub(fd2e(a.__getval()), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __mul__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_mul(i2e(a), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_mul(fd2e(a.__getval()), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_mul(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_mul(fd2e(a.__getval()), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __pos__(a):
        return a

    def __neg__(a):
        return 0 - a

    def __abs__(a):
        c = arith_abs(fd2e(a.__getval()))
        return Arith(c, __SECRET__)


cdef class Arith(object):
    """The Arith is an unevaluated expression made of Variables.

    This class **shall not be directly instanciated**, but through operations
    on integers, variables or other expressions.
    >>> a = variable(0, 1)
    >>> e = a + 1

    Arith may be summed, subtracted, multiplied with other variables or
    integers. They may also be compounded into all kind of expressions
    >>> c = e * e + 2 * e + 1
    """

    cdef long mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid pointer value")
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def __repr__(self):
        return repr(Variable(e2fd(self.mlvalue), __SECRET__))

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
            return Cstr(c, __SECRET__)
        if isinstance(value, Arith):
            c = cstr_lt(self.__getval(), value.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Variable):
            c = cstr_lt(self.__getval(), fd2e(value.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(value, numbers.Integral):
            c = cstr_lt(self.__getval(), i2e(value))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __le(self, value):
        if isinstance(value, Cstr):
            c = cstr_le(self.__getval(), value.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Arith):
            c = cstr_le(self.__getval(), value.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Variable):
            c = cstr_le(self.__getval(), fd2e(value.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(value, numbers.Integral):
            c = cstr_le(self.__getval(), i2e(value))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __eq(self, value):
        if isinstance(value, Cstr):
            c = cstr_eq(self.__getval(), value.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Arith):
            c = cstr_eq(self.__getval(), value.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Variable):
            c = cstr_eq(self.__getval(), fd2e(value.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(value, numbers.Integral):
            c = cstr_eq(self.__getval(), i2e(value))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __ne(self, value):
        if isinstance(value, Cstr):
            c = cstr_ne(self.__getval(), value.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Arith):
            c = cstr_ne(self.__getval(), value.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Variable):
            c = cstr_ne(self.__getval(), fd2e(value.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(value, numbers.Integral):
            c = cstr_ne(self.__getval(), i2e(value))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __gt(self, value):
        if isinstance(value, Cstr):
            c = cstr_gt(self.__getval(), value.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Arith):
            c = cstr_gt(self.__getval(), value.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Variable):
            c = cstr_gt(self.__getval(), fd2e(value.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(value, numbers.Integral):
            c = cstr_gt(self.__getval(), i2e(value))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __ge(self, value):
        if isinstance(value, Cstr):
            c = cstr_ge(self.__getval(), value.__abs__().__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Arith):
            c = cstr_ge(self.__getval(), value.__getval())
            return Cstr(c, __SECRET__)
        if isinstance(value, Variable):
            c = cstr_ge(self.__getval(), fd2e(value.__getval()))
            return Cstr(c, __SECRET__)
        if isinstance(value, numbers.Integral):
            c = cstr_ge(self.__getval(), i2e(value))
            return Cstr(c, __SECRET__)
        raise TypeError("Expressions of incompatible types")

    def __add__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_add(i2e(a), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_add(a.__getval(), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_add(a.__getval(), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_add(a.__getval(), i2e(b))
            return Arith(c, __SECRET__)
        if isinstance(b, Cstr):
            return a + Variable(cstr_boolean(b.__getval()), __SECRET__)
        return NotImplemented

    def __sub__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_sub(i2e(a), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_sub(a.__getval(), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_sub(a.__getval(), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_sub(a.__getval(), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __mul__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_mul(i2e(a), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_mul(a.__getval(), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_mul(a.__getval(), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_mul(a.__getval(), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __pos__(a):
        return a

    def __neg__(a):
        return 0 - a

    def __abs__(a):
        c = arith_abs(a.__getval())
        return Arith(c, __SECRET__)

cdef class Cstr(object):
    """The Cstr is a way to build relations on expressions or variables.

    This class **shall not be directly instanciated**, but through arithmetic
    relations on expressions or through dedicated functions instead.
    >>> x = [variable(0, 5) for i in range(5)]
    >>> c1 = x[0] != 0
    >>> c2 = x[3] > x[2] + x[1]
    >>> c3 = alldifferent(x)

    Constraints must be posted to the solver in order to be taken into account.
    >>> constraint(c1)

    Constraints may be compounded with & (and), | (or), ^ (xor), ~ (not)
    operators. Constraints may also be converted (reified) into variables
    evaluated to 1 if the constraint is verified and to 0 if the constraint is
    violated.
    >>> constraint(c1 | c2)

    Constraints are automatically reified when included into arithmetic
    operations. `c_sum` means that more than 3 constraints of c are verified.
    >>> c = [x[i] > x[i + 1] for i in range(4)]
    >>> c_sum = sum(c) > 3

    We can force the reification of a constraint into a variable with a `+`
    prefix.
    >>> x_c2 = +c2
    >>> constraint(x_c2 == 0)

    This is equivalent to:
    >>> constraint(~c2)

    Some constraints are not reifiable, e.g. alldifferent.
    >>> +alldifferent(x)
    Traceback (most recent call last):
        ...
    ValueError: Non reifiable constraint
    """

    cdef long mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid pointer value")
        if value == 0:
            raise ValueError("Non reifiable constraint")
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def __repr__(self):
        return (<bytes> cstr_name(self.__getval())).decode()

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
        """`&` (and) operator on constraints.

        >>> x = array([variable(0, 1) for i in range(3)])
        >>> c1 = x[0] != x[1]
        >>> c2 = x[1] != x[2]
        >>> constraint(c1 & c2)
        >>> assert solve(x)
        >>> x.value()
        [0, 1, 0]
        """
        return Cstr(cstr_and(c1.__getval(), c2.__getval()), __SECRET__)

    def __or__(Cstr c1, Cstr c2):
        """`|` (or) operator on constraints.

        >>> x = array([variable(0, 1) for i in range(3)])
        >>> c1 = x[0] != x[1]
        >>> c2 = x[1] != x[2]
        >>> constraint(c1 | c2)
        >>> assert solve(x)
        >>> x.value()
        [0, 0, 1]
        """
        return Cstr(cstr_or(c1.__getval(), c2.__getval()), __SECRET__)

    def __invert__(self):
        """`~` (not) operator on constraints.

        >>> x = array([variable(0, 1) for i in range(3)])
        >>> c1 = x[0] != x[1]
        >>> c2 = x[1] != x[2]
        >>> constraint(~c1)
        >>> constraint(c2)
        >>> assert solve(x)
        >>> x.value()
        [0, 0, 1]

        The ~ operator can only be applied to reifiable constraints.
        >>> x = array([variable(0, 1) for i in range(3)])
        >>> c = alldifferent(x)
        >>> constraint(~c)
        Traceback (most recent call last):
            ...
        ValueError: Non reifiable constraint
        """
        return Cstr(cstr_not(self.__getval()), __SECRET__)

    def __xor__(Cstr c1, Cstr c2):
        """`^` (xor) operator on constraints.

        >>> x = array([variable(0, 1) for i in range(3)])
        >>> c1 = x[0] != x[1]
        >>> c2 = x[1] != x[2]
        >>> constraint(c1 ^ c2)
        >>> assert solve(x)
        >>> x.value()
        [0, 0, 1]
        """
        return Cstr(cstr_xor(c1.__getval(), c2.__getval()), __SECRET__)

    def __add__(c1, c2):
        if isinstance(c2, Cstr):
            return c1 + Variable(cstr_boolean(c2.__getval()), __SECRET__)
        if isinstance(c1, Cstr):
            return Variable(cstr_boolean(c1.__getval()), __SECRET__) + c2
        return NotImplemented

    def __sub__(c1, c2):
        if isinstance(c2, Cstr):
            return c1 - Variable(cstr_boolean(c2.__getval()), __SECRET__)
        if isinstance(c1, Cstr):
            return Variable(cstr_boolean(c1.__getval()), __SECRET__) - c2
        return NotImplemented

    def __mul__(c1, c2):
        if isinstance(c2, Cstr):
            return c1 * Variable(cstr_boolean(c2.__getval()), __SECRET__)
        if isinstance(c1, Cstr):
            return Variable(cstr_boolean(c1.__getval()), __SECRET__) * c2
        return NotImplemented

    def __pos__(a):
        """Constraint reification."""
        return Variable(cstr_boolean(a.__getval()), __SECRET__)

    def __neg__(a):
        """Constraint reification."""
        return 0 - Variable(cstr_boolean(a.__getval()), __SECRET__)

    def __abs__(a):
        """Constraint reification."""
        return Variable(cstr_boolean(a.__getval()), __SECRET__)

    def post(self):
        """Constraint posting to the solver."""
        if cstr_post(self.__getval()) == 1:
            raise ValueError("The problem is overconstrained")

    # For Python 2.x
    def __nonzero__(self):
        """Constraints cannot be interpreted as booleans."""
        raise ValueError("A constraint cannot be interpreted as a boolean.")

    # For Python 3.x
    def __bool__(self):
        """Constraints cannot be interpreted as booleans."""
        raise ValueError("A constraint cannot be interpreted as a boolean.")

# TODO operateurs terme à terme (+, -, *) with zip_longest
cdef class Array(object):
    """Array helps the manipulation of arrays of variables and/or expressions.

    - It can be indexed by variables, expressions or integers.
    - You can compare two arrays of the same length and get an iterable with
      the corresponding constraints.
    - You can access its max() or min().
    - You can access its sorted version: sort()
    - You can apply an alldifferent constraint: alldifferent()
    - You can apply a global cardinAlity constraint: gcc()

    This class **shall not be directly instanciated**, but through `array`
    function over an iterable structure instead.
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
        cdef array.array values = array.array('L', range(self.length))
        cdef long* pt_vals = values.data.as_longs
        fdarray_read(self.mlvalue, pt_vals)
        for i in range(self.length):
            yield Variable(values[i], __SECRET__)

    def __repr__(self):
        return repr(list(self))

    def __getitem__(self, key):
        """Returns self[key].
        key can be a variable, an expression or an integer.

        Warning: An implicit constraint is set on the key so that it takes
        values between 0 and (len-1).
        """
        cdef long value
        if isinstance(key, Variable):
            value = fdarray_get(self.mlvalue, key.__getval())
        elif isinstance(key, Arith):
            value = fdarray_get(self.mlvalue, e2fd(key.__getval()))
        elif isinstance(key, numbers.Integral):
            value = fdarray_get(self.mlvalue, e2fd(i2e(key)))
        else:
            raise TypeError("Index should be integer, variable or expression")
        if value == 0:
            raise IndexError("Index out of bounds")
        return Variable(value, __SECRET__)

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
        """Returns a new variable that is the max of all expressions in self.

        >>> a = array([variable(0, 10) for i in range(3)])
        >>> constraint(a.alldifferent())
        >>> constraint(a.min() == 4)
        >>> constraint(a.max() == 6)
        >>> solve(a)
        True
        >>> a.value()
        [4, 5, 6]
        """
        cdef long value
        value = fdarray_max(self.mlvalue)
        if value == 0:
            raise IndexError("Empty list")
        return Variable(value, __SECRET__)

    def min(self):
        """Returns a new variable that is the min of all expressions in self.

        >>> a = array([variable(0, 10) for i in range(3)])
        >>> constraint(a.alldifferent())
        >>> constraint(a.min() == 4)
        >>> constraint(a.max() == 6)
        >>> solve(a)
        True
        >>> a.value()
        [4, 5, 6]
        """
        cdef long value
        value = fdarray_min(self.mlvalue)
        if value == 0:
            raise IndexError("Empty list")
        return Variable(value, __SECRET__)

    def sort(self):
        """Return an array of variables sorted in increasing order."""
        cdef long value
        value = sorting_sort(self.mlvalue)
        return Array(value, self.length)

    def alldifferent(self, *args, **kwargs):
        """Equivalent to alldifferent applied to all elements of the array.

        >>> a = array([variable(0, 2) for i in range(3)])
        >>> constraint(a.alldifferent())
        >>> solve(a)
        True
        >>> a.value()
        [0, 1, 2]

        See also: help(alldifferent)
        """

        return alldifferent(list(self), *args, **kwargs)

    def gcc(self, distribution):
        """Return a Global Cardinality Constraint w.r.t distribution.

        For each pair (c, v) in distribution, c variables in the array will be
        instantiated to v.
        Also, the sum of the cardinals will be equal to the number of variables
        in the array (the corresponding constraint is automatically posted).
        >>> a = array([variable(0,3) for i in range(5)])
        >>> c = constraint(a.gcc([(1, 3), (1, 2), (3, 1)]))
        >>> solve(a)
        True
        >>> a.value()
        [1, 1, 1, 2, 3]

        Note that this constraint is not reifiable.

        If you need a simpler cardinality constraint, you can write:
        >>> a = array([variable(0,3) for i in range(5)])
        >>> constraint(sum([p == 1 for p in a]) == 2)
        >>> solve(a)
        True
        >>> a.value()
        [0, 0, 0, 1, 1]
        """

        cdef long value
        cdef long l = len(distribution)
        cdef long card = 0

        cards = []
        do_not_gc = []
        values = []
        for (c, v) in distribution:
            if isinstance(c, numbers.Integral):
                c = Variable.create([c])
            if isinstance(c, Arith):
                c = Variable(c)
            if not isinstance(c, Variable):
                msg = "Cardinals must be integers, variables or expressions"
                raise TypeError(msg)
            if not isinstance(v, numbers.Integral):
                raise TypeError("Values must be integers")
            do_not_gc.append(c)
            cards.append(c.__getval())
            values.append(v)

        cdef array.array _cards = array.array('L', cards)
        cdef array.array _values = array.array('L', values)
        cdef long* pt_cards = _cards.data.as_longs
        cdef long* pt_values = _values.data.as_longs

        value = gcc_cstr(self.mlvalue, pt_cards, pt_values, l)
        return Cstr(value, __SECRET__)


cdef class Strategy(object):
    """
        Strategies for the Goal.forall method.
        min_min, min_domain, queen
        TODO: pass a callback to be evaluated.
    """
    cdef long mlvalue
    cdef object _toclean

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)
        # IMPORTANT! Free memory
        for i in self._toclean:
            del __ml_callbacks[i]

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid pointer value")
        self.mlvalue = value
        self._toclean = []

    def __getval(self):
        return self.mlvalue

    def toclean(self, p):
        self._toclean.append(p)

    @classmethod
    def min_min(cls):
        """Favours the value with the minimum minimum value in its domain."""
        return cls(strategy_minmin(), __SECRET__)

    @classmethod
    def min_domain(cls):
        """Favours the value with the smaller domain."""
        return cls(strategy_mindomain(), __SECRET__)

    @classmethod
    def queen(cls):
        """Good strategy for queens: min_domain then min_min."""
        return cls(strategy_queen(), __SECRET__)


    @classmethod
    def custom(cls, cb):
        """Custom strategy from Python callback."""
        @callback
        def custom_callback(v1, v2):
            var1 = Variable(v1, __SECRET__)
            var2 = Variable(v2, __SECRET__)
            res = 1 if cb(var1, var2) else 0
            return res
        __ml_callbacks[custom_callback.id] = custom_callback
        set_strategy_callback(custom_callback.id, strategy_cb)
        strategy = cls(strategy_callback(custom_callback.id), __SECRET__)
        strategy.toclean(custom_callback.id)
        return strategy


cdef class Assignation(object):

    cdef long mlvalue
    cdef object _toclean

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)
        # IMPORTANT! Free memory
        for i in self._toclean:
            del __ml_callbacks[i]

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid pointer value")
        self.mlvalue = value
        self._toclean = []

    def __getval(self):
        return self.mlvalue

    def toclean(self, p):
        self._toclean.append(p)

    @classmethod
    def indomain(cls):
        return cls(assignation_indomain(), __SECRET__)

    @classmethod
    def assign(cls):
        return cls(assignation_assign(), __SECRET__)

    @classmethod
    def dichotomic(cls):
        return cls(assignation_dichotomic(), __SECRET__)

    @classmethod
    def atomic(cls, f):
        @callback
        def atomic_callback(value):
            variable = Variable(value, __SECRET__)
            f(variable)
        __ml_callbacks[atomic_callback.id] = atomic_callback
        set_assign_callback(atomic_callback.id, on_assign_callback)
        res = cls(assignation_atomic(atomic_callback.id), __SECRET__)
        res.toclean(atomic_callback.id)
        return res

    def __and__(Assignation c1, Assignation c2):
        """`&` (and) operator on assignation.

        """
        res = Assignation(assignation_and(c1.__getval(), c2.__getval()), __SECRET__)
        res._toclean = c1._toclean + c2._toclean
        c1._toclean.clear()
        c2._toclean.clear()
        return res

    def __or__(Assignation c1, Assignation c2):
        """`|` (or) operator on assignation.

        """
        res = Assignation(assignation_or(c1.__getval(), c2.__getval()), __SECRET__)
        res._toclean = c1._toclean + c2._toclean
        c1._toclean.clear()
        c2._toclean.clear()
        return res

cdef class Goal(object):

    cdef long mlvalue
    cdef object _variables
    cdef object _toclean
    cdef object _keep

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)
        # IMPORTANT! Free memory
        for i in self._toclean:
            del __ml_callbacks[i]

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid pointer value")
        self.mlvalue = value
        self._toclean = []
        self.variables = []
        self._keep = []

    def __getval(self):
        return self.mlvalue

    def __and__(Goal c1, Goal c2):
        """`&` (and) operator on goals.

        """
        res = Goal(goals_and(c1.__getval(), c2.__getval()), __SECRET__)
        res.variables = c1.variables + c2.variables
        res._toclean = c1._toclean + c2._toclean
        res._keep = c1._keep + c2._keep
        c1._toclean.clear()
        c2._toclean.clear()
        return res

    def __or__(Goal c1, Goal c2):
        """`|` (or) operator on goals.

        """
        res = Goal(goals_or(c1.__getval(), c2.__getval()), __SECRET__)
        res.variables = c1.variables + c2.variables
        res._toclean = c1._toclean + c2._toclean
        res._keep = c1._keep + c2._keep
        c1._toclean.clear()
        c2._toclean.clear()
        return res

    def toclean(self, p):
        self._toclean.append(p)

    def keep(self, p):
        self._keep.append(p)

    @property
    def variables(self):
        return self._variables

    @variables.setter
    def variables(self, v):
        self._variables = v

    @classmethod
    def fail(cls):
        res = cls(goals_fail(), __SECRET__)
        return res

    @classmethod
    def success(cls):
        res = cls(goals_success(), __SECRET__)
        return res

    @classmethod
    def atomic(cls, callback):
        try:
            __ml_callbacks[callback.id] = callback
            set_atomic_callback(callback.id, atomic_callback)
        except AttributeError:
            msg = "Use the @facile.callback decorator around your callbacks"
            raise TypeError(msg)
        else:
            res = cls(goals_atomic(callback.id), __SECRET__)
            res.toclean(callback.id)
            return res

    @classmethod
    def unify(cls, Variable var, int i):
        print ("unify {} {}".format(var, i))
        res = cls(goals_unify(var.__getval(), i), __SECRET__)
        res.variables = [[var]]  # does it make sense?
        return res

    @classmethod
    def forall(cls, variables, strategy=None, assign=Assignation.indomain()):
        cdef long length = 0
        cdef long* pt_vars
        cdef array.array _vars

        cdef long c_assign = 0
        cdef long c_strategy = 0 # default to no strategy

        if isinstance(assign, str):
            assign = eval("Assignation." + assign + "()")

        if not isinstance(assign, Assignation):
            raise ValueError("assign must refer to a valid Assignation")

        c_assign = assign.__getval() # default to indomain

        if isinstance(variables, collections.Iterable):
            variables = list(variables)
            length = len(variables)

            for v in variables:
                msg = "All arguments must be variables"
                assert isinstance(v, Variable), msg

            _vars = array.array('L', [v.__getval() for v in variables])
            pt_vars = _vars.data.as_longs

            if strategy is not None:
                if isinstance(strategy, str):
                    strategy = eval("Strategy." + strategy + "()")

                if isinstance(strategy, types.FunctionType):
                    strategy = Strategy.custom(strategy)

                msg = "The second argument is a strategy"
                assert isinstance(strategy, Strategy), msg
                c_strategy = strategy.__getval()

            if length < 1:
                raise TypeError("The argument list must be non empty")

            res = cls(goals_forall(c_strategy, pt_vars, length, c_assign), __SECRET__)
            res.variables = [variables]
            res.keep([strategy, assign])
            return res

        raise TypeError("The argument must be iterable")

    @classmethod
    def minimize(cls, goal, expr, keep, *args):
        if not isinstance(goal, Goal):
            goal = Goal.forall(goal, *args)
        if isinstance(expr, Arith):
            expr = Variable(e2fd(expr.__getval()), __SECRET__)
        if not isinstance(expr, Variable):
            raise SyntaxError

        msg = "The expression you want should be part of your goal"
        values = [x.__getval() for sub in goal.variables for x in sub]
        if expr.__getval() not in values:
            raise ValueError(msg)

        __ml_callbacks[keep.id] = keep
        set_onsol_callback(keep.id, on_solution_callback)

        obj = goals_minimize(goal.__getval(), expr.__getval(), keep.id)

        res = cls(obj, __SECRET__)
        res.variables = goal.variables
        res.toclean(keep.id)
        return res


class Solution(dict):

    @staticmethod
    def __pretty_print(item):
        key, _ = item
        pretty_print = {
                "solved": "Resolution status",
                "time"  : "Resolution time",
                "backtrack": "Backtracks",
                "evaluation": "Current evaluation",
                "solution": "Current solution",
                }
        try:
            return pretty_print[key]
        except KeyError:
            return key

    def __getattr__(self, p):
        return self[p]

    def __bool__(self):
        return self['solved']

    def __nonzero__(self):
        return self['solved']

    def __repr__(self):
        return repr(self['solved'])

    def __str__(self):
        res = ""
        for item in sorted(self.items(), key=self.__pretty_print):
            k, v = item
            k = self.__pretty_print(item).capitalize()
            if k == "Resolution time":
                msg = "{:<30}: {:.2g}s"
            else:
                msg = "{:<30}: {}"
            if k == "Current solution":
                res += msg.format(k, reprlib.repr(v)) + "\n"
            else:
                res += msg.format(k, v) + "\n"
        return res


cdef object __ml_callbacks = {}

cdef void on_backtrack_callback(int i, int n):
    __ml_callbacks[i](n)

cdef void atomic_callback(int i):
    __ml_callbacks[i]()

cdef void on_solution_callback(int i, int n):
    __ml_callbacks[i](n)

cdef void on_assign_callback(int i, long v):
    __ml_callbacks[i](v)

cdef int strategy_cb(int i, long v1, long v2):
    return __ml_callbacks[i](v1, v2)

def interrupt():
    fcl_interrupt()

def solve(objective, *args, time=True, backtrack=False, on_backtrack=None,
        all_solutions=False, minimize=None, on_solution=None, **kwargs):
    """Solves the current CSP problem.

    The `solve` function solves the problem defined by all posted constraints
    on variables passed in parameter.

    The objective is a Goal to meet by the solver. You may also pass an iterable
    structure of 

    The `time` parameter, if set to True (default), embeds the time spent on
    resolution in the solution display structure.

    The `backtrack` parameter, if set to True, embeds the number of backtracks
    performed during the resolution in the solution display structure.

    The `on_backtrack` parameter accepts a callback to be called each time the
    solver backtracks. The callback must be a Python function wrapped by the
    @facile.callback decorator.

    The most basic usage of the solve function comes as follows:

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)
    >>> assert solve([a, b])
    >>> a.value(), b.value()
    (0, 1)

    """

    global __ml_callbacks

    cdef long length
    cdef long* pt_vars
    cdef array.array _vars

    if not isinstance(objective, Goal):
        objective = Goal.forall(objective, *args, **kwargs)
    else:
        if len(args) > 0:
            raise Exception("name your parameters")

    sol = Solution()

    cdef int i = 0

    if on_backtrack is None:
        def mute(n): pass
        on_backtrack = callback(mute)
    else:
        on_backtrack = callback(on_backtrack)

    if on_solution is None:
        def mute(n): pass
        on_solution = callback(mute)
    else:
        on_solution = callback(on_solution)

    if backtrack:
        def stack_backtrack(n):
            on_backtrack(n)
            sol['backtrack'] = n
        # tricky one: bind the id of stack_backtrack to the id of on_backtrack
        __ml_callbacks[on_backtrack.id] = stack_backtrack
    else:
        try:
            __ml_callbacks[on_backtrack.id] = on_backtrack
        except AttributeError:
            msg = "Use the @facile.callback decorator around your callbacks"
            raise TypeError(msg)

    set_backtrack_callback(on_backtrack.id, on_backtrack_callback)

    if time:
        import time
        start = time.perf_counter()

    if minimize is not None:

        @callback
        def keep(value):
            sol['solved'] = False
            sol['evaluation'] = value
            sol['solution'] = [x.value() for sub in objective.variables
                    for x in sub]
            if time:
                sol['time'] = time.perf_counter() - start

            on_solution(Solution(sol))

        obj = Goal.minimize(objective, minimize, keep)
        obj = obj | Goal.success()
        _ = goals_solve(on_backtrack.id, obj.__getval()) == 1
        sol['solved'] = True
        sol['time'] = time.perf_counter() - start

        # IMPORTANT! Free memory
        del __ml_callbacks[on_backtrack.id]

        return sol

    if all_solutions:
        res = []

        @callback
        def keep():
            sol['solved'] = True
            sol['solution'] = [x.value() for sub in objective.variables
                    for x in sub]
            if time:
                sol['time'] = time.perf_counter() - start

            on_solution(Solution(sol))
            res.append(Solution(sol))

        obj = (objective & Goal.atomic(keep) & Goal.fail()) | Goal.success()
        sol['solved'] = goals_solve(on_backtrack.id, obj.__getval()) == 1
        sol['time'] = time.perf_counter() - start
        sol['solution'] = None
        res.append(sol)

        # IMPORTANT! Free memory
        del __ml_callbacks[on_backtrack.id]

        return res

    # else
    sol['solved'] = goals_solve(on_backtrack.id, objective.__getval()) == 1

    # IMPORTANT! Free memory
    del __ml_callbacks[on_backtrack.id]

    if time:
        sol['time'] = time.perf_counter() - start

    sol['solution'] = [x.value() for sub in objective.variables for x in sub]

    return sol

def solve_all(*args, **kwargs):
    """Solves the CSP problem and yields all solutions.

    The `solve_all` function solves the problem defined by all posted
    constraints on variables passed in parameter.

    It returns all possible solutions to the problem.

    The last solution is None as it may contain the resolution time and
    number of backtracks until the end of the exploration of the search space.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)
    >>> [s.solution for s in solve_all([a, b]) if s.solution is not None]
    [[0, 1], [1, 0]]
    """
    return solve(*args, all_solutions=True, **kwargs)

def minimize(goal, expr, *args, **kwargs):
    """
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
    >>> res = minimize([a, b], a*a + b*b)
    >>> res.solution
    [5, 5]
    >>> res.evaluation
    50
    """
    res = solve(goal, *args, minimize=expr, **kwargs)
    return res

def constraint(cstr):
    """
    The `constraint` function defines a constraint and posts it to the
    solver. The constraint can be expressed in an intuitive manner, based
    on expressions on variables.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)

    `constraint` raises `TypeError` if the parameter is not a constraint.

    >>> constraint(a)
    Traceback (most recent call last):
        ...
    TypeError: The argument must be a (non-reified) constraint

    See also: help(Cstr)
    """

    if not isinstance(cstr, Cstr):
        raise TypeError("The argument must be a (non-reified) constraint")
    cstr.post()

def alldifferent(variables, lazy=False):
    """ Creates an alldifferent constraint (non-reifiable).

    `alldifferent` raises a TypeError if `variables` is not iterable and if
    variables does not contain more than two variables.

    When `on_refine` is True, the consistency of the constraint is not only
    checked when a new variable is instantiated, but also when its domain is
    refined.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(alldifferent([a, b]))
    >>> solve([a, b])
    True
    >>> a.value(), b.value()
    (0, 1)

    See also: help(Array.alldifferent)
    """

    cdef long length
    cdef long* pt_vars
    cdef array.array _vars
    cdef int _lazy = 1
    if lazy: _lazy = 0
    if isinstance(variables, collections.Iterable):
        variables = list(variables)
        length = len(variables)
        if length < 2:
            raise TypeError("The argument list must be non empty")
        _vars = array.array('L', range(length))
        for i in range(length):
            if isinstance(variables[i], Variable):
                _vars[i] = variables[i].__getval()
            elif isinstance(variables[i], Arith):
                _vars[i] = e2fd(variables[i].__getval())
            else:
                raise TypeError("Arguments must be variables or expressions")
        pt_vars = _vars.data.as_longs
        return Cstr(cstr_alldiff(pt_vars, length, _lazy), __SECRET__)
    raise TypeError("The argument must be iterable")

def variable(min_val, max_val=None, *args, **kwargs):
    """Creates a Variable taking values on a discrete interval.

    The `variable` function creates a variable on a discrete interval, with
    `min_val` and `max_val`  as bounds.
    >>> a = variable(0, 2)

    Variables may be summed, subtracted, multiplied with other variables or
    integers. They may also be combined into all kind of expressions.
    >>> e = a * a + 2 * a + 1

    See also: help(Variable)
    """

    if isinstance(min_val, Arith):
        return Variable.create(min_val, *args, **kwargs)
    if isinstance(min_val, Cstr):
        return Variable.create(min_val, *args, **kwargs)
    if isinstance(min_val, collections.Iterable):
        return Variable.create(min_val, *args, **kwargs)
    print ("Usage deprecated, prefer variable([list of values])")
    if max_val < min_val:
        raise ValueError("The upper bound should exceed the lower bound")
    return Variable.interval(min_val, max_val, *args, **kwargs)

def array(variables):
    """Creates an Array from an iterable structure.

    This structure facilitates the manipulation of arrays of variables and/or
    expressions.

    - It can be indexed by variables, expressions or integers.
    - You can compare two arrays of the same length and get an iterable with
      the corresponding constraints.
    - You can access its max() or min().
    - You can access its sorted version: sort()
    - You can apply an alldifferent constraint: alldifferent()
    - You can apply a Global Cardinality Constraint: gcc()

    >>> a = array([variable(0, 1), variable(0, 3), variable(0, 5)])
    >>> constraint(a.min() == 1)
    >>> constraint(alldifferent(a))
    >>> constraint(abs(a[0] - a[1]) >= 2)
    >>> constraint(abs(a[1] - a[2]) >= 2)
    >>> solve(a)
    True
    >>> a.value()
    [1, 3, 5]

    See also: help(Array)
    """
    cdef long length
    cdef long* pt_vars
    cdef long value
    cdef Variable v
    cdef array.array _vars
    if isinstance(variables, collections.Iterable):
        variables = list(variables)
        length = len(variables)
        if length < 1:
            raise TypeError("The argument list must be non-empty")
        _vars = array.array('L', range(length))
        for i in range(length):
            if isinstance(variables[i], Variable):
                _vars[i] = variables[i].__getval()
            elif isinstance(variables[i], Arith):
                _vars[i] = e2fd(variables[i].__getval())
            elif isinstance(variables[i], numbers.Integral):
                _vars[i] = e2fd(i2e(variables[i]))
            else:
                raise TypeError("The arguments must be variables or expressions")
        pt_vars = _vars.data.as_longs
        value = fdarray_create(pt_vars, length)
        return Array(value, length)
    raise TypeError("The argument must be iterable")

