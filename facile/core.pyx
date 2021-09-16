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
import functools
import heapq
import itertools
import numbers
import reprlib
import types
from collections import defaultdict
from collections.abc import Iterable

cimport cpython
from cpython cimport array
from libc.stdint cimport uintptr_t

from interface cimport *

# Initialize OCaml
init()

class Stak_Fail(RuntimeError):
    pass

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
        if self.id >= 204800:
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

    cdef uintptr_t mlvalue

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

    def remove_low(self, int x):
        return Domain(domain_removelow(x, self.mlvalue), __SECRET__)

    def remove_up(self, int x):
        return Domain(domain_removeup(x, self.mlvalue), __SECRET__)

    @classmethod
    def create(cls, p):
        cdef array.array values = array.array('i', p)
        cdef int* pt_vals = values.data.as_ints
        cdef uintptr_t value = domain_create(pt_vals, len(values))
        return cls(value, __SECRET__)

name_register = defaultdict(str)
name_counter = defaultdict(int)

cdef class Event(object):

    cdef uintptr_t mlvalue
    cdef object name

    def __dealloc__(self):
        # nope: returning caml_named_value which is already a root!
        # fcl_destroy(self.mlvalue)
        pass

    def __getval(self):
        return self.mlvalue

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Do not use this!")
        self.mlvalue = value

    @classmethod
    def on(cls, name):
        if name == "max":
            return cls(fd_onmax(), __SECRET__)
        if name == "min":
            return cls(fd_onmin(), __SECRET__)
        if name == "refine":
            return cls(fd_onrefine(), __SECRET__)
        if name == "subst":
            return cls(fd_onsubst(), __SECRET__)
        raise ValueError("Unknown event")


cdef class Variable(object):
    """The Variable is the core element for CSP problems.

    This class **shall not be directly instanciated**, but through the
    `variable` function instead.
    >>> a = variable([0, 1])

    Variables may be summed, subtracted, multiplied with other variables or
    integers. They may also be compounded into all kind of expressions
    >>> c = a * a + 2 * a + 1
    """

    cdef uintptr_t mlvalue
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
            return self.min()
        else:
            return None

    def min(self):
        return val_min(self.mlvalue)

    def max(self):
        return val_max(self.mlvalue)

    def domain(self):
        return Domain(val_domain(self.mlvalue), __SECRET__)

    def refine(self, Domain d):
        if val_refine(self.mlvalue, d.mlvalue) == 1:
            raise Stak_Fail

    def delay(self, events, Cstr c):
        events_val = [e.__getval() for e in events]
        cdef array.array _events = array.array('Q', events_val)
        cdef uintptr_t* pt_events = <uintptr_t*>_events.data.as_voidptr
        val_delay(self.mlvalue, pt_events, len(events), c.__getval())

    def in_interval(self, int inf, int sup):
        cdef uintptr_t res
        res = interval_ismember(self.mlvalue, inf, sup)
        return Variable(res, __SECRET__)

    @classmethod
    def interval(cls, int min_val, int max_val, name=None):
        cdef uintptr_t value = val_interval(min_val, max_val)
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

    def __floordiv__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_div(i2e(a), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_div(fd2e(a.__getval()), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_div(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_div(fd2e(a.__getval()), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __mod__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_mod(i2e(a), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_mod(fd2e(a.__getval()), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_mod(fd2e(a.__getval()), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_mod(fd2e(a.__getval()), i2e(b))
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

    cdef uintptr_t mlvalue

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

    def __floordiv__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_div(i2e(a), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_div(a.__getval(), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_div(a.__getval(), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_div(a.__getval(), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __mod__(a, b):
        if isinstance(a, numbers.Integral):
            c = arith_mod(i2e(a), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Arith):
            c = arith_mod(a.__getval(), b.__getval())
            return Arith(c, __SECRET__)
        if isinstance(b, Variable):
            c = arith_mod(a.__getval(), fd2e(b.__getval()))
            return Arith(c, __SECRET__)
        if isinstance(b, numbers.Integral):
            c = arith_mod(a.__getval(), i2e(b))
            return Arith(c, __SECRET__)
        return NotImplemented

    def __pos__(a):
        return a

    def __neg__(a):
        return 0 - a

    def __abs__(a):
        c = arith_abs(a.__getval())
        return Arith(c, __SECRET__)

    def value(self):
        v = Variable.create(self)
        return v.value()

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

    cdef uintptr_t mlvalue

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
            raise Stak_Fail

    # For Python 2.x
    def __nonzero__(self):
        """Constraints cannot be interpreted as booleans."""
        raise ValueError("A constraint cannot be interpreted as a boolean.")

    # For Python 3.x
    def __bool__(self):
        """Constraints cannot be interpreted as booleans."""
        raise ValueError("A constraint cannot be interpreted as a boolean.")

def linearize(cumul, x):
    value, mult = cumul
    i_, s_ = x
    return value + mult * i_, mult * s_

# TODO operateurs terme à terme (+, -, *) with zip_longest
cdef class Array(object):
    """Array helps the manipulation of arrays of variables and/or expressions.

    - It can be indexed by variables, expressions or integers.
    - You can compare two arrays of the same length and get an iterable with
      the corresponding constraints.
    - You can access its max() or min().
    - You can access its sorted version: sort()
    - You can apply an alldifferent constraint: alldifferent()
    - You can apply a global cardinality constraint: gcc()

    This class **shall not be directly instanciated**, but through `array`
    function over an iterable structure instead.
    """

    cdef uintptr_t mlvalue
    cdef long length
    cdef tuple shape

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value, length, shape):
        self.mlvalue = value
        self.length = length
        self.shape = shape

    def __getval(self):
        return self.mlvalue

    def __len__(self):
        return self.length

    def __iter__(self):
        cdef array.array values = array.array('Q', range(self.length))
        cdef uintptr_t* pt_vals = <uintptr_t*>values.data.as_voidptr
        fdarray_read(self.mlvalue, pt_vals)
        for i in range(self.length):
            yield Variable(values[i], __SECRET__)

    def __repr__(self):
        if len(self.shape) > 1:
            import numpy as np

            return repr(np.array(list(self)).reshape(self.shape))
        return repr(list(self))

    def __getitem__(self, key):
        """Returns self[key].
        key can be a variable, an expression or an integer.

        Warning: An implicit constraint is set on the key so that it takes
        values between 0 and (len-1).
        """
        cdef uintptr_t value
        try:
            import numpy as np

            if isinstance(key, tuple):
                if len(key) != len(self.shape):
                    msg = "The index must of the same dimension as the array"
                    raise KeyError(msg)
                if any(isinstance(k, slice) for k in key):
                    new_array = np.array(list(self))
                    return array(new_array.reshape(self.shape)[key])

                assert all(
                    not isinstance(i_, int) or i_ < s_
                    for i_, s_ in zip(key, self.shape)
                )

                # key = np.ravel_multi_index(key, dims=self.shape)
                key, _ = functools.reduce(
                    linearize, zip(reversed(key), reversed(self.shape))
                )
        except ImportError:
            pass
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
        res = [x.value() for x in self]
        if len(self.shape) > 1:
            import numpy as np

            return np.array(res).reshape(self.shape)
        return res

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
        cdef uintptr_t value
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
        cdef uintptr_t value
        value = fdarray_min(self.mlvalue)
        if value == 0:
            raise IndexError("Empty list")
        return Variable(value, __SECRET__)

    def sum(self):
        """Returns a new variable that is the sum of all expressions in self"""
        cdef uintptr_t value
        value = fdarray_sum(self.mlvalue)
        if value == 0:
            raise IndexError("Empty list")
        return Variable(value, __SECRET__)

    def sort(self):
        """Return an array of variables sorted in increasing order."""
        cdef uintptr_t value
        value = sorting_sort(self.mlvalue)
        return Array(value, self.length, self.shape)

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
        >>> a = array([variable(0, 3) for i in range(5)])
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

        cdef uintptr_t value
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

        cdef array.array _cards = array.array('Q', cards)
        cdef array.array _values = array.array('l', values)
        cdef uintptr_t* pt_cards = <uintptr_t*> _cards.data.as_voidptr
        cdef long* pt_values = _values.data.as_longs

        value = gcc_cstr(self.mlvalue, pt_cards, pt_values, l)
        return Cstr(value, __SECRET__)

    @classmethod
    def binary(cls, shape):
        import numpy as np

        v = np.empty(shape, dtype=object).ravel()
        for i, x in enumerate(np.ravel(v)):
            v[i] = Variable.binary()
        return array(v.reshape(shape))

    @classmethod
    def variable(cls, shape, min_val, max_val=None, *args, **kwargs):
        import numpy as np

        v = np.empty(shape, dtype=object).ravel()
        for i, x in enumerate(np.ravel(v)):
            v[i] = variable(min_val, max_val, *args, **kwargs)
        return array(v.reshape(shape))


cdef class Strategy(object):
    """
        Strategies for the Goal.forall method.
        min_min, min_domain, queen
    """
    cdef uintptr_t mlvalue
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
    def custom(cls, custom):
        """Custom strategy from Python callback."""
        @callback
        def custom_callback(values):
            return custom(values)
        __ml_callbacks[custom_callback.id] = custom_callback
        set_strategy_callback(custom_callback.id, strategy_cb)
        strategy = cls(strategy_callback(custom_callback.id), __SECRET__)
        strategy.toclean(custom_callback.id)
        return strategy


cdef class Assignment(object):

    cdef uintptr_t mlvalue
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
        self._keep = []

    def __getval(self):
        return self.mlvalue

    def toclean(self, p):
        self._toclean.append(p)

    def keep(self, p):
        self._keep.append(p)

    @classmethod
    def indomain(cls):
        return cls(assignment_indomain(), __SECRET__)

    @classmethod
    def assign(cls):
        return cls(assignment_assign(), __SECRET__)

    @classmethod
    def dichotomic(cls):
        return cls(assignment_dichotomic(), __SECRET__)

    @classmethod
    def atomic(cls, f):
        @callback
        def cb(value):
            variable = Variable(value, __SECRET__)
            f(variable)
        __ml_callbacks[cb.id] = atomic_callback
        set_assign_callback(cb.id, on_assign_callback)
        res = cls(assignment_atomic(cb.id), __SECRET__)
        res.keep(cb)
        res.toclean(cb.id)
        return res

    def __and__(Assignment c1, Assignment c2):
        """`&` (and) operator on assignment.

        """
        res = Assignment(assignment_and(c1.__getval(), c2.__getval()), __SECRET__)
        res._toclean = c1._toclean + c2._toclean
        c1._toclean.clear()
        c2._toclean.clear()
        return res

    def __or__(Assignment c1, Assignment c2):
        """`|` (or) operator on assignment.

        """
        res = Assignment(assignment_or(c1.__getval(), c2.__getval()), __SECRET__)
        res._toclean = c1._toclean + c2._toclean
        c1._toclean.clear()
        c2._toclean.clear()
        return res

class Selector(object):

    def variables(self):
        """Returns the variables on which to produce goals."""
        pass

    def _variables(self):
        v = self.variables()
        if not isinstance(v, Iterable):
            msg = "A selector must return an array of variables"
            raise NotImplementedError(msg)
        return len(v)

    def select(self):
        """Returns an index to next variable on which to produce a goal."""
        pass

    def _select(self):
        select = self.select()
        if not callable(select):
            return -1
        select = callback(select)
        __ml_callbacks[select.id] = select
        set_selector_select_callback(select.id, selector_select)
        return select.id

    def __call__(self, i):
        """Returns a goal for variables()[i]."""
        pass

    def _labelling(self):
        @callback
        def labelling_call(idx):
            res =  self(idx)
            assert isinstance(res, Goal)
            return res
        __ml_callbacks[labelling_call.id] = labelling_call
        set_selector_labelling_callback(labelling_call.id, selector_labelling)
        return labelling_call.id


cdef class Goal(object):

    cdef uintptr_t mlvalue
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
    def atomic(cls, cb):
        call = callback(cb)
        __ml_callbacks[call.id] = call
        set_atomic_callback(call.id, atomic_callback)
        res = cls(goals_atomic(call.id), __SECRET__)
#         print("atomic {}".format(call.id))
#         res.toclean(call.id)
#         res.keep(call)
        return res

    @classmethod
    def create(cls, custom):
        call = callback(custom)
        __ml_callbacks[ call.id ] = call
        set_goal_creator_callback(call.id, goal_creator)
        res = cls(goals_create(call.id), __SECRET__)
        return res

    @classmethod
    def unify(cls, Variable var, int i):
        print ("unify {} {}".format(var, i))
        res = cls(goals_unify(var.__getval(), i), __SECRET__)
        res.variables = [[var]]  # does it make sense?
        return res

    @classmethod
    def forall(cls, variables, strategy=None, assign=Assignment.indomain()):
        cdef long length = 0
        cdef uintptr_t* pt_vars
        cdef array.array _vars

        cdef uintptr_t c_assign = 0
        cdef uintptr_t c_strategy = 0 # default to no strategy

        if isinstance(variables, Selector):
            s = variables._select()
            v = variables._variables() 
            l = variables._labelling() 
            res = cls(goals_selector_forall(s, v, l), __SECRET__)
            res.keep(variables)
            return res

        if isinstance(assign, str):
            assign = eval("Assignment." + assign + "()")

        if not isinstance(assign, Assignment):
            raise ValueError("assign must refer to a valid Assignment")

        c_assign = assign.__getval() # default to indomain

        if isinstance(variables, Iterable):
            variables = list(variables)
            length = len(variables)
            for v in variables:
                msg = "All arguments must be variables"
                assert isinstance(v, Variable), msg

            _vars = array.array('Q', [v.__getval() for v in variables])
            pt_vars = <uintptr_t*>_vars.data.as_voidptr

            if strategy is not None:
                if isinstance(strategy, str):
                    strategy = eval("Strategy." + strategy + "()")

                # if isinstance(strategy, types.FunctionType):
                if callable(strategy):
                    strategy = Strategy.custom(strategy)

                msg = "The second argument is a strategy"
                assert isinstance(strategy, Strategy), msg
                c_strategy = strategy.__getval()

            if length < 1:
                raise TypeError("The argument list must be non empty")
            res = cls(goals_forall(c_strategy, pt_vars, length, c_assign),
                      __SECRET__)
            res.variables = [variables]
            res.keep([strategy, assign])
            return res

        raise TypeError("The argument must be iterable")

    @classmethod
    def minimize(cls, goal, expr, keep, mode="continue", *args, **kwargs):
        cdef Goal g_goal
        if not isinstance(goal, Goal):
            goal = Goal.forall(goal, *args)
        if isinstance(expr, Arith):
            expr = Variable(e2fd(expr.__getval()), __SECRET__)
        if not isinstance(expr, Variable):
            raise SyntaxError

        g_goal = goal & Goal.forall([expr])

        __ml_callbacks[keep.id] = keep
        set_onsol_callback(keep.id, on_solution_callback)

        d_mode = {'continue': mode_continue(),
                  'restart': mode_restart(),
                  'dichotomic': mode_dicho()}

        obj = goals_minimize(d_mode[mode], g_goal.__getval(),
                             expr.__getval(), keep.id)

        res = cls(obj, __SECRET__)
        res.variables = goal.variables
        res.toclean(keep.id)
        res.keep(g_goal._keep)
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

cdef class Stakbool(object):

    cdef uintptr_t mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value, secret):
        if secret != __SECRET__:
            raise ValueError("Invalid pointer value")
        self.mlvalue = value

    @classmethod
    def ref(cls, bint v):
        return cls(stak_bool_ref(v), __SECRET__)

    def get(self):
        return bool(stak_bool_get(self.mlvalue))

    def set(self, bint v):
        stak_bool_set(self.mlvalue, v)

def stak_trail(fun):
    fun = callback(fun)
    __ml_callbacks[fun.id] = fun
    set_atomic_callback(fun.id, atomic_callback)
    stak_trail_i(fun.id)


cdef object __ml_callbacks = {}

cdef void on_backtrack_callback(int i, int n):
    __ml_callbacks[i](n)

cdef int atomic_callback(int i):
    try:
        __ml_callbacks[i]()
        return 0
    except Stak_Fail:
        return -1

cdef void on_solution_callback(int i, int n):
    __ml_callbacks[i](n)

cdef void on_assign_callback(int i, uintptr_t v):
    __ml_callbacks[i](v)

cdef int selector_select(int i):
    return __ml_callbacks[i]()

# TODO
test_keep_goals = {}

cdef uintptr_t selector_labelling(int i, int l):
    res = __ml_callbacks[i](l)
    test_keep_goals[i] = res
    assert isinstance(res, Goal)
    return res.__getval()

cdef uintptr_t goal_creator(int i):
    try:
        res = __ml_callbacks[i]()
    except Stak_Fail:
        return 0
    test_keep_goals[i] = res
    assert isinstance(res, Goal)
    return res.__getval()

cdef int strategy_cb(int i, uintptr_t* v1, int length):
    return __ml_callbacks[i]([Variable(v1[j], __SECRET__)
                              for j in range(length)])

cdef int update_cb(int i, int a):
    try:
        return __ml_callbacks[i](a)
    except Stak_Fail:
        return -1

cdef void delay_cb(int i, uintptr_t c):
    __ml_callbacks[i](c)

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
    cdef uintptr_t* pt_vars
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

        obj = Goal.minimize(objective, minimize, keep, *args, **kwargs)
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

def maximize(goal, expr, *args, **kwargs):
    """
    The `maximize` function solves the problem defined by all posted
    constraints on variables passed in parameter, and minimizes the
    expression passed in parameter.

    It returns an empty list if the problem has no solution, and a pair
    `(optimal, values)` with `value`s appearing in the same order as
    `variables`.

    The `maximize` function raises TypeError if `variables` is not
    iterable or if expression is not valid.
    """
    res = minimize(goal, -expr, *args, **kwargs)
    if res.solved:
        res['evaluation'] *= -1
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
    try:
        cstr.post()
    except Stak_Fail:
        raise ValueError("The problem is overconstrained")

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
    cdef uintptr_t* pt_vars
    cdef array.array _vars
    cdef int _lazy = 1
    if lazy: _lazy = 0
    if isinstance(variables, Iterable):
        variables = list(variables)
        length = len(variables)
        if length < 2:
            raise TypeError("The argument list must be non empty")
        _vars = array.array('Q', range(length))
        for i in range(length):
            if isinstance(variables[i], Variable):
                _vars[i] = variables[i].__getval()
            elif isinstance(variables[i], Arith):
                _vars[i] = e2fd(variables[i].__getval())
            else:
                raise TypeError("Arguments must be variables or expressions")
        pt_vars = <uintptr_t*>_vars.data.as_voidptr
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

    if isinstance(min_val, range):
        if min_val.step == 1:
            return Variable.interval(min_val.start, min_val.stop - 1,
                                     *args, **kwargs)
    if isinstance(min_val, Arith):
        return Variable.create(min_val, *args, **kwargs)
    if isinstance(min_val, Cstr):
        return Variable.create(min_val, *args, **kwargs)
    if isinstance(min_val, Iterable):
        return Variable.create(min_val, *args, **kwargs)
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
    cdef uintptr_t* pt_vars
    cdef uintptr_t value
    cdef Variable v
    cdef array.array _vars
    numpy_msg = (
        "Only numpy arrays of variables, expressions, constraints and integers "
        "are accepted"
    )
    shape = len(variables),  # tuple[int]
    try:
        import numpy as np 

        if isinstance(variables, np.ndarray):
            if not all(
                any(isinstance(x, type_) for type_ in (int, Variable, Arith))
                for x in variables.ravel()
            ):
                raise TypeError(numpy_msg)

        shape = variables.shape
        variables = variables.ravel()

    except AttributeError, ImportError:
        pass
    if isinstance(variables, Iterable):
        variables = list(variables)
        length = len(variables)
        if length < 1:
            raise TypeError("The argument list must be non-empty")
        _vars = array.array('Q', range(length))
        for i in range(length):
            if isinstance(variables[i], Variable):
                _vars[i] = variables[i].__getval()
            elif isinstance(variables[i], Arith):
                _vars[i] = e2fd(variables[i].__getval())
            elif isinstance(variables[i], numbers.Integral):
                _vars[i] = e2fd(i2e(variables[i]))
            else:
                raise TypeError("The arguments must be variables or expressions")
        pt_vars = <uintptr_t*>_vars.data.as_voidptr
        value = fdarray_create(pt_vars, length)
        return Array(value, length, shape)
    raise TypeError("The argument must be iterable")

def make_constraint(update, delay):
    @callback
    def update_callback(int a):
        return update(a)
    @callback
    def delay_callback(c):
        c = Cstr(c, __SECRET__)
        delay(c)
        return
    __ml_callbacks[update_callback.id] = update_callback
    set_update_callback(update_callback.id, update_cb)
    __ml_callbacks[delay_callback.id] = delay_callback
    set_delay_callback(delay_callback.id, delay_cb)
    # TODO toclean(delay_callback.id, update_callback.id)
    return Cstr(cstr_create(update_callback.id, delay_callback.id), __SECRET__)


def facile_sum(*args):
    "A typed version of the sum built-in"
    return sum(*args)
