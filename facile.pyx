# distutils: language = c
# -*- coding: utf-8 -*-

from interface cimport *

# Initialize OCaml
init()

class Heuristic:
    No = 0
    Min_size = 1
    Min_value = 2
    Min_min = 3


cdef class Variable(object):

    cdef value mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__ (self, int i, int j):
        self.mlvalue = val_interval(i, j)

    def __repr__(self):
        cdef int vmin, vmax
        val_minmax(self.mlvalue, &vmin, &vmax)
        if (val_isbound(self.mlvalue) == 1) :
            return "value bound to %d" % (vmin)
        else:
            return "%s in [%d,%d]" % (<bytes> val_name(self.mlvalue), vmin, vmax)

    def __getval(self):
        return self.mlvalue

    def value(self):
        """ Renvoie la valeur numérique de la variable.
        Renvoie None si le solver n'a pas trouvé de solution. """
        cdef int vmin, vmax
        val_minmax(self.mlvalue, &vmin, &vmax)
        if (val_isbound(self.mlvalue)==1):
            return vmin
        else:
            return None

    def __richcmp__(self, value, integer):
    # < 0 # <= 1 # == 2 # != 3 # > 4 # >= 5
        if integer == 0:
            return self.__lt(value)
        if integer == 1:
            return self.__le(value)
        if integer == 2:
            return self.__eq(value)
        if integer == 3:
            return self.__ne(value)
        if integer == 4:
            return self.__gt(value)
        if integer == 5:
            return self.__ge(value)
        return None

    def __lt(self, fd):
        if isinstance(fd, Arith):
            c = cstr_lt(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_lt(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_lt(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError

    def __le(self, fd):
        if isinstance(fd, Arith):
            c = cstr_le(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_le(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_le(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError

    def __eq(self, fd):
        if isinstance(fd, Arith):
            c = cstr_eq(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_eq(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_eq(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError

    def __ne(self, fd):
        if isinstance(fd, Arith):
            c = cstr_ne(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_ne(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_ne(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError

    def __gt(self, fd):
        if isinstance(fd, Arith):
            c = cstr_gt(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_gt(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_gt(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError

    def __ge(self, fd):
        if isinstance(fd, Arith):
            c = cstr_ge(fd2e(self.__getval()), fd.__getval())
            return Cstr(c)
        if isinstance(fd, Variable):
            c = cstr_ge(fd2e(self.__getval()), fd2e(fd.__getval()))
            return Cstr(c)
        if isinstance(fd, int):
            c = cstr_ge(fd2e(self.__getval()), i2e(fd))
            return Cstr(c)
        raise TypeError

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
        raise TypeError

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
        raise TypeError

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
        raise TypeError

    def __abs__(a):
        c = arith_abs(fd2e(a.__getval()))
        return Arith(c)


cdef class Arith(object):

    cdef value mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value):
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def __richcmp__(self, value, integer):
    # < 0 # <= 1 # == 2 # != 3 # > 4 # >= 5
        if integer == 0:
            return self.__lt(value)
        if integer == 1:
            return self.__le(value)
        if integer == 2:
            return self.__eq(value)
        if integer == 3:
            return self.__ne(value)
        if integer == 4:
            return self.__gt(value)
        if integer == 5:
            return self.__ge(value)
        return None

    def __lt(self, value):
        if isinstance(value, Arith):
            c = cstr_lt(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_lt(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_lt(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError

    def __le(self, value):
        if isinstance(value, Arith):
            c = cstr_le(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_le(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_le(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError

    def __eq(self, value):
        if isinstance(value, Arith):
            c = cstr_eq(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_eq(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_eq(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError

    def __ne(self, value):
        if isinstance(value, Arith):
            c = cstr_ne(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_ne(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_ne(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError

    def __gt(self, value):
        if isinstance(value, Arith):
            c = cstr_gt(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_gt(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_gt(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError

    def __ge(self, value):
        if isinstance(value, Arith):
            c = cstr_ge(self.__getval(), value.__getval())
            return Cstr(c)
        if isinstance(value, Variable):
            c = cstr_ge(self.__getval(), fd2e(value.__getval()))
            return Cstr(c)
        if isinstance(value, int):
            c = cstr_ge(self.__getval(), i2e(value))
            return Cstr(c)
        raise TypeError


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
        raise TypeError

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
        raise TypeError

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
        raise TypeError

    def __abs__(a):
        c = arith_abs(a.__getval())
        return Arith(c)

cdef class Cstr(object):

    cdef value mlvalue

    def __dealloc__(self):
        fcl_destroy(self.mlvalue)

    def __cinit__(self, value):
        self.mlvalue = value

    def __getval(self):
        return self.mlvalue

    def __repr__(self):
        return (<bytes> cstr_name(self.__getval()))

    def __or__(self, Cstr c):
        return Cstr(cstr_or(self.__getval(), c.__getval()))

    def post(self):
        cstr_post(self.__getval())

cimport cpython
import numpy as np
cimport numpy as cnp

def solve(variables, backtrack=False, heuristic=Heuristic.No):
    """
    solve(variables, backtrack=False, heuristic=Heuristic.No)

    La fonction solve résout le problème défini par les contraintes posées
    précédemment sur les variables passées en paramètre.

    Elle renvoie par défaut True si le problème a une solution et False sinon.

    Si backtrack est à True, elle renvoie deux arguments res et bt, le premier
    étant le même booléen que présenté ci-dessus, et le second étant le nombre
    de backtracks opérés jusqu'à la première solution.

    Le dernier argument se nomme heuristic et permet de choisir parmi 4
    stratégies de choix de la prochaine variable à explorer lors du parcours de
    l'arbre de recherche:

    - par défaut, si on ne précise rien, c'est facile.Heuristic.No qui est
    choisi;
    - avec facile.Heuristic.Min_size, l'algorithme de recherche choisit pour
    prochaine variable celle au domaine le plus petit;
    - avec facile.Heuristic.Min_value, l'algorithme de recherche choisit pour
    prochaine variable celle dont le domaine a la valeur minimale la plus petite;
    - avec facile.Heuristic.Min_min, l'algorithme de recherche choisit pour
    prochaine variable celle au domaine le plus petit et à la valeur minimale
    de domaine la plus petite.

    La fonction soulève une exception SyntaxError si variables n'est pas itérable.
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
        npvars = np.array([v.__getval() for v in variables])
        pt_vars = cnp.PyArray_DATA(npvars)
        length = len(variables)
        if length < 1:
            return SyntaxError
        if (backtrack==False):
            return goals_array_solve(<value*> pt_vars, length, heuristic) == 1
        else:
            res = goals_array_solve_bt(<value*> pt_vars, length, heuristic, &bt)
            return (res, bt)
    raise SyntaxError

def minimize(variables, expr):
    """
    minimize(variables, expression)

    La fonction minimize résout le problème défini par les contraintes posées
    précédemment sur les variables passées en paramètre, en minimisant
    l'expression passée en paramètre.

    Elle renvoie un vecteur vide si le problème n'a pas de solution, est un
    couple (optimal, valeurs) avec les valeurs dans le même ordre que variables.

    La fonction soulève une exception SyntaxError si variables n'est pas
    itérable ou si expression n'est pas valide.

    >>> a = variable(0, 10)
    >>> b = variable(0, 10)
    >>> constraint(a + b == 10)
    >>> minimize([a, b], a*a + b*b)
    (55, array([5, 5]))
    """
    cdef long length
    cdef long optimal
    if not isinstance(expr, Arith):
        raise SyntaxError
    if cpython.PySequence_Check(variables):
        npvars = np.array([v.__getval() for v in variables])
        pt_vars = cnp.PyArray_DATA(npvars)
        length = len(variables)
        if length < 1:
            return SyntaxError
        sol = np.zeros(length, long)
        pt_sol = cnp.PyArray_DATA(sol)
        if goals_minimize(<value*> pt_vars, length, expr.__getval(),
                          <long*> pt_sol, &optimal) == 1 :
            return (optimal, sol)
        else:
            return []
    raise SyntaxError


def constraint(cstr):
    """
    constraint(cstr)

    Cette fonction permet de définir une contrainte pour le solveur, exprimée de
    manière "naturelle" avec des expressions sur les variables.

    La fonction soulève une exception SyntaxError si cstr n'est pas une
    contrainte.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> constraint(a != b)
    """

    if not isinstance(cstr, Cstr):
        raise SyntaxError
    cstr.post()


def alldifferent(variables):
    """
    alldifferent(variables)

    Cette fonction permet de définir une contrainte globale alldifferent pour le
    solveur.

    La fonction soulève une exception SyntaxError si variables n'est pas
    itérable, et si variables ne contient pas au moins 2 variables.

    >>> a = variable(0, 1)
    >>> b = variable(0, 1)
    >>> alldifferent([a, b])
    """

    cdef long length
    cdef value* pt_vars
    if cpython.PySequence_Check(variables):
        if len(variables) < 2:
            raise SyntaxError
        npvars = None
        if isinstance(variables[0], Variable):
            npvars = np.array([v.__getval() for v in variables])
        if isinstance(variables[0], Arith):
            npvars = np.array([e2fd(v.__getval()) for v in variables])
        if npvars is None:
            raise TypeError
        pt_vars = <value*> cnp.PyArray_DATA(npvars)
        length = len(variables)
        cstr_post(cstr_alldiff(pt_vars, length))
        return
    raise SyntaxError

def variable(a, b):
    """
    variable(min, max)

    Cette fonction crée une variable à valeurs entière sur un intervalle
    discret, dont les bornes min et max sont passées en paramètre.

    >>> variable(0, 2)
    _0 in [0,2]
    """
    return Variable(a, b)
