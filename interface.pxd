ctypedef long value

cdef extern from "interface.h":

    void init()
    void fcl_destroy(value v)

    value val_interval(int, int)
    void val_minmax(value, int*, int*)
    char* val_name(value)
    int val_isbound(value)

    value fd2e(value)
    value e2fd(value)
    value i2e(int)

    value arith_add(value, value)
    value arith_sub(value, value)
    value arith_mul(value, value)
    value arith_abs(value)

    value cstr_lt(value, value)
    value cstr_le(value, value)
    value cstr_eq(value, value)
    value cstr_ne(value, value)
    value cstr_gt(value, value)
    value cstr_ge(value, value)

    char* cstr_name(value)
    void cstr_post(value)
    value cstr_or(value, value)
    value cstr_alldiff(value*, long)

    int goals_array_solve(value*, long, int)
    int goals_array_solve_bt(value*, long, int, long*)
    int goals_minimize(value*, long, value, long*, long*)
