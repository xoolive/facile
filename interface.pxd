cdef extern from "interface.h":

    void init()
    void fcl_destroy(long v)

    long val_interval(int, int)
    void val_minmax(long, int*, int*)
    char* val_name(long)
    int val_isbound(long)

    void val_refine(long var, long domain)
    long val_domain(long var)
    int domain_size(long domain)
    void domain_values(long domain, int* values)
    long domain_remove(int element, long domain)

    long interval_ismember(long, int, int)
    long sorting_sort(long)
    long gcc_cstr(long, long*, long*, long)

    long fd2e(long)
    long e2fd(long)
    long i2e(int)

    long arith_add(long, long)
    long arith_sub(long, long)
    long arith_mul(long, long)
    long arith_abs(long)

    long cstr_lt(long, long)
    long cstr_le(long, long)
    long cstr_eq(long, long)
    long cstr_ne(long, long)
    long cstr_gt(long, long)
    long cstr_ge(long, long)

    char* cstr_name(long)
    int cstr_post(long)
    long cstr_or(long, long)
    long cstr_and(long, long)
    long cstr_xor(long, long)
    long cstr_not(long)
    long cstr_alldiff(long*, long, int)
    long cstr_boolean(long)

    long fdarray_create(long*, long)
    void fdarray_read(long, long*)
    long fdarray_get(long, long)
    long fdarray_max(long)
    long fdarray_min(long)

    long strategy_minvalue()
    long strategy_mindomain()
    long strategy_minmin()

    long goals_success()
    long goals_fail()
    long goals_or(long, long)
    long goals_and(long, long)
    long goals_forall(long, long*, long, long)
    long goals_atomic(int i)
    long goals_unify(long v, int i)
    long goals_minimize(long goal, long expr, int i)
    int goals_solve(int, long)

    long goals_forvar(int i)

    void fcl_interrupt()

    void set_backtrack_callback(int, void(*fct)(int, int))
    void set_atomic_callback(int, void(*fct)(int))
    void set_onsol_callback(int, void(*fct)(int, int))
    void set_goal_forvar_callback(int i, long(*fct)(int, long, long))

    long parse_array(long, long*)
