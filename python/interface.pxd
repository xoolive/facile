from libc.stdint cimport uintptr_t


cdef extern from "./binding/facile.h":

    void init()
    void fcl_destroy(uintptr_t v)

    void stak_fail()

    uintptr_t val_interval(int, int)
    uintptr_t val_create(uintptr_t v)
    char* val_name(uintptr_t)
    int val_isbound(uintptr_t)
    int val_min(uintptr_t)
    int val_max(uintptr_t)
    int val_refine(uintptr_t, uintptr_t)
    void val_delay(uintptr_t, uintptr_t*, long, uintptr_t)

    uintptr_t val_domain(uintptr_t var)
    int domain_size(uintptr_t domain)
    void domain_values(uintptr_t domain, int* values)
    uintptr_t domain_create(int* values, long size)
    uintptr_t domain_removelow(int, uintptr_t)
    uintptr_t domain_removeup(int, uintptr_t)

    uintptr_t fd_onmax()
    uintptr_t fd_onmin()
    uintptr_t fd_onrefine()
    uintptr_t fd_onsubst()

    uintptr_t interval_ismember(uintptr_t, int, int)
    uintptr_t sorting_sort(uintptr_t)
    uintptr_t gcc_cstr(uintptr_t, uintptr_t*, long*, long)

    uintptr_t fd2e(uintptr_t)
    uintptr_t e2fd(uintptr_t)
    uintptr_t i2e(int)

    uintptr_t arith_add(uintptr_t, uintptr_t)
    uintptr_t arith_sub(uintptr_t, uintptr_t)
    uintptr_t arith_mul(uintptr_t, uintptr_t)
    uintptr_t arith_div(uintptr_t, uintptr_t)
    uintptr_t arith_mod(uintptr_t, uintptr_t)
    uintptr_t arith_abs(uintptr_t)

    uintptr_t cstr_lt(uintptr_t, uintptr_t)
    uintptr_t cstr_le(uintptr_t, uintptr_t)
    uintptr_t cstr_eq(uintptr_t, uintptr_t)
    uintptr_t cstr_ne(uintptr_t, uintptr_t)
    uintptr_t cstr_gt(uintptr_t, uintptr_t)
    uintptr_t cstr_ge(uintptr_t, uintptr_t)

    char* cstr_name(uintptr_t)
    int cstr_post(uintptr_t)
    uintptr_t cstr_or(uintptr_t, uintptr_t)
    uintptr_t cstr_and(uintptr_t, uintptr_t)
    uintptr_t cstr_xor(uintptr_t, uintptr_t)
    uintptr_t cstr_not(uintptr_t)
    uintptr_t cstr_alldiff(uintptr_t*, uintptr_t, long)
    uintptr_t cstr_boolean(uintptr_t)
    uintptr_t cstr_create(int, int)

    uintptr_t fdarray_create(uintptr_t*, long)
    void fdarray_read(uintptr_t, uintptr_t*)
    uintptr_t fdarray_get(uintptr_t, uintptr_t)
    uintptr_t fdarray_max(uintptr_t)
    uintptr_t fdarray_min(uintptr_t)
    uintptr_t fdarray_sum(uintptr_t)

    uintptr_t strategy_lexicographic()
    uintptr_t strategy_minmin()
    uintptr_t strategy_mindomain()
    uintptr_t strategy_queen()
    uintptr_t strategy_callback(int)

    uintptr_t goals_success()
    uintptr_t goals_fail()
    uintptr_t goals_or(uintptr_t, uintptr_t)
    uintptr_t goals_and(uintptr_t, uintptr_t)
    uintptr_t goals_forall(uintptr_t, uintptr_t*, uintptr_t, uintptr_t)
    uintptr_t goals_atomic(int i)
    uintptr_t goals_create(int i)
    uintptr_t goals_unify(uintptr_t v, int i)
    uintptr_t goals_minimize(uintptr_t mode, uintptr_t goal, uintptr_t expr, int i)
    int goals_solve(int, uintptr_t)

    uintptr_t mode_continue();
    uintptr_t mode_restart();
    uintptr_t mode_dicho();

    uintptr_t goals_selector_forall(uintptr_t, uintptr_t, int)

    uintptr_t assignment_indomain()
    uintptr_t assignment_assign()
    uintptr_t assignment_dichotomic()
    uintptr_t assignment_atomic(int i)
    uintptr_t assignment_and(uintptr_t, uintptr_t)
    uintptr_t assignment_or(uintptr_t, uintptr_t)

    void set_backtrack_callback(int, void(*fct)(int, int))
    void set_atomic_callback(int, int(*fct)(int))
    void set_onsol_callback(int, void(*fct)(int, int))
    void set_assign_callback(int, void(*fct)(int, uintptr_t))
    void set_strategy_callback(int i, int(*fct)(int, uintptr_t*, int))
    void set_update_callback(int i, int(*fct)(int, int))
    void set_delay_callback(int i, void(*fct)(int, uintptr_t))
    void set_selector_select_callback(int i, int (*fct)(int))
    void set_selector_labelling_callback(int i, uintptr_t (*fct)(int, int))
    void set_goal_creator_callback(int i, uintptr_t(*fct)(int))

    uintptr_t stak_bool_ref(int b)
    int stak_bool_get(uintptr_t v)
    void stak_bool_set(uintptr_t v, int b)
    void stak_trail_i(int i)

    uintptr_t parse_array(uintptr_t, uintptr_t*)
