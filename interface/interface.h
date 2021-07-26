#include <caml/mlvalues.h>

void init();
void fcl_destroy(value*);
void stak_fail();

value* val_interval(int i, int j);
char* val_name(value* in);
int val_isbound(value* in);
value* val_create(value* v);
value* val_domain(value* var);
int val_min(value* in);
int val_max(value* in);
int val_refine(value* val, value* domain);
void val_delay(value* val, value** events, int len, value* cstr);

int domain_size(value* domain);
void domain_values(value* domain, int* values);
value* domain_create(int* values, long size);
value* domain_removelow(int, value* domain);
value* domain_removeup(int, value* domain);

value* fd_onmax();
value* fd_onmin();
value* fd_onrefine();
value* fd_onsubst();

value* interval_ismember(value* in, int inf, int sup);
value* sorting_sort(value* in);
value* gcc_cstr(value*, value**, long*, long);

value* fd2e(value* in);
value* e2fd(value* in);
value* i2e(int in);

value* arith_add(value*, value*);
value* arith_sub(value*, value*);
value* arith_mul(value*, value*);
value* arith_div(value*, value*);
value* arith_mod(value*, value*);
value* arith_abs(value*);

value* cstr_lt(value* in1, value* in2);
value* cstr_le(value* in1, value* in2);
value* cstr_eq(value* in1, value* in2);
value* cstr_ne(value* in1, value* in2);
value* cstr_gt(value* in1, value* in2);
value* cstr_ge(value* in1, value* in2);

value* fdarray_create(value**val, long len);
void fdarray_read(value* in1, value** in2);
value* fdarray_get(value* in1, value* in2);
value* fdarray_max(value* in1);
value* fdarray_min(value* in1);

char* cstr_name(value* in);
int cstr_post(value* in);
value* cstr_or(value*, value*);
value* cstr_and(value*, value*);
value* cstr_xor(value*, value*);
value* cstr_not(value*);
value* cstr_alldiff(value**, long, int);
value* cstr_boolean(value*);
value* cstr_create(int, int);

value* strategy_minmin();
value* strategy_mindomain();
value* strategy_queen();
value* strategy_callback(int);

value* goals_success();
value* goals_fail();
value* goals_or(value*, value*);
value* goals_and(value*, value*);
value* goals_atomic(int i);
value* goals_create(int i);
value* goals_unify(value* v, int i);
value* goals_forall(value*, value**, long, value*);
value* goals_minimize(value* mode, value* goal, value* expr, int i);
int goals_solve(int, value*);

value* mode_continue();
value* mode_restart();
value* mode_dicho();

value* goals_selector_forall(long select, long len, int labelling);

value* assignment_indomain();
value* assignment_assign();
value* assignment_dichotomic();
value* assignment_atomic(int i);
value* assignment_and(value*, value*);
value* assignment_or(value*, value*);

void set_backtrack_callback(int i, void(*fct)(int, int));
void set_atomic_callback(int, int(*fct)(int));
void set_onsol_callback(int i, void(*fct)(int, int));
void set_assign_callback(int i, void(*fct)(int, value*));
void set_strategy_callback(int i, int(*fct)(int, value**, int));
void set_update_callback(int i, int(*fct)(int, int));
void set_delay_callback(int i, void(*fct)(int, value*));
void set_selector_select_callback(int i, int (*fct)(int));
void set_selector_labelling_callback(int i, long(*fct)(int, int));
void set_goal_creator_callback(int i, long(*fct)(int));

value* stak_bool_ref(int b);
int stak_bool_get(value* v);
void stak_bool_set(value* v, int b);
void stak_trail_i(int i);

value* parse_array(value*, long*);
