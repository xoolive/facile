#include <caml/mlvalues.h>

void init();
void fcl_destroy(value*);

value* val_interval(int i, int j);
char* val_name(value* in);
int val_isbound(value* in);
void val_minmax(value* in, int* min, int* max);

void val_refine(value* var, value* domain);
value* val_domain(value* var);
int domain_size(value* domain);
void domain_values(value* domain, int* values);
value* domain_remove(int element, value* domain);

value* interval_ismember(value* in, int inf, int sup);
value* sorting_sort(value* in);
value* gcc_cstr(value*, value**, long*, long);

value* fd2e(value* in);
value* e2fd(value* in);
value* i2e(int in);

value* arith_add(value*, value*);
value* arith_sub(value*, value*);
value* arith_mul(value*, value*);
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

value* strategy_minvalue();
value* strategy_mindomain();
value* strategy_minmin();

value* goals_success();
value* goals_fail();
value* goals_or(value*, value*);
value* goals_and(value*, value*);
value* goals_atomic(int i);
value* goals_unify(value* v, int i);
value* goals_forall(value*, value**, long, value*);
value* goals_minimize(value* goal, value* expr, int i);
int goals_solve(int, value*);

value* goals_forvar(int);

void fcl_interrupt(void);

void set_backtrack_callback(int i, void(*fct)(int, int));
void set_atomic_callback(int, void(*fct)(int));
void set_onsol_callback(int i, void(*fct)(int, int));

// too complicated without typedef
typedef value* (*g_fv)(int, value*, value*);
// void set_goal_forvar_callback(int i, (value*)(*fct)(int, value*, value*));
void set_goal_forvar_callback(int i, g_fv fct);

value* parse_array(value*, long*);
