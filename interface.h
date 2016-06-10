#include <caml/mlvalues.h>

typedef enum h {
  NO_HEURISTIC,
  MIN_SIZE,
  MIN_VALUE,
  MIN_MIN } heuristic;

void init();

value* val_interval(int i, int j);
char* val_name(value* in);
int val_isbound(value* in);
void val_minmax(value* in, int* min, int* max);

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
value* cstr_alldiff(value**, long);
value* cstr_boolean(value*);

int goals_array_solve(value**, long, heuristic);
value* goals_array_solve_all(value**, long);
value* parse_array(value*, long*);
int goals_array_solve_bt(value**, long, heuristic, long*);
int goals_minimize(value**, long, value*, long*, long*);
