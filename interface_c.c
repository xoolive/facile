#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdio.h>

#include "interface.h"

#define CLOSURE(A)\
  static value * closure = NULL;\
  if (closure == NULL)\
    closure = caml_named_value(A);

void init() {
  static char* argv[2] = { "python", NULL };
  caml_startup(argv);
}

value* fcl_wrap(value v)
{
  value* res = malloc(sizeof(value*));
  *res = v;
  caml_register_global_root(res);
  return res;
}

void fcl_destroy(value* v)
{
  caml_remove_global_root(v);
  free(v);
}

value* val_interval(int i, int j)
{
  value v;
  CLOSURE("Fd.interval");
  v = caml_callback2(*closure, Val_int(i), Val_int(j));
  return fcl_wrap(v);
}

char* val_name(value* in)
{
  CLOSURE ("Fd.name");
  // dangerous !! ^^
  return String_val(caml_callback(*closure, *in));
}

void val_minmax(value* in, int* min, int* max)
{
  value a;
  CLOSURE ("Fd.min_max");
  a = caml_callback(*closure, *in);
  *min = Int_val(Field(a, 0));
  *max = Int_val(Field(a, 1));
  return;
}

int val_isbound(value* in)
{
  CLOSURE ("Fd.is_bound");
  // PAS dangerous !! ^^
  return Bool_val(caml_callback(*closure, *in));
}

value* interval_ismember(value* in, int inf, int sup)
{
  value a;
  CLOSURE ("Interval.is_member");
  a = caml_callback3(*closure, *in, Val_int(inf), Val_int(sup));
  return fcl_wrap(a);
}

value* sorting_sort(value* in)
{
  value a;
  CLOSURE ("Sorting.sort");
  a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value* i2e(int in)
{
  value a;
  CLOSURE ("i2e");
  a = caml_callback(*closure, Val_int(in));
  return fcl_wrap(a);
}

value* e2fd(value* in)
{
  value a;
  CLOSURE ("e2fd");
  a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value* fd2e(value* in)
{
  value a;
  CLOSURE ("fd2e");
  a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value* arith_add(value* val1, value* val2)
{
  value a;
  CLOSURE("arith_add");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value* arith_sub(value* val1, value* val2)
{
  value a;
  CLOSURE("arith_sub");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value* arith_mul(value* val1, value* val2)
{
  value a;
  CLOSURE("arith_mul");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value* arith_abs(value* val1)
{
  value a;
  CLOSURE("arith_abs");
  a = caml_callback(*closure, *val1);
  return fcl_wrap(a);
}

value* cstr_lt(value* in1, value* in2)
{
  value a;
  CLOSURE ("lt");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_le(value* in1, value* in2)
{
  value a;
  CLOSURE ("le");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_eq(value* in1, value* in2)
{
  value a;
  CLOSURE ("eq");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_ne(value* in1, value* in2)
{
  value a;
  CLOSURE ("ne");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_gt(value* in1, value* in2)
{
  value a;
  CLOSURE ("gt");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_ge(value* in1, value* in2)
{
  value a;
  CLOSURE ("ge");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

char* cstr_name(value* in)
{
  CLOSURE ("Cstr.name");
  // dangerous !! ^^
  return String_val(caml_callback(*closure, *in));
}

int cstr_post(value* in)
{
  value v;
  CLOSURE ("Cstr.post");
  v = caml_callback_exn(*closure, *in);
  return Is_exception_result(v);
}

value* cstr_or(value* in1, value* in2)
{
  value a;
  CLOSURE ("Cstr.or");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_and(value* in1, value* in2)
{
  value a;
  CLOSURE ("Cstr.and");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_xor(value* in1, value* in2)
{
  value a;
  CLOSURE ("Cstr.xor");
  a = caml_callback2_exn(*closure, *in1, *in2);
  if Is_exception_result(a) return 0;
  return fcl_wrap(a);
}

value* cstr_not(value* in)
{
  value a;
  CLOSURE("Cstr.not");
  a = caml_callback_exn(*closure, *in);
  if Is_exception_result(a) return 0;
  return fcl_wrap(a);
}

value* cstr_alldiff(value** val, long len)
{
  value array, a;
  size_t i = 0;
  CLOSURE("Cstr.alldiff");
  // À la barbare
  array = caml_alloc(len, 0);
  for(; i< len; ++i)
    Store_field(array, i, val[i][0]);
  a = caml_callback(*closure, array);
  return fcl_wrap(a);
}

value* cstr_boolean(value* cstr)
{
  value a;
  CLOSURE("Cstr.boolean");
  a = caml_callback_exn(*closure, *cstr);
  if Is_exception_result(a) return 0;
  return fcl_wrap(a);
}

value* gcc_cstr(value* array, value** cards, long* values, long len)
{
  value a, distribution;
  size_t i = 0;
  CLOSURE("Gcc.cstr");
  distribution = caml_alloc(len, 0);
  for(; i<len; ++i)
  {
    value b = caml_alloc(2, 0);
    Store_field(b, 0, cards[i]);
    Store_field(b, 1, Val_long(values[i]));

    Store_field(distribution, i, b);
  }
  a = caml_callback2(*closure, *array, distribution);
  return fcl_wrap(a);
}

value* fdarray_create(value** val, long len)
{
  value array;
  size_t i = 0;
  // À la barbare
  array = caml_alloc(len, 0);
  for(; i< len; ++i)
    Store_field(array, i, val[i][0]);
  return fcl_wrap(array);
}

void fdarray_read(value* val1, value** val2)
{
  size_t i = 0;
  for (; i<Wosize_val(*val1); ++i)
    val2[i] = fcl_wrap(Field(*val1, i));
}

value* fdarray_min(value* in1)
{
  value a;
  CLOSURE("FdArray.min");
  a = caml_callback_exn(*closure, *in1);
  if Is_exception_result(a) return 0;
  return fcl_wrap(a);
}

value* fdarray_max(value* in1)
{
  value a;
  CLOSURE("FdArray.max");
  a = caml_callback_exn(*closure, *in1);
  if Is_exception_result(a) return 0;
  return fcl_wrap(a);
}

value* fdarray_get(value* in1, value* in2)
{
  value a;
  CLOSURE("FdArray.get");
  a = caml_callback2_exn(*closure, *in1, *in2);
  if Is_exception_result(a) return 0;
  return fcl_wrap(a);
}

int goals_array_solve(value** val, long len, heuristic h)
{
  value array;
  size_t i = 0;
  CLOSURE("Goals.Array.solve");
  // À la barbare
  array = caml_alloc(len, 0);
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  return Bool_val(caml_callback2(*closure, array, Val_int(h)));
}

value* goals_array_solve_all(value** val, long len)
{
  value array, all;
  size_t i = 0;
  CLOSURE("Gools.Array.solve_all");
  // À la barbare
  array = caml_alloc(len, 0);
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  all = caml_callback(*closure, array);
  return fcl_wrap(all);
}

value* parse_array(value* list, long* res)
{
  size_t i = 0;
  value head;
  value* tmp;
  if ( *list == Val_emptylist )
  {
    fcl_destroy(list);
    return 0;
  }
  head = Field(*list, 0);
  for (i = 0; i < Wosize_val(head); ++i)
    res[i] = Int_val(Field(head, i));
  tmp = list;
  list = fcl_wrap(Field(*list, 1));
  fcl_destroy(tmp);
  return list;
}

int goals_array_solve_bt(value** val, long len, heuristic h, long* bt)
{
  value array, v;
  size_t i = 0;
  CLOSURE("Goals.Array.solve_bt");
  // À la barbare
  array = caml_alloc(len, 0);
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  v = caml_callback2(*closure, array, Val_int(h));
  *bt = Int_val(Field(v, 1));
  return Bool_val(Field(v, 0));
}


int goals_minimize(value** val, long len, value* expr, long* solution,
                   long* optimal)
{
  value array, res;
  size_t i = 0;
  CLOSURE("Goals.minimize");
  // À la barbare
  array = caml_alloc(len, 0);
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  res = caml_callback2(*closure, array, *expr);
  if (res == Val_int(0))
    return 0;
  for (i=0; i < Wosize_val(Field(Field(res, 0), 1)); ++i)
    solution[i] = Int_val(Field(Field(Field(res,0), 1), i));
  *optimal = Int_val(Field(Field(res,0), 0));
  return 1;
}


