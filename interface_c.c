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
  CLOSURE("Fd.interval");
  value v = caml_callback2(*closure, Val_int(i), Val_int(j));
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
  CLOSURE ("Fd.min_max");
  value a = caml_callback(*closure, *in);
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

value* i2e(int in)
{
  CLOSURE ("i2e");
  value a = caml_callback(*closure, Val_int(in));
  return fcl_wrap(a);
}

value* e2fd(value* in)
{
  CLOSURE ("e2fd");
  value a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value* fd2e(value* in)
{
  CLOSURE ("fd2e");
  value a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value* arith_add(value* val1, value* val2)
{
  CLOSURE("arith_add");
  value a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value* arith_sub(value* val1, value* val2)
{
  CLOSURE("arith_sub");
  value a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value* arith_mul(value* val1, value* val2)
{
  CLOSURE("arith_mul");
  value a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value* arith_abs(value* val1)
{
  CLOSURE("arith_abs");
  value a = caml_callback(*closure, *val1);
  return fcl_wrap(a);
}

value* cstr_lt(value* in1, value* in2)
{
  CLOSURE ("lt");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_le(value* in1, value* in2)
{
  CLOSURE ("le");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_eq(value* in1, value* in2)
{
  CLOSURE ("eq");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_ne(value* in1, value* in2)
{
  CLOSURE ("ne");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_gt(value* in1, value* in2)
{
  CLOSURE ("gt");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_ge(value* in1, value* in2)
{
  CLOSURE ("ge");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

char* cstr_name(value* in)
{
  CLOSURE ("Cstr.name");
  // dangerous !! ^^
  return String_val(caml_callback(*closure, *in));
}

void cstr_post(value* in)
{
  CLOSURE ("Cstr.post");
  caml_callback(*closure, *in);
  return;
}

value* cstr_or(value* in1, value* in2)
{
  CLOSURE ("Cstr.or");
  value a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value* cstr_alldiff(value** val, long len)
{
  CLOSURE("Cstr.alldiff");
  // À la barbare
  value array = caml_alloc(len, 0);
  size_t i = 0;
  for(; i< len; ++i)
    Store_field(array, i, val[i][0]);
  value a = caml_callback(*closure, array);
  return fcl_wrap(a);
}

int goals_array_solve(value** val, long len, heuristic h)
{
  CLOSURE("Goals.Array.solve");
  // À la barbare
  value array = caml_alloc(len, 0);
  size_t i = 0;
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  return Bool_val(caml_callback2(*closure, array, Val_int(h)));
}

int goals_array_solve_bt(value** val, long len, heuristic h, long* bt)
{
  CLOSURE("Goals.Array.solve_bt");
  // À la barbare
  value array = caml_alloc(len, 0);
  size_t i = 0;
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  value v = caml_callback2(*closure, array, Val_int(h));
  *bt = Int_val(Field(v, 1));
  return Bool_val(Field(v, 0));
}


int goals_minimize(value** val, long len, value* expr, long* solution,
                   long* optimal)
{
  CLOSURE("Goals.minimize");
  // À la barbare
  value array = caml_alloc(len, 0);
  size_t i = 0;
  for(; i < len; ++i)
    Store_field(array, i, val[i][0]);
  value res = caml_callback2(*closure, array, *expr);
  if (res == Val_int(0))
    return 0;
  for (i=0; i < Wosize_val(Field(Field(res, 0), 1)); ++i)
    solution[i] = Int_val(Field(Field(Field(res,0), 1), i));
  *optimal = Int_val(Field(Field(res,0), 0));
  return 1;
}


