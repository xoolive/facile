#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <stdio.h>
#include <stdlib.h>

#include "facile.h"

#define CLOSURE(A)                 \
  static value *closure = NULL;    \
  if (closure == NULL)             \
  {                                \
    closure = caml_named_value(A); \
  }

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
static value Val_some(value v)
{
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

void *callbacks[204800];

void init()
{
  static char *argv[2] = {"python", NULL};
  caml_startup(argv);
}

void stak_fail()
{
  CLOSURE("Facile.Stak.Fail");
  caml_raise_with_string(*closure, "Python closure");
}

value *fcl_wrap(value v)
{
  value *res = (value *)malloc(sizeof(value));
  *res = v;
  caml_register_global_root(res);
  return res;
}

void fcl_destroy(value *v)
{
  caml_remove_global_root(v);
  free(v);
}

value *val_interval(int i, int j)
{
  value v;
  CLOSURE("Fd.interval");
  v = caml_callback2(*closure, Val_int(i), Val_int(j));
  return fcl_wrap(v);
}

value *val_create(value *v)
{
  value a;
  CLOSURE("Fd.create");
  a = caml_callback(*closure, *v);
  return fcl_wrap(a);
}

char *val_name(value *in)
{
  CLOSURE("Fd.name");
  // dangerous !! ^^
  return String_val(caml_callback(*closure, *in));
}

int val_min(value *in)
{
  value a;
  CLOSURE("Fd.min");
  a = caml_callback(*closure, *in);
  return Int_val(a);
}

int val_max(value *in)
{
  value a;
  CLOSURE("Fd.max");
  a = caml_callback(*closure, *in);
  return Int_val(a);
}

int val_refine(value *val, value *domain)
{
  value v;
  CLOSURE("Fd.refine");
  v = caml_callback2_exn(*closure, *val, *domain);
  return Is_exception_result(v);
}

void val_delay(value *val, value **events, unsigned long len, value *cstr)
{
  value array;
  size_t i = 0;
  CLOSURE("Fd.delay")
  array = caml_alloc(len, 0);
  for (; i < len; ++i)
    Store_field(array, i, events[i][0]);
  caml_callback3(*closure, array, *val, *cstr);
  return;
}

void val_minmax(value *in, int *min, int *max)
{
  value a;
  CLOSURE("Fd.min_max");
  a = caml_callback(*closure, *in);
  *min = Int_val(Field(a, 0));
  *max = Int_val(Field(a, 1));
  return;
}

int val_isbound(value *in)
{
  CLOSURE("Fd.is_bound");
  // PAS dangerous !! ^^
  return Bool_val(caml_callback(*closure, *in));
}

value *val_domain(value *var)
{
  value v;
  CLOSURE("Fd.dom");
  v = caml_callback(*closure, *var);
  return fcl_wrap(v);
}

int domain_size(value *domain)
{
  value v;
  CLOSURE("Domain.size");
  v = caml_callback(*closure, *domain);
  return Int_val(v);
}

void domain_values(value *domain, int *values)
{
  value v;
  size_t i = 0;
  CLOSURE("Domain.values");
  v = caml_callback(*closure, *domain);
  while (v != Val_emptylist)
  {
    values[i] = Int_val(Field(v, 0)); /* accessing the head */
    v = Field(v, 1);                  /* point to the tail for next loop */
    ++i;
  }
}

value *domain_create(int *val, unsigned long len)
{
  value v, array;
  size_t i = 0;
  CLOSURE("Domain.create");
  // À la barbare
  array = caml_alloc(len, 0);
  for (; i < len; ++i)
    Store_field(array, i, Val_int(val[i]));
  v = caml_callback(*closure, array);
  return fcl_wrap(v);
}

value *domain_removelow(int lb, value *domain)
{
  value v;
  CLOSURE("Domain.remove_low");
  v = caml_callback2(*closure, Val_int(lb), *domain);
  return fcl_wrap(v);
}

value *domain_removeup(int ub, value *domain)
{
  value v;
  CLOSURE("Domain.remove_up");
  v = caml_callback2(*closure, Val_int(ub), *domain);
  return fcl_wrap(v);
}

value *fd_onmax()
{
  CLOSURE("Fd.on_max");
  return closure;
}

value *fd_onmin()
{
  CLOSURE("Fd.on_min");
  return closure;
}

value *fd_onrefine()
{
  CLOSURE("Fd.on_refine");
  return closure;
}

value *fd_onsubst()
{
  CLOSURE("Fd.on_subst");
  return closure;
}

value *interval_ismember(value *in, int inf, int sup)
{
  value a;
  CLOSURE("Interval.is_member");
  a = caml_callback3(*closure, *in, Val_int(inf), Val_int(sup));
  return fcl_wrap(a);
}

value *sorting_sort(value *in)
{
  value a;
  CLOSURE("Sorting.sort");
  a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value *i2e(int in)
{
  value a;
  CLOSURE("i2e");
  a = caml_callback(*closure, Val_int(in));
  return fcl_wrap(a);
}

value *e2fd(value *in)
{
  value a;
  CLOSURE("e2fd");
  a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value *fd2e(value *in)
{
  value a;
  CLOSURE("fd2e");
  a = caml_callback(*closure, *in);
  return fcl_wrap(a);
}

value *arith_add(value *val1, value *val2)
{
  value a;
  CLOSURE("arith_add");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value *arith_sub(value *val1, value *val2)
{
  value a;
  CLOSURE("arith_sub");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value *arith_mul(value *val1, value *val2)
{
  value a;
  CLOSURE("arith_mul");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value *arith_div(value *val1, value *val2)
{
  value a;
  CLOSURE("arith_div");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value *arith_mod(value *val1, value *val2)
{
  value a;
  CLOSURE("arith_mod");
  a = caml_callback2(*closure, *val1, *val2);
  return fcl_wrap(a);
}

value *arith_abs(value *val1)
{
  value a;
  CLOSURE("arith_abs");
  a = caml_callback(*closure, *val1);
  return fcl_wrap(a);
}

value *cstr_lt(value *in1, value *in2)
{
  value a;
  CLOSURE("lt");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_le(value *in1, value *in2)
{
  value a;
  CLOSURE("le");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_eq(value *in1, value *in2)
{
  value a;
  CLOSURE("eq");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_ne(value *in1, value *in2)
{
  value a;
  CLOSURE("ne");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_gt(value *in1, value *in2)
{
  value a;
  CLOSURE("gt");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_ge(value *in1, value *in2)
{
  value a;
  CLOSURE("ge");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

char *cstr_name(value *in)
{
  CLOSURE("Cstr.name");
  // dangerous !! ^^
  return String_val(caml_callback(*closure, *in));
}

int cstr_post(value *in)
{
  value v;
  CLOSURE("Cstr.post");
  v = caml_callback_exn(*closure, *in);
  return Is_exception_result(v);
}

value *cstr_or(value *in1, value *in2)
{
  value a;
  CLOSURE("Cstr.or");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_and(value *in1, value *in2)
{
  value a;
  CLOSURE("Cstr.and");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *cstr_xor(value *in1, value *in2)
{
  value a;
  CLOSURE("Cstr.xor");
  a = caml_callback2_exn(*closure, *in1, *in2);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *cstr_not(value *in)
{
  value a;
  CLOSURE("Cstr.not");
  a = caml_callback_exn(*closure, *in);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *cstr_alldiff(value **val, unsigned long len, int b)
{
  value array, a;
  size_t i = 0;
  CLOSURE("Cstr.alldiff");
  // À la barbare
  array = caml_alloc(len, 0);
  for (; i < len; ++i)
    Store_field(array, i, val[i][0]);
  a = caml_callback2(*closure, Val_int(b), array);
  return fcl_wrap(a);
}

value *cstr_boolean(value *cstr)
{
  value a;
  CLOSURE("Cstr.boolean");
  a = caml_callback_exn(*closure, *cstr);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *gcc_cstr(value *array, value **cards, long *values, unsigned long len)
{
  value a;
  size_t i = 0;
  CLOSURE("Gcc.cstr");
  value distribution = caml_alloc(len, 0);
  for (; i < len; ++i)
  {
    value b = caml_alloc(2, 0);
    Store_field(b, 0, cards[i][0]);
    Store_field(b, 1, Val_long(values[i]));

    Store_field(distribution, i, b);
  }
  a = caml_callback2(*closure, *array, distribution);
  return fcl_wrap(a);
}

value *cstr_create(int update, int delay)
{
  value a;
  CLOSURE("Cstr.create");
  a = caml_callback2(*closure, Val_int(update), Val_int(delay));
  return fcl_wrap(a);
}

value *fdarray_create(value **val, unsigned long len)
{
  value array;
  size_t i = 0;
  // À la barbare
  array = caml_alloc(len, 0);
  for (; i < len; ++i)
    Store_field(array, i, val[i][0]);
  return fcl_wrap(array);
}

void fdarray_read(value *val1, value **val2)
{
  size_t i = 0;
  for (; i < Wosize_val(*val1); ++i)
    val2[i] = fcl_wrap(Field(*val1, i));
}

value *fdarray_min(value *in1)
{
  value a;
  CLOSURE("FdArray.min");
  a = caml_callback_exn(*closure, *in1);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *fdarray_max(value *in1)
{
  value a;
  CLOSURE("FdArray.max");
  a = caml_callback_exn(*closure, *in1);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *fdarray_sum(value *in1)
{
  value a;
  CLOSURE("FdArray.sum");
  a = caml_callback_exn(*closure, *in1);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *fdarray_get(value *in1, value *in2)
{
  value a;
  CLOSURE("FdArray.get");
  a = caml_callback2_exn(*closure, *in1, *in2);
  if Is_exception_result (a)
    return 0;
  return fcl_wrap(a);
}

value *strategy_queen()
{
  CLOSURE("Strategy.queen");
  return fcl_wrap(*closure);
}

value *strategy_mindomain()
{
  CLOSURE("Strategy.min_domain");
  return fcl_wrap(*closure);
}

value *strategy_minmin()
{
  CLOSURE("Strategy.min_min");
  return fcl_wrap(*closure);
}

value *strategy_lexicographic()
{
  CLOSURE("Strategy.lexicographic");
  return fcl_wrap(*closure);
}

value *strategy_callback(int i)
{
  value res;
  CLOSURE("Strategy.callback");
  res = caml_callback(*closure, Val_int(i));
  return fcl_wrap(res);
}

value *goals_success()
{
  CLOSURE("Goals.success");
  return fcl_wrap(*closure);
}

value *goals_fail()
{
  CLOSURE("Goals.fail");
  return fcl_wrap(*closure);
}

value *goals_or(value *in1, value *in2)
{
  value a;
  CLOSURE("Goals.or");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *goals_and(value *in1, value *in2)
{
  value a;
  CLOSURE("Goals.and");
  a = caml_callback2(*closure, *in1, *in2);
  return fcl_wrap(a);
}

value *goals_atomic(int i)
{
  value a;
  CLOSURE("Goals.atomic");
  a = caml_callback(*closure, Val_int(i));
  return fcl_wrap(a);
}

value *goals_create(int i)
{
  value a;
  CLOSURE("Goals.create");
  a = caml_callback(*closure, Val_int(i));
  return fcl_wrap(a);
}

value *goals_unify(value *v, int i)
{
  value a;
  CLOSURE("Goals.unify");
  a = caml_callback2(*closure, *v, Val_int(i));
  return fcl_wrap(a);
}

value *assignment_indomain()
{
  CLOSURE("Assignment.indomain");
  return fcl_wrap(*closure);
}

value *assignment_assign()
{
  CLOSURE("Assignment.assign");
  return fcl_wrap(*closure);
}

value *assignment_dichotomic()
{
  CLOSURE("Assignment.dichotomic");
  return fcl_wrap(*closure);
}

value *assignment_atomic(int i)
{
  value a;
  CLOSURE("Assignment.atomic");
  a = caml_callback(*closure, Val_int(i));
  return fcl_wrap(a);
}

value *assignment_and(value *v1, value *v2)
{
  value a;
  CLOSURE("Assignment.and");
  a = caml_callback2(*closure, *v1, *v2);
  return fcl_wrap(a);
}

value *assignment_or(value *v1, value *v2)
{
  value a;
  CLOSURE("Assignment.or");
  a = caml_callback2(*closure, *v1, *v2);
  return fcl_wrap(a);
}

value *goals_forall(value *s, value **val, long len, value *labelling)
{
  value array;
  value select;
  value res;
  size_t i = 0;
  select = (s == 0 ? Val_none : Val_some(*s));
  CLOSURE("Goals.forall");
  array = caml_alloc(len, 0);
  for (; i < (size_t)len; ++i)
    Store_field(array, i, val[i][0]);
  res = caml_callback3(*closure, select, *labelling, array);
  return fcl_wrap(res);
}

value *goals_selector_labelling(int i)
{
  value v;
  CLOSURE("Goals.selector.labelling");
  v = caml_callback(*closure, Val_int(i));
  return fcl_wrap(v);
}

value *goals_selector_select(int i)
{
  value v;
  CLOSURE("Goals.selector.select");
  v = caml_callback(*closure, Val_int(i));
  return fcl_wrap(v);
}

value *goals_selector_forall(long sel, long len, int labelling)
{
  value array;
  value res;
  value select = Val_none;
  size_t i = 0;
  CLOSURE("Goals.forall");
  array = caml_alloc(len, 0);
  for (; i < (size_t)len; ++i)
    Store_field(array, i, Val_int(i));
  /*   value *s = goals_selector_select(sel); */
  if (sel != -1)
    select = Val_some(*goals_selector_select(sel));
  value *l = goals_selector_labelling(labelling);
  res = caml_callback3(*closure, select, *l, array);
  /*   fcl_destroy(s); */
  fcl_destroy(l);
  return fcl_wrap(res);
}

value *mode_continue()
{
  CLOSURE("Goals.continue");
  return closure;
}

value *mode_restart()
{
  CLOSURE("Goals.restart");
  return closure;
}

value *mode_dicho()
{
  CLOSURE("Goals.dicho");
  return closure;
}

value *goals_minimize(value *mode, value *goal, value *expr, int i)
{
  value res;
  CLOSURE("Goals.minimize");
  value array[4] = {*mode, *goal, *expr, Val_int(i)};
  res = caml_callbackN(*closure, 4, array);
  return fcl_wrap(res);
}

int goals_solve(int i, value *goal)
{
  CLOSURE("Goals.solve");
  return Bool_val(caml_callback2(*closure, Val_int(i), *goal));
}

void set_backtrack_callback(int i, void (*fct)(int, int))
{
  callbacks[i] = (void *)fct;
}

void set_atomic_callback(int i, int (*fct)(int))
{
  callbacks[i] = (void *)fct;
}

void set_onsol_callback(int i, void (*fct)(int, int))
{
  callbacks[i] = (void *)fct;
}

void set_assign_callback(int i, void (*fct)(int, value *))
{
  callbacks[i] = (void *)fct;
}

void set_strategy_callback(int i, int (*fct)(int, value **, int))
{
  callbacks[i] = (void *)fct;
}

void set_update_callback(int i, int (*fct)(int, int))
{
  callbacks[i] = (void *)fct;
}

void set_delay_callback(int i, void (*fct)(int, value *))
{
  callbacks[i] = (void *)fct;
}

void set_selector_select_callback(int i, int (*fct)(int))
{
  callbacks[i] = (void *)fct;
}

void set_selector_labelling_callback(int i, long (*fct)(int, int))
{
  callbacks[i] = (void *)fct;
}

void set_goal_creator_callback(int i, long (*fct)(int))
{
  callbacks[i] = (void *)fct;
}

value ml_backtrack_callback(value v_i, value v_n)
{
  CAMLparam2(v_i, v_n);
  void (*c_backtrack_callback)(int, int) = (void (*)(int, int))callbacks[Int_val(v_i)];
  c_backtrack_callback(Int_val(v_i), Int_val(v_n));
  CAMLreturn(Val_unit);
}

value ml_atomic_callback(value v_i, value unit)
{
  CAMLparam2(v_i, unit);
  int (*c_atomic_callback)(int) = (int (*)(int))callbacks[Int_val(v_i)];
  int x = c_atomic_callback(Int_val(v_i));
  if (x == -1)
    stak_fail();
  CAMLreturn(Val_unit);
}

value ml_onsol_callback(value v_i, value v_n)
{
  CAMLparam2(v_i, v_n);
  void (*c_onsol_callback)(int, int) = (void (*)(int, int))callbacks[Int_val(v_i)];
  c_onsol_callback(Int_val(v_i), Int_val(v_n));
  CAMLreturn(Val_unit);
}

value ml_assign_atomic(value v_i, value v)
{
  CAMLparam2(v_i, v);
  void (*c_assign_callback)(int, value *) = (void (*)(int, value *))callbacks[Int_val(v_i)];
  c_assign_callback(Int_val(v_i), fcl_wrap(v)); // fcl_wrap actually useless but for compatibility reason!
  CAMLreturn(Val_unit);
}

value ml_strategy_cb(value v_i, value v1) // Var.Fd.t array
{
  CAMLparam2(v_i, v1);
  int len = Wosize_val(v1);
  int (*cb_strategy)(int, value **, int) =
      (int (*)(int, value **, int))callbacks[Int_val(v_i)];

  value **v2 = (value **)malloc(len * sizeof(value *));

  for (int i = 0; i < len; ++i)
    v2[i] = fcl_wrap(Field(v1, i));
  int res = cb_strategy(Int_val(v_i), v2, len);

  free(v2);
  CAMLreturn(Val_int(res));
}

value ml_update_cb(value v_i, value v_)
{
  CAMLparam2(v_i, v_);
  int (*update_cb)(int, int) = (int (*)(int, int))callbacks[Int_val(v_i)];
  int res = update_cb(Int_val(v_i), Int_val(v_));
  CAMLreturn(Val_int(res));
}

value ml_delay_cb(value v_i, value v_c)
{
  CAMLparam2(v_i, v_c);
  void (*delay_cb)(int, value *) = (void (*)(int, value *))callbacks[Int_val(v_i)];
  delay_cb(Int_val(v_i), fcl_wrap(v_c));
  CAMLreturn(Val_unit);
}

value ml_selector_select(value v_i, value v_)
{
  CAMLparam2(v_i, v_);
  int (*select)(int) = (int (*)(int))callbacks[Int_val(v_i)];
  int res = select(Int_val(v_i));
  CAMLreturn(Val_int(res));
}

value ml_selector_labelling(value v_i, value v_l)
{
  CAMLparam2(v_i, v_l);
  value *(*labelling)(int, int) =
      (long (*)(int, int))callbacks[Int_val(v_i)];
  value *res = (value *)labelling(Int_val(v_i), Int_val(v_l));
  CAMLreturn(*res);
}

value ml_goal_creator(value v_i, value v_unit)
{
  CAMLparam2(v_i, v_unit);
  value *(*creator)(int) = (long (*)(int))callbacks[Int_val(v_i)];
  value *res = (value *)creator(Int_val(v_i));
  if (res == 0)
    stak_fail();
  CAMLreturn(*res);
}

value *stak_bool_ref(int b)
{
  value v;
  CLOSURE("Stak.ref");
  v = caml_callback(*closure, Val_bool(b));
  return fcl_wrap(v);
}

int stak_bool_get(value *v)
{
  CLOSURE("Stak.get");
  return Bool_val(caml_callback(*closure, *v));
}

void stak_bool_set(value *v, int b)
{
  CLOSURE("Stak.set");
  caml_callback2(*closure, *v, Val_bool(b));
}

void stak_trail_i(int i)
{
  CLOSURE("Stak.trail");
  caml_callback(*closure, Val_int(i));
}

value *parse_array(value *list, long *res)
{
  size_t i = 0;
  value head;
  value *tmp;
  if (*list == Val_emptylist)
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
