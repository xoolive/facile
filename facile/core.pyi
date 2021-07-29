from typing import (
    Callable,
    Iterable,
    Iterator,
    List,
    NoReturn,
    Optional,
    Tuple,
    Union,
)

ArithmeticExpr = Union["Arith", "Cstr", int, "Variable"]

class Domain:
    def __len__(self) -> int: ...
    def size(self) -> int: ...
    def values(self) -> List[int]: ...
    def remove_low(self, x: int) -> "Domain": ...
    def remove_up(self, x: int) -> "Domain": ...
    @classmethod
    def create(cls, p: List[int]) -> "Domain": ...

class Event:
    @classmethod
    def on(cls, name: str) -> Domain: ...

class Variable:
    def set_name(self, n: str) -> None: ...
    @property
    def name(self) -> str: ...
    def mlname(self) -> str: ...
    def value(self) -> Optional[int]: ...
    def min(self) -> int: ...
    def max(self) -> int: ...
    def domain(self) -> Domain: ...
    def refine(self, d: Domain) -> None: ...
    def delay(self, events: Iterable[Event], c: "Cstr") -> None: ...
    def in_interval(self, inf: int, sup: int) -> "Variable": ...
    @classmethod
    def interval(
        cls, min_val: int, max_val: int, name: Optional[str] = None
    ) -> "Variable": ...
    @classmethod
    def create(
        cls, values: Union["Arith", "Cstr", List[int]]
    ) -> "Variable": ...
    @classmethod
    def binary(cls, *args, **kwargs) -> "Variable": ...
    def __lt__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __le__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __gt__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __ge__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __eq__(self, other) -> "Cstr": ...  # type: ignore
    def __ne__(self, other) -> "Cstr": ...  # type: ignore
    def __radd__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rsub__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rmul__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rfloordiv__(self, other: ArithmeticExpr) -> "Arith": ...
    def __add__(self, other: ArithmeticExpr) -> "Arith": ...
    def __sub__(self, other: ArithmeticExpr) -> "Arith": ...
    def __mul__(self, other: ArithmeticExpr) -> "Arith": ...
    def __floordiv__(self, other: ArithmeticExpr) -> "Arith": ...
    def __mod__(self: ArithmeticExpr, other: ArithmeticExpr) -> "Arith": ...
    def __pos__(self) -> "Variable": ...
    def __neg__(self) -> "Arith": ...
    def __abs__(self) -> "Arith": ...

class Arith:
    def __lt__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __le__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __gt__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __ge__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __eq__(self, other) -> "Cstr": ...  # type: ignore
    def __ne__(self, other) -> "Cstr": ...  # type: ignore
    def __add__(self, other: ArithmeticExpr) -> "Arith": ...
    def __sub__(self, other: ArithmeticExpr) -> "Arith": ...
    def __mul__(self, other: ArithmeticExpr) -> "Arith": ...
    def __floordiv__(self, other: ArithmeticExpr) -> "Arith": ...
    def __radd__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rsub__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rmul__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rfloordiv__(self, other: ArithmeticExpr) -> "Arith": ...
    def __mod__(self, other: ArithmeticExpr) -> "Arith": ...
    def __pos__(self) -> "Arith": ...
    def __neg__(self) -> "Arith": ...
    def __abs__(self) -> "Arith": ...
    def value(self) -> int: ...

class Cstr:
    def __lt__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __le__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __gt__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __ge__(self, other: ArithmeticExpr) -> "Cstr": ...
    def __eq__(self, other) -> "Cstr": ...  # type: ignore
    def __ne__(self, other) -> "Cstr": ...  # type: ignore
    def __and__(self, other: "Cstr") -> "Cstr": ...
    def __or__(self, other: "Cstr") -> "Cstr": ...
    def __invert__(self) -> "Cstr": ...
    def __xor__(self, other: "Cstr") -> "Cstr": ...
    def __add__(self, other: ArithmeticExpr) -> "Arith": ...
    def __sub__(self, other: ArithmeticExpr) -> "Arith": ...
    def __mul__(self, other: ArithmeticExpr) -> "Arith": ...
    def __radd__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rsub__(self, other: ArithmeticExpr) -> "Arith": ...
    def __rmul__(self, other: ArithmeticExpr) -> "Arith": ...
    def __pos__(self) -> "Variable": ...
    def __neg__(self) -> "Variable": ...
    def __abs__(self) -> "Variable": ...
    def post(self) -> None: ...
    def __bool__(self) -> NoReturn: ...

class Array:
    def __len__(self) -> int: ...
    def __iter__(self) -> Iterator[Variable]: ...
    def __repr__(self) -> str: ...
    def __getitem__(self, key: Union[Variable, Arith, int]) -> Variable: ...
    def value(self) -> List[int]: ...
    def max(self) -> Variable: ...
    def min(self) -> Variable: ...
    def sum(self) -> Variable: ...
    def sort(self) -> "Array": ...
    def alldifferent(self, *args, **kwargs) -> Cstr: ...
    def gcc(
        self, distribution: Iterable[Tuple[Union[int, Arith, Variable], int]]
    ) -> Cstr: ...

class Strategy:
    @classmethod
    def min_min(cls) -> "Strategy": ...
    @classmethod
    def min_domain(cls) -> "Strategy": ...
    @classmethod
    def queen(cls) -> "Strategy": ...
    @classmethod
    def custom(cls, custom: Callable[[List[int]], int]) -> "Strategy": ...

class Goal: ...

class Solution:
    solved: bool
    time: int
    backtrack: Optional[int]
    evaluation: int
    solution: List[int]

def solve(
    objective: Union[Goal, Iterable[Variable]],
    *args,
    time: bool = True,
    backtrack: bool = False,
    on_backtrack: Callable[[], None] = None,
    all_solutions: bool = False,
    minimize: Optional[Union[Variable, Arith]] = None,
    on_solution: Callable[[], None] = None,
    **kwargs,
) -> Solution: ...
def solve_all(*args, **kwargs) -> List[Solution]: ...
def minimize(
    goal: Union[Goal, Iterable[Variable]],
    expr: Union[Variable, Arith],
    *args,
    **kwargs,
) -> Solution: ...
def constraint(cstr: Cstr) -> None: ...
def alldifferent(variables: Iterable[Union[Variable, Arith]]) -> Cstr: ...
def variable(
    min_val: Union[int, range, Arith, Cstr, Iterable[int]],
    max_val: Optional[int] = None,
    *args,
    **kwargs,
) -> Variable: ...
def array(variables: Iterable[Union[int, Variable, Arith]]) -> Array: ...
