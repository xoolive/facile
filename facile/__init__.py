import importlib_metadata

from .core import *  # noqa: F401, F403
from .core import facile_sum as sum  # noqa: F401

__version__ = importlib_metadata.version("facile")
