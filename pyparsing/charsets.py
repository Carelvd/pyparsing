import string

_bslash    = chr(92)
alphas     = string.ascii_lowercase + string.ascii_uppercase
nums       = "0123456789"
hexnums    = nums + "ABCDEFabcdef"
alphanums  = alphas + nums
printables = "".join(c for c in string.printable if c not in string.whitespace)

__all__ = ["_bslash","alphas","alphanums","hexnums","nums","printables"]
