"""
Compat
======

This provides a compatability layer between Python 2 and Python 3.
"""
import sys

PY_3 = sys.version.startswith('3')
if PY_3:
    #: The maximum value an integer may have in Python
    _MAX_INT   = sys.maxsize
    #: The base implementation for string in Python
    basestring = str
    #: Unicode Characters
    unichr     = chr
    #: Unicode strings
    _ustr      = str
    #: List of builtins accepting a single argument, that can be used as parse actions.
    singleArgBuiltins = [sum, len, sorted, reversed, list, tuple, set, any, all, min, max]
    __all__ = ["_MAX_INT", "_ustr", "basestring","singleArgBuiltins"]

else:
    _MAX_INT = sys.maxint
    #: Mapping range to xrange
    range = xrange

    def _ustr(obj):
        """Drop-in replacement for str(obj) that tries to be Unicode friendly. It first tries
           str(obj). If that fails with a UnicodeEncodeError, then it tries unicode(obj). It
           then < returns the unicode object | encodes it with the default encoding | ... >.
        """
        if isinstance(obj,unicode):
            return obj

        try:
            # If this works, then _ustr(obj) has the same behaviour as str(obj), so
            # it won't break any existing code.
            return str(obj)

        except UnicodeEncodeError:
            # The Python docs (http://docs.python.org/ref/customization.html#l2h-182)
            # state that "The return value must be a string object". However, does a
            # unicode object (being a subclass of basestring) count as a "string
            # object"?
            # If so, then return a unicode object:
            return unicode(obj)
            # Else encode it... but how? There are many choices... :)
            # Replace unprintables with escape codes?
            #return unicode(obj).encode(sys.getdefaultencoding(), 'backslashreplace_errors')
            # Replace unprintables with question marks?
            #return unicode(obj).encode(sys.getdefaultencoding(), 'replace')
            # ...

    # build list of single arg builtins, tolerant of Python version, that can be used as parse actions
    singleArgBuiltins = []
    import __builtin__
    for fname in "sum len sorted reversed list tuple set any all min max".split():
        try:
            singleArgBuiltins.append(getattr(__builtin__,fname))
        except AttributeError:
            continue
    __all__ = ["_MAX_INT", "_ustr", "basestring","singleArgBuiltins", "range"]
