# module pyparsing.py
#
# Copyright (c) 2003-2013  Paul T. McGuire
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
__doc__ = \
"""
pyparsing module - Classes and methods to define and execute parsing grammars

The pyparsing module is an alternative approach to creating and executing simple grammars,
vs. the traditional lex/yacc approach, or the use of regular expressions.  With pyparsing, you
don't need to learn a new syntax for defining grammars or matching expressions - the parsing module
provides a library of classes that you use to construct the grammar directly in Python.

Here is a program to parse "Hello, World!" (or any greeting of the form C{"<salutation>, <addressee>!"})::

    from pyparsing import Word, alphas

    # define grammar of a greeting
    greet = Word( alphas ) + "," + Word( alphas ) + "!"

    hello = "Hello, World!"
    print (hello, "->", greet.parseString( hello ))

The program outputs the following::

    Hello, World! -> ['Hello', ',', 'World', '!']

The Python representation of the grammar is quite readable, owing to the self-explanatory
class names, and the use of '+', '|' and '^' operators.

The parsed results returned from C{parseString()} can be accessed as a nested list, a dictionary, or an
object with named attributes.

The pyparsing module handles some of the problems that are typically vexing when writing text parsers:
 - extra or missing whitespace (the above program will also handle "Hello,World!", "Hello  ,  World  !", etc.)
 - quoted strings
 - embedded comments
"""

__version__ = "2.0.1"
__versionTime__ = "16 July 2013 22:22"
__author__ = "Paul McGuire <ptmcg@users.sourceforge.net>"

import collections
from pyparsing.abc         import ParserElement
from pyparsing.ast         import ParseResults
from pyparsing.charsets    import *
from pyparsing.expressions import *
from pyparsing.exceptions  import *
from pyparsing.tokens      import *
from pyparsing.utils       import col,lineno,line,nullDebugAction,_defaultStartDebugAction,_defaultSuccessDebugAction,_defaultExceptionDebugAction,_trim_arity,_xml_escape
from pyparsing.helpers     import *
from pyparsing.debug       import OnlyOnce, traceParseAction
from pyparsing.exceptions  import ParseBaseException,ParseException,ParseFatalException,ParseSyntaxException,RecursiveGrammarException
#~ sys.stderr.write( "testing pyparsing module, version %s, %s\n" % (__version__,__versionTime__ ) )

#
# Complete Classes
#
#: Here we tie the ParseElement back to the Literal class
ParserElement.literalStringClass = Literal
#: Here we tie the ParseElement back to the MatchFirst class
ParserElement.orExpressionClass  = MatchFirst
#: Here we tie the ParseElement back to the MatchFirst class
ParserElement.andExpressionClass = And
#: Here we tie the ParseElement back to the MatchFirst class
ParserElement.notAnyExpressionClass = NotAny
#: Here we tie the ParseElement back to the ZeroOrMore class
ParserElement.zeroOrMoreExpressionClass = ZeroOrMore
#: Here we tie the ParseElement back to the OneOrMore class
ParserElement.oneOrMoreExpressionClass = OneOrMore
#
# Registering Classes
#
#: Register ParseResults as a mutable mapping within the collections module
collections.MutableMapping.register(ParseResults)

__all__ = [
# Expressions
'And', 'CaselessKeyword', 'CaselessLiteral', 'CharsNotIn', 'Combine', 'Dict', 'Each', 'Empty',
'FollowedBy', 'Forward', 'GoToColumn', 'Group', 'Keyword', 'LineEnd', 'LineStart', 'Literal',
'MatchFirst', 'NoMatch', 'NotAny', 'OneOrMore', 'Optional', 'Or',
'ParseResults', 'ParseSyntaxException', 'ParserElement', 'QuotedString', 'RecursiveGrammarException',
'Regex', 'SkipTo', 'StringEnd', 'StringStart', 'Suppress', 'Token', 'TokenConverter', 'Upcase',
# Tokens
'White', 'Word', 'WordEnd', 'WordStart', 'ZeroOrMore',
'alphanums', 'alphas', 'alphas8bit', 'anyCloseTag', 'anyOpenTag', 'cStyleComment', 'col',
'commaSeparatedList', 'commonHTMLEntity', 'countedArray', 'cppStyleComment', 'dblQuotedString',
'dblSlashComment', 'delimitedList', 'dictOf', 'downcaseTokens', 'empty', 'hexnums',
'htmlComment', 'javaStyleComment', 'keepOriginalText', 'line', 'lineEnd', 'lineStart', 'lineno',
'makeHTMLTags', 'makeXMLTags', 'matchOnlyAtCol', 'matchPreviousExpr', 'matchPreviousLiteral',
'nestedExpr', 'nullDebugAction', 'nums', 'oneOf', 'opAssoc', 'operatorPrecedence', 'printables',
'punc8bit', 'pythonStyleComment', 'quotedString', 'removeQuotes', 'replaceHTMLEntity', 
'replaceWith', 'restOfLine', 'sglQuotedString', 'srange', 'stringEnd',
'stringStart', 'unicodeString', 'upcaseTokens', 'withAttribute',
# Helpers
'indentedBlock', 'originalTextFor', 'ungroup', 'infixNotation', 
# Exceptions
'ParseBaseException', 'ParseElementEnhance', 'ParseException', 'ParseExpression', 'ParseFatalException',
# Debug
'traceParseAction', 'OnlyOnce',
]


_L = Literal


# convenience constants for positional expressions
empty       = Empty().setName("empty")
lineStart   = LineStart().setName("lineStart")
lineEnd     = LineEnd().setName("lineEnd")
stringStart = StringStart().setName("stringStart")
stringEnd   = StringEnd().setName("stringEnd")

_escapedPunc    = Word( _bslash, r"\[]-*.$+^?()~ ", exact=2 ).setParseAction(lambda s,l,t:t[0][1])
#: Escaped parser for Hex characters
_escapedHexChar = Regex(r"\\0?[xX][0-9a-fA-F]+").setParseAction(lambda s,l,t:unichr(int(t[0].lstrip(r'\0x'),16)))
#: Escaped parser for Oct characters
_escapedOctChar = Regex(r"\\0[0-7]+").setParseAction(lambda s,l,t:unichr(int(t[0][1:],8)))
_singleChar     = _escapedPunc | _escapedHexChar | _escapedOctChar | Word(printables, excludeChars=r'\]', exact=1)
_charRange      = Group(_singleChar + Suppress("-") + _singleChar)
_reBracketExpr  = Literal("[") + Optional("^").setResultsName("negate") + Group( OneOrMore( _charRange | _singleChar ) ).setResultsName("body") + "]"

_expanded = lambda p: (isinstance(p,ParseResults) and ''.join(unichr(c) for c in range(ord(p[0]),ord(p[1])+1)) or p)




unicodeString = Combine(_L('u') + quotedString.copy())

alphas8bit = srange(r"[\0xc0-\0xd6\0xd8-\0xf6\0xf8-\0xff]")
punc8bit = srange(r"[\0xa1-\0xbf\0xd7\0xf7]")

anyOpenTag,anyCloseTag = makeHTMLTags(Word(alphas,alphanums+"_:"))
commonHTMLEntity = Combine(_L("&") + oneOf("gt lt amp nbsp quot").setResultsName("entity") +";").streamline()
_htmlEntityMap = dict(zip("gt lt amp nbsp quot".split(),'><& "'))
replaceHTMLEntity = lambda t : t.entity in _htmlEntityMap and _htmlEntityMap[t.entity] or None

# it's easy to get these comment structures wrong - they're very common, so may as well make them available
cStyleComment = Regex(r"/\*(?:[^*]*\*+)+?/").setName("C style comment")

htmlComment = Regex(r"<!--[\s\S]*?-->")
restOfLine = Regex(r".*").leaveWhitespace()
dblSlashComment = Regex(r"\/\/(\\\n|.)*").setName("// comment")
cppStyleComment = Regex(r"/(?:\*(?:[^*]*\*+)+?/|/[^\n]*(?:\n[^\n]*)*?(?:(?<!\\)|\Z))").setName("C++ style comment")

javaStyleComment = cppStyleComment
pythonStyleComment = Regex(r"#.*").setName("Python style comment")
_commasepitem = Combine(OneOrMore(Word(printables, excludeChars=',') +
                                  Optional( Word(" \t") +
                                            ~Literal(",") + ~LineEnd() ) ) ).streamline().setName("commaItem")
commaSeparatedList = delimitedList( Optional( quotedString.copy() | _commasepitem, default="") ).setName("commaSeparatedList")


