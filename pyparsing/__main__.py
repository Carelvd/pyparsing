from .__init__ import *

def test( teststring ):
    try:
        tokens = simpleSQL.parseString( teststring )
        tokenlist = tokens.asList()
        print (teststring + "->"   + str(tokenlist))
        print ("tokens = "         + str(tokens))
        print ("tokens.columns = " + str(tokens.columns))
        print ("tokens.tables = "  + str(tokens.tables))
        print (tokens.asXML("SQL",True))
    except ParseBaseException as err:
        print (teststring + "->")
        print (err.line)
        print (" "*(err.column-1) + "^")
        print (err)
    print()

selectToken    = CaselessLiteral( "select" )
fromToken      = CaselessLiteral( "from" )

ident          = Word( alphas, alphanums + "_$" )
columnName     = delimitedList( ident, ".", combine=True ).setParseAction( upcaseTokens )
columnNameList = Group( delimitedList( columnName ) )#.setName("columns")
tableName      = delimitedList( ident, ".", combine=True ).setParseAction( upcaseTokens )
tableNameList  = Group( delimitedList( tableName ) )#.setName("tables")
simpleSQL      = ( selectToken + \
                 ( '*' | columnNameList ).setResultsName( "columns" ) + \
                 fromToken + \
                 tableNameList.setResultsName( "tables" ) )

test( "SELECT * from XYZZY, ABC" )
test( "select * from SYS.XYZZY" )
test( "Select A from Sys.dual" )
test( "Select AA,BB,CC from Sys.dual" )
test( "Select A, B, C from Sys.dual" )
test( "Select A, B, C from Sys.dual" )
test( "Xelect A, B, C from Sys.dual" )
test( "Select A, B, C frox Sys.dual" )
test( "Select" )
test( "Select ^^^ frox Sys.dual" )
test( "Select A, B, C from Sys.dual, Table2   " )
