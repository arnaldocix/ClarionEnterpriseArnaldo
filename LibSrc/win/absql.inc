!ABCIncludeFile


  OMIT('_EndOfInclude_',_ABCSQLPresent_)  ! Omit this if already compiled
_ABCSQLPresent_         EQUATE(1)

  INCLUDE('ABERROR.INC'),ONCE

  MAP
    MODULE('ABSQL.CLW')
!!! <summary>
!!! Executes sqlCode as a series of SQL commands using sqlTable.
!!! Each command is seperated by eosMarker.  Any errors are
!!! reported using the passed errorHandler
!!! </summary>
!!! <param name="sqlCode">A string containing one or more SQL commands</param>
!!! <param name="eosMarker">
!!! The string or character that marks the end of an SQL statement.
!!! Passing an empty string indicates that there is one SQL statement per line
!!! Example of end of statement markers are 'GO' for MSSQL, or ; for SQLite
!!! </param>
!!! <param name="sqlTable">
!!! The file that is used to execute the SQL statements
!!! The file does not need to be open, however the OWNER attribute must have been
!!! set up so the file can connect to a database
!!! </param>
!!! <param name="errorHandler">
!!! errorHandler.Throw(Msg:ExecutionOfSqlFailed) is called if an error occurs when attempting
!!! to execute the SQL code
!!! </param>
!!! <returns> Level:Benign on success.  If an error occurs, Level:Notify is returned</returns>
ExecuteSql       PROCEDURE(STRING sqlCode, <STRING eosMarker>, FILE sqlTable, ErrorClass errorHandler),BYTE,PROC,DLL(_ABCDllMode_)
!!! <summary>
!!! Executes SQL code that is stored in the file sqlFile using the file sqlTable.
!!! Each command is seperated by eosMarker.  Any errors are
!!! reported using the passed errorHandler
!!! </summary>
!!! <param name="sqlCode">A string containing the name of a file that containa one or more SQL commands</param>
!!! <param name="eosMarker">
!!! The string or character that marks the end of an SQL statement.
!!! Passing an empty string indicates that there is one SQL statement per line
!!! Example of end of statement markers are 'GO' for MSSQL, or ; for SQLite
!!! </param>
!!! <param name="sqlTable">
!!! The file that is used to execute the SQL statements
!!! The file does not need to be open, however the OWNER attribute must have been
!!! set up so the file can connect to a database
!!! </param>
!!! <param name="errorHandler">
!!! errorHandler.Throw(Msg:ExecutionOfSqlFailed) is called if an error occurs when attempting
!!! to execute the SQL code.  Other errors will be thrown if there is a problem reading the file
!!! </param>
!!! <returns> Level:Benign on success.</returns>
ExecuteSqlScript PROCEDURE(STRING sqlFile, <STRING eosMarker>, FILE sqlTable, ErrorClass errorHandler),BYTE,PROC,DLL(_ABCDllMode_)
    END
  END

SQLExecutor         CLASS,TYPE,MODULE('absql.clw'),LINK('absql.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
errorHandler          &ErrorClass,PRIVATE
delimiter             &STRING,PRIVATE
f                     &FILE,PRIVATE
statements            &StatementList,PRIVATE

Destruct              PROCEDURE()

!!! <summary>
!!! Initialises the SQLExecutor
!!! </summary>
!!! <param name="errorHandler">
!!! Error Handler used to handle errors
!!! </param>
!!! <param name="sqlTable">
!!! The file that is used to execute the SQL statements
!!! The file does not need to be open, however the OWNER attribute must have been
!!! set up so the file can connect to a database
!!! </param>
!!! <param name="delimiter">
!!! The string or character that marks the end of an SQL statement.
!!! Passing an empty string indicates that there is one SQL statement per line
!!! Example of end of statement markers are 'GO' for MSSQL, or ; for SQLite
!!! </param>
Init                  PROCEDURE(ErrorClass errorHandler, FILE sqlTable, <STRING delimiter>)

!!! <summary>
!!! Reads the file parsing the contents so they can be executed by calls to SQLExecutor.ExecuteStatement
!!! The file is parsed into seperate SQL Statements using the delimiter passed to SQLExecutor.Init
!!! </summary>
!!! <param name="sqlTable">
!!! The file that is used to execute the SQL statements
!!! The file does not need to be open, however the OWNER attribute must have been
!!! set up so the file can connect to a database
!!! </param>
!!! <returns>
!!! Level:Benign          on success.  
!!! Level:Program         if SQLExecutor.Init has not been called.
!!! Level:Notify          if an error occured when reading the file
!!! </returns>
Load                 PROCEDURE(STRING fileName),BOOL,PROC

!!! <summary>
!!! Executes the statementNumber'th SQL command from the file passed to SQLExecutor.Open
!!! </summary>
!!! <param name="statementNumber">Which statement to execute.  Starting at 1</param>
!!! <returns>
!!! Level:Benign          on success.  
!!! Level:Program         if SQLExecutor.Init or SQLExecutor.Load have not been called.
!!! Level:Notify          if an error occured when reading the file
!!! </returns>
ExecuteStatement      PROCEDURE(LONG statementNumber),BOOL,PROC

!!! <summary>
!!! Returns the number of statements that are in the file passed to SQLExecutor.Load
!!! </summary>
StatementCount        PROCEDURE(),LONG
                    END

_EndOfInclude_
