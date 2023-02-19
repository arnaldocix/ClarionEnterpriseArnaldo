  MEMBER

  INCLUDE('absql.inc'),ONCE
  INCLUDE('abfile.inc'),ONCE
  INCLUDE('dynstr.inc'),ONCE


  MAP
ExecuteSqlStatement PROCEDURE(STRING sqlCode, FILE sqlTable, ErrorClass errorHandler),BYTE,PRIVATE
  END

SV_SqlScriptFile            FILE,DRIVER('ASCII')
record                RECORD
line                    STRING(1000)
                      END
                    END
Access:SV_SqlScriptFile     CLASS(FileManager)
Init                  PROCEDURE(ErrorClass errorHandler)
                    END

StatementList       QUEUE,TYPE
statement             &STRING
                    END

!region Exposed Functions
ExecuteSql          PROCEDURE(STRING sqlCode, <STRING eosMarker>, FILE sqlTable, ErrorClass errorHandler)
ds                    &IDynStr
returnCode            BYTE
i                     LONG(1)
eosLen                LONG,AUTO
strLen                LONG,AUTO
  CODE
  IF OMITTED(eosMarker)
    RETURN ExecuteSqlStatement(sqlCode, sqlTable, errorHandler)
  END
  eosLen = LEN(eosMarker) - 1
  strLen = LEN(CLIP(sqlCode))
  ds &= NewDynStr()
  LOOP i = 1 TO strlen + 1
    IF i = strlen + 1 OR (i < strlen - eosLen AND sqlCode[i : i + eosLen] = eosMarker)
      returnCode = ExecuteSqlStatement(ds.Str(), sqlTable, errorHandler)
      IF returnCode <> Level:Benign
        BREAK
      END
      ds.kill()
      i += eosLen
    ELSE
      ds.cat(sqlCode[i])
    END
  END
  DisposeDynStr(ds)
  RETURN returnCode
  
ExecuteSqlScript    PROCEDURE(STRING sqlFile, <STRING eosMarker>, FILE sqlTable, ErrorClass errorHandler)
ds                    &IDynStr
returnCode            BYTE
  CODE
  SV_SqlScriptFile{PROP:Name} = sqlFile
  Access:SV_SqlScriptFile.Init(errorHandler)
  returnCode = Access:SV_SqlScriptFile.Open()
  IF returnCode = Level:Benign
    ds &= NewDynStr()
    SET(SV_SqlScriptFile)
    LOOP
      IF Access:SV_SqlScriptFile.Next() <> Level:Benign THEN BREAK .
      ds.cat(CLIP(SV_SqlScriptFile.line))
    END
    IF OMITTED(eosMarker)
      returnCode = ExecuteSqlStatement(ds.Str(), sqlTable, errorHandler)
    ELSE
      returnCode = ExecuteSql(ds.Str(), eosMarker, sqlTable, errorHandler)
    END
    DisposeDynStr(ds)
    Access:SV_SqlScriptFile.Close()
  END
  RETURN returnCode

ExecuteSqlStatement PROCEDURE(STRING sqlCode, FILE sqlTable, ErrorClass errorHandler)
  CODE
  sqlTable{PROP:SQL} = sqlCode
  IF ERRORCODE()
    errorHandler.ThrowMessage(Msg:ExecutionOfSqlFailed, sqlCode)
    RETURN Level:Notify
  END
  RETURN Level:Benign
!endregion
!region SQLExecutor
SQLExecutor.Destruct    PROCEDURE()
  CODE
  IF NOT SELF.statements &= NULL
    LOOP
      GET(SELF.statements, 1)
      IF ERRORCODE()
        BREAK
      END
      DISPOSE(SELF.statements.statement)
      DELETE(SELF.statements)
    END
    DISPOSE(SELF.statements)
  END
  IF NOT SELF.delimiter &= NULL
    DISPOSE(SELF.delimiter)
  END
  
SQLExecutor.Init    PROCEDURE(ErrorClass errorHandler, FILE sqlTable, <STRING delimiter>)
  CODE
  SELF.errorHandler &= errorHandler
  SELF.f &= sqlTable
  IF NOT OMITTED(delimiter)
    SELF.delimiter &= NEW STRING(SIZE(delimiter))
    SELF.delimiter = delimiter
  END
  
SQLExecutor.Load    PROCEDURE(STRING fileName)
ds                    &IDynStr
returnCode            BYTE
strlen                LONG,AUTO
dLen                  LONG,AUTO
  CODE
  IF SELF.errorHandler &= NULL
    RETURN Level:Program
  END
  SELF.statements &= NEW StatementList
  SV_SqlScriptFile{PROP:Name} = fileName
  Access:SV_SqlScriptFile.Init(SELF.errorHandler)
  returnCode = Access:SV_SqlScriptFile.Open()
  IF returnCode = Level:Benign
    ds &= NewDynStr()
    IF SELF.delimiter &= NULL
      dLen = 0
    ELSE
      dLen = LEN(SELF.delimiter)
    END
    SET(SV_SqlScriptFile)
    LOOP
      IF Access:SV_SqlScriptFile.Next() <> Level:Benign THEN BREAK .
      strlen = LEN(CLIP(SV_SqlScriptFile.line))
      IF SELF.delimiter &= NULL OR SUB(SV_SqlScriptFile.line, strlen - dLen + 1, dLen) = SELF.delimiter
        ds.cat(SUB(SV_SqlScriptFile.line, 1, strlen - dLen))
        SELF.statements.statement &= NEW STRING(ds.StrLen())
        SELF.statements.statement = ds.Str()
        ds.Kill()
        ADD(SELF.statements)
      ELSE
        ds.cat(CLIP(SV_SqlScriptFile.line))
      END
    END
    IF ds.StrLen() > 0
      SELF.statements.statement &= NEW STRING(ds.StrLen())
      SELF.statements.statement = ds.Str()
      ADD(SELF.statements)
    END
    DisposeDynStr(ds)
    Access:SV_SqlScriptFile.Close()
  END
  RETURN returnCode
  
SQLExecutor.ExecuteStatement    PROCEDURE(LONG statementNumber)
  CODE
  IF SELF.statements &= NULL
    RETURN Level:Program
  END
  GET(SELF.statements, statementNumber)
  IF ERRORCODE()
    SELF.errorHandler.ThrowMessage(Msg:FetchOfSqlFailed, statementNumber)
  END
  RETURN ExecuteSqlStatement(SELF.statements.statement, SELF.f, SELF.errorHandler)

SQLExecutor.StatementCount  PROCEDURE()
  CODE
  IF SELF.statements &= NULL
    RETURN 0
  END
  RETURN RECORDS(SELF.statements)
!endregion

Access:SV_SqlScriptFile.Init    PROCEDURE(ErrorClass errorHandler)
  CODE
  SELF.Buffer &= SV_SqlScriptFile.record
  SELF.Init(SV_SqlScriptFile, errorHandler)
  SELF.Init()
  SELF.LazyOpen = FALSE
