SystemStringQueue   QUEUE
Str                     &SystemStringClass
                    END

SystemStringClass   CLASS, MODULE('SystemString.CLW'),LINK('SystemString.CLW', _ABCLinkMode_), DLL(_ABCDllMode_), TYPE
s                       &STRING,PROTECTED
cs                      &CSTRING,PROTECTED
lines                   &SystemStringQueue,PROTECTED
EOLMarker               CSTRING(8),PROTECTED
TokensDelimiters        CSTRING (101),PROTECTED
TokenSelStart           LONG,PROTECTED
TokenSelEnd             LONG,PROTECTED

Construct               PROCEDURE()
Destruct                PROCEDURE()
!!! <summary>
!!! Dispose the internal String representation
!!! </summary>
DisposeIt               PROCEDURE()
!!! <summary>
!!! Dispose the internal CString representation
!!! </summary>
DisposeItCString        PROCEDURE()
!!! <summary>
!!! Alias for FromString
!!! </summary>
!!! <param name="svalue">The value to be assigned to the SystemString</param>
Str                     PROCEDURE(STRING svalue)
!!! <summary>
!!! Alias for ToString
!!! </summary>
!!! <returns>
!!! The value of the SystemString
!!! </returns>
Str                     PROCEDURE(),STRING
!!! <summary>
!!! Initializes the SystemStringClass to an empty string
!!! </summary>
Clean                   PROCEDURE()
!!! <summary>
!!! Free the memory allocated when the String was split into lines
!!! </summary>
CleanLines              PROCEDURE()
!!! <summary>
!!! Assign the passed STRING value to the SystemString
!!! </summary>
FromString              PROCEDURE(STRING svalue)
!!! <summary>
!!! Assign the passed CSTRING value to the SystemString
!!! </summary>
FromString              PROCEDURE(*CSTRING svalue)
!!! <summary>
!!! Return the value of the SystemString
!!! </summary>
!!! <returns>
!!! The value of the SystemString
!!! </returns>
ToString                PROCEDURE(),STRING
!!! <summary>
!!! Return the value of the SystemString as a CSTRING
!!! </summary>
!!! <returns>
!!! The value of the SystemString
!!! </returns>
ToCString               PROCEDURE(),*CSTRING
FromCString             PROCEDURE(),PROTECTED


!!! <summary>
!!! Return the value of the EOL marker used for the line splitting
!!! </summary>
!!! <returns>
!!! The value of the EOL marker 
!!! </returns>
GetEOLMarker            PROCEDURE(),STRING
!!! <summary>
!!! Set the value of the EOL marker used for the line splitting
!!! </summary>
SetEOLMarker            PROCEDURE(STRING EOLMarker)
!!! <summary>
!!! Append the string passed svalue to the value of the SystemString
!!! </summary>
Append                  PROCEDURE(STRING svalue)
!!! <summary>
!!! Append the passed string svalue to the value of the SystemString adding the EOLMarker at the end of the svalue
!!! </summary>
AppendLine              PROCEDURE(STRING svalue)
!!! <summary>
!!! Add a new line to the existing lines in the SystemString.
!!! The new line will be added after the last line position
!!! The new line will NOT affect the SystemString value till the FromLines function is called
!!! </summary>
AddLine                 PROCEDURE(STRING svalue)
!!! <summary>
!!! Insert a new line to the existing lines in the SystemString.
!!! The new line will be added at the startIndex position, moving any exising lines
!!! The new line will NOT affect the SystemString value till the FromLines function is called
!!! </summary>
InsertLine              PROCEDURE(ULONG startIndex, STRING svalue)
!!! <summary>
!!! Return the number of lines in the SystemString after the SplitToLines was called
!!! </summary>
!!! <returns>
!!! Return the number of lines in the SystemString
!!! </returns>
CountLines              PROCEDURE(),ULONG
!!! <summary>
!!! Gets the lineNumber'th line in the SystemString after SplitToLines was called
!!! </summary>
!!! <returns>
!!! Return the string value of the line
!!! </returns>
GetLineValue            PROCEDURE(ULONG lineNumber),STRING
!!! <summary>
!!! Gets the lineNumber'th line in the SystemString with leading and trailing spaces removed after SplitToLines was called
!!! </summary>
!!! <returns>
!!! Return the string value of the line
!!! </returns>
GetLineTrimValue        PROCEDURE(ULONG lineNumber),STRING
!!! <summary>
!!! Gets the lineNumber'th line in the SystemString as a SystemStringClass after SplitToLines was called
!!! </summary>
!!! <returns>
!!! Return the SystemStringClass value of the line
!!! </returns>
GetLine                 PROCEDURE(ULONG lineNumber),*SystemStringClass
!!! <summary>
!!! Get the SystemString queue of lines as a SystemStringQueue
!!! Any modification to this queue will affect the current SystemStringClass
!!! </summary>
!!! <returns>
!!! Return the SystemStringQueue of the current SystemStringClass
!!! </returns>
GetLines                PROCEDURE(),*SystemStringQueue
!!! <summary>
!!! Delete the lineNumber line in the SystemString
!!! </summary>
DeleteLine              PROCEDURE(ULONG lineNumber)
!!! <summary>
!!! Split the SystemString string value into lines using the EOLMarker as the separator
!!! Any previous lines will be discarded
!!! The length of each line is variable
!!! </summary>
SplitToLines            PROCEDURE()
!!! <summary>
!!! Split the SystemString string value into lines of the same length
!!! Any previous lines will be discarded
!!! The length of each line is lineLength
!!! If the length of the total SystemString value is less than the lineLength then the only line length is the SystemString length
!!! The last line length could be less than the lineLength
!!! </summary>
SplitToLines            PROCEDURE(LONG lineLength)
!!! <summary>
!!! Merge the SystemString lines into one string and assign it to the SystemString value
!!! </summary> 
FromLines               PROCEDURE()
!!! <summary>
!!! Merge the SystemString lines into one string and assign it to the SystemString value
!!! The value of the delimiter string will be appended to the content of each line before merging it to the SystemString value
!!! </summary>
FromLines               PROCEDURE(STRING delimiter)
!!! <summary>
!!! Merge the SystemString lines into one string and assign it to the SystemString value
!!! The value of the leftDelimiter and rigthDelimiter string will be added to the content of each line before merging it to the SystemString value
!!! </summary>
FromLines               PROCEDURE(STRING leftDelimiter, STRING rigthDelimiter)
!!! <summary>
!!! Merge the SystemString lines into one string and assign it to the SystemString value
!!! The value of the current EOLMarker will be appended to the content of each line before merging it to the SystemString value
!!! </summary>
FromLinesWithEOL        PROCEDURE()


!!! <summary>
!!! Get the value of the Tokens Delimiters that will be used in the Tokenizing process of the SystemString
!!! </summary>
!!! <returns>
!!! Return the Tokens Delimiters of the current SystemStringClass
!!! </returns>
GetTokensDelimiters     PROCEDURE(),STRING
!!! <summary>
!!! Set the value of the Tokens Delimiters that will be used in the Tokenizing process of the SystemString
!!! </summary>
SetTokensDelimiters     PROCEDURE(STRING delimiters)
!!! <summary>
!!! Reset the current token of the SystemString
!!! The NextToken will start from the begining of the string
!!! </summary>
ResetToken              PROCEDURE()
!!! <summary>
!!! Reset the current token of the SystemString and postion the tokens process to the end of the string
!!! The NextToken will be null but this is intended to be used with PrevToken to do a backward processing of the tokens
!!! </summary>
ResetTokenToEnd         PROCEDURE()
!!! <summary>
!!! Return True if the Token process was never started and NextToken/PrevToken was never called 
!!! </summary>
!!! <returns>
!!! Return True if the Token process was never started
!!! </returns>
IsTokenInited           PROCEDURE(),BYTE
!!! <summary>
!!! Return True SystemString is NULL of after the call to NextToken/PrevToken
!!! the Current Token contain an invalid (out of range) value
!!! </summary>
!!! <returns>
!!! Return True if the Current Token is out of range
!!! </returns>
IsTokenNull             PROCEDURE(),BYTE
!!! <summary>
!!! Return string token for the result after the call to NextToken/PrevToken
!!! </summary>
!!! <returns>
!!! Return the Current Token
!!! </returns>
GetCurrentToken         PROCEDURE(),STRING
!!! <summary>
!!! Move the pointer to the current token to the Next available Token in the SystemString value
!!! The processing is done from left to right stating from the last token found or if it is the first time that is called form the first charater of the string
!!! The Tokens will be separated using the values of the TokensDelimiters
!!! The SystemString value or it's lines will not be affected by the Tokenizing process
!!! If no more tokens exist, the status will be set so the IsTokenNull will return true
!!! </summary>
!!! <returns>
!!! If successful and the Next token exists it will return the value of the Current Found Token.
!!! Other wise it will return empty
!!! </returns>
NextToken               PROCEDURE(),STRING,PROC
!!! <summary>
!!! Move the pointer to the current token to the Previous available Token in the SystemString value
!!! The processing is done from right to left starting from the last token found or if it is the first time that was called from the last character of the string.
!!! To start form the last caracter of the string call first the ResetTokenToEnd method
!!! The Tokens will be separated using the values of the TokensDelimiters
!!! The SystemString value or it's lines will not be afected by the Tokenizing process
!!! If no more tokens exist, the status will be set so the IsTokenNull will return true
!!! </summary>
!!! <returns>
!!! If successful and a Previous token exists it will return the value of the Current Found Token.
!!! Other wise it will return empty
!!! </returns>
PrevToken               PROCEDURE(),STRING,PROC
!!! <summary>
!!! If a current token exist in the SystemString it will be removed from the SystemString value.
!!! The SystemString value will be affected by this call
!!! </summary>
!!! <returns>
!!! If succesful it will return True
!!! Other wise it will return false
!!! </returns>
PopToken                PROCEDURE(),BYTE,PROC
!!! <summary>
!!! The svalue will be inserted in the startIndex of the SystemString value.
!!! The SystemString value will be affected by this call
!!! </summary>
PushToken               PROCEDURE(ULONG startIndex, STRING svalue)
!!! <summary>
!!! The svalue will be search in the SystemString value and the found index will be returned if it is found
!!! And zero if it is not
!!! The search starts from the beginning of the string
!!! The search will use the the Token Delimiters to search for a token and not just a substring
!!! If you just need a substring use IndexOf
!!! </summary>
FoundToken              PROCEDURE(STRING svalue),LONG
!!! <summary>
!!! The svalue will be searched in the SystemString value and the index will be returned if it is found
!!! And zero if it is not
!!! The search starts from the startIndex
!!! The search will use the the Tokens Delimiters to search for a token and not just a substring
!!! If you just need a substring use IndexOf
!!! </summary>
FoundToken              PROCEDURE(STRING svalue, ULONG startIndex),LONG


StartsWith              PROCEDURE(STRING svalue),BYTE
StartsWith              PROCEDURE(STRING svalue,BYTE IgnoreCase),BYTE
EndsWith                PROCEDURE(STRING svalue),BYTE
EndsWith                PROCEDURE(STRING svalue,BYTE IgnoreCase),BYTE
IndexOf                 PROCEDURE(STRING svalue),LONG
IndexOf                 PROCEDURE(STRING svalue, ULONG startIndex),LONG
LastIndexOf             PROCEDURE(STRING svalue),LONG
LastIndexOf             PROCEDURE(STRING svalue, ULONG startIndex),LONG
Length                  PROCEDURE(),LONG
Split                   PROCEDURE(STRING token,BYTE includeEmptyResults = false)
CleanSplitQueue         PROCEDURE(*SystemStringQueue collection)
Insert                  PROCEDURE(ULONG startIndex, STRING value),STRING
Contains                PROCEDURE(STRING svalue),BYTE
PadLeft                 PROCEDURE(ULONG totalWidth),STRING
PadLeft                 PROCEDURE(ULONG totalWidth, STRING paddingChar),STRING
PadRight                PROCEDURE(ULONG totalWidth),STRING
PadRight                PROCEDURE(ULONG totalWidth, STRING paddingChar),STRING
PadCenter               PROCEDURE(ULONG totalWidth),STRING
PadCenter               PROCEDURE(ULONG totalWidth, STRING paddingChar),STRING

ReplaceInContent          PROCEDURE(STRING oldString,STRING newString),LONG,PROC!return the new string with the oldstring replaced
Replace                 PROCEDURE(STRING oldString,STRING newString),STRING!return the new string with the oldstring replaced
ReplaceSubString        PROCEDURE(STRING originalString, STRING oldString,STRING newString),STRING!execute a replace in the internal string, does not return anything


FormatString            PROCEDURE(STRING objectsString, <STRING objectsString1>, <STRING objectsString2>, <STRING objectsString3>, <STRING objectsString4>, <STRING objectsString5>),STRING
Remove                  PROCEDURE(ULONG startIndex),STRING
Remove                  PROCEDURE(ULONG startIndex, ULONG subLength),STRING
Substring               PROCEDURE(ULONG startIndex),STRING
Substring               PROCEDURE(ULONG startIndex, ULONG subLength),STRING
SubstringLeft           PROCEDURE(ULONG startIndex),STRING
SubstringLeft           PROCEDURE(ULONG startIndex, ULONG subLength),STRING
ToLower                 PROCEDURE(),STRING
ToUpper                 PROCEDURE(),STRING
ToCapitalize            PROCEDURE(),STRING
Trim                    PROCEDURE(),STRING !Remove spaces in front and leading
Trim                    PROCEDURE(STRING char1,<STRING char2>,<STRING char3>,<STRING char4>),STRING
Trim                    PROCEDURE(BYTE trimType,STRING char1,<STRING char2>,<STRING char3>,<STRING char4>),STRING,PROTECTED
TrimStart               PROCEDURE(STRING char1,<STRING char2>,<STRING char3>,<STRING char4>),STRING
TrimEnd                 PROCEDURE(STRING char1,<STRING char2>,<STRING char3>,<STRING char4>),STRING

ToBase64                PROCEDURE(),STRING
FromBase64              PROCEDURE(STRING svalue)
EncodeBase64            PROCEDURE()
DecodeBase64            PROCEDURE()
Compress                PROCEDURE()
Decompress              PROCEDURE()
ToFile                  PROCEDURE(STRING fileName),BYTE,PROC
FromFile                PROCEDURE(STRING fileName),BYTE,PROC
FromBlob                PROCEDURE(*BLOB b)
ToBlob                  PROCEDURE(*BLOB b)

!region Private Methods
Take24                  PROCEDURE(byte h, byte m, byte l, *STRING Into),PRIVATE
Take32                  PROCEDURE(*byte h, *byte m, *byte l, *STRING SFrom),BYTE,PRIVATE
Capitalize              PROCEDURE(STRING cs),STRING,PRIVATE
!endregion
                    END