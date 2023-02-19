! BUILTINS.CLW
! CW Runtime Library map definitions
! Automatically included by compiler
! Needs equates included first with unsigned & signed definitions

  MODULE('C%V%RUN%X%')
! New overloaded functions

      !!! <summary>
      !!! Builds keys and indexes.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      BUILD(FILE fileLabel),NAME('Cla$FILE_BUILDf')

      !!! <summary>
      !!! Builds keys and indexes.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY declaration.</param>
      BUILD(KEY keyLabel),NAME('Cla$FILE_BUILDk')

      !!! <summary>
      !!! Builds keys and indexes.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY declaration.</param>
      !!! <param name="components">A string constant or variable containing the list of the component fields on which to BUILD the dynamic INDEX. The fields must be separated by commas, with leading plus (+) or minus (-) to indicate ascending or descending sequence (if supported by the file driver).</param>
      !!! <param name="filter">A string constant, variable, or expression containing a logical expression with which to filter out unneeded records from the dynamic index. This requires that you name components for the index. You must BIND all variables used in the filter expression.</param>
      BUILD(KEY keyLabel, <STRING components>,STRING filter),NAME('Cla$FILE_BUILDksf')

      !!! <summary>
      !!! Builds keys and indexes.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY declaration.</param>
      !!! <param name="components">A string constant or variable containing the list of the component fields on which to BUILD the dynamic INDEX. The fields must be separated by commas, with leading plus (+) or minus (-) to indicate ascending or descending sequence (if supported by the file driver).</param>
      BUILD(KEY keyLabel, STRING components),NAME('Cla$FILE_BUILDks')

      !!! <summary>
      !!! Closes a data structure.
      !!! </summary>
      !!! <param name="entity">The label of a FILE, VIEW, APPLICATION, WINDOW, or REPORT structure.</param>
      CLOSE(FILE entity),NAME('Cla$FILE_CLOSE')

      !!! <summary>
      !!! Closes a data structure.
      !!! </summary>
      !!! <param name="entity">The label of a FILE, VIEW, APPLICATION, WINDOW, or REPORT structure.</param>
      CLOSE(WINDOW entity),NAME('Cla$CLOSEwindow')

      !!! <summary>
      !!! Removes a record from a FILE, VIEW, or QUEUE structure.
      !!! </summary>
      !!! <param name="entity">The label of a FILE, VIEW, or QUEUE structure.</param>
      DELETE(FILE entity),NAME('Cla$FILE_DELETE')

      !!! <summary>
      !!! Removes a record from a FILE, VIEW, or QUEUE structure.
      !!! </summary>
      !!! <param name="entity">The label of a FILE, VIEW, or QUEUE structure.</param>
      DELETE(QUEUE entity),NAME('Cla$DELETEqueue')

      !!! <summary>
      !!! Checks duplicate entries in unique keys.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <returns>A non-zero value (true) if writing the current record to the data file would post the "Creates Duplicate Key" error. All KEYs declared without a DUP attribute are checked.</returns>
      DUPLICATE(FILE fileLabel),LONG,NAME('Cla$FILE_DUPLICATEf')

      !!! <summary>
      !!! Checks duplicate entries in unique keys.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY declaration.</param>
      !!! <returns>A non-zero value (true) if writing the current record to the data file would post the "Creates Duplicate Key" error. Only the specified KEY is checked.</returns>
      DUPLICATE(KEY keyLabel),LONG,NAME('Cla$FILE_DUPLICATEk')

      !!! <summary>
      !!! Fixes (initializes) a dynamic file.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE structure, which must be a reference variable.</param>
      FIXFORMAT(FILE fileLabel),NAME('Cla$FILE_FIXFORMAT')

      !!! <summary>
      !!! Releases all resources allocated by GETSTATE
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <param name="savedState">A LONG numeric variable which represents the value returned by the GETSTATE procedure.</param>
      FREESTATE(FILE fileLabel, POINTER_T savedState),NAME('Cla$FILE_FREESTATE')

      !!! <summary>
      !!! Gets the NULL state of a table.
      !!! </summary>
      !!! <param name="table">The label of a FILE declaration.</param>
      !!! <returns>A string containing the NULL state of the table. The size of the return string is file driver dependent</returns>
      GETNULLS(FILE table),STRING,NAME('Cla$FILE_GETNULLS')

      !!! <summary>
      !!! Identifies a file's current state including the record buffer, file pointer, file sequencing order, and the error state of the system.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <param name="saveBlob">An integer constant or variable containing either zero (0) or one (1). If one, the state of the BLOB is also returned. If omitted or zero (0) the BLOB state is not returned.</param>
      !!! <returns>The current state of the data file. This includes the record buffer, MEMOs, BLOBs, file pointer, file sequencing order, and the current error state of the system.</returns>
      GETSTATE(FILE fileLabel, BYTE saveBlob=FALSE),POINTER_T,NAME('Cla$FILE_GETSTATE')

      !!! <summary>
      !!! Arms record locking.
      !!! </summary>
      !!! <param name="entity">The label of a FILE opened for shared access or a VIEW whose component files are opened for shared access.</param>
      HOLD(FILE entity),NAME('Cla$FILE_HOLDf')

      !!! <summary>
      !!! Arms record locking.
      !!! </summary>
      !!! <param name="entity">The label of a FILE opened for shared access or a VIEW whose component files are opened for shared access.</param>
      !!! <param name="seconds">A numeric constant or variable which specifies the maximum wait time in seconds.</param>
      HOLD(FILE entity, SIGNED seconds),NAME('Cla$FILE_HOLDfi')

      !!! <summary>
      !!! Locks a data file.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE opened for shared access.</param>
      LOCK(FILE fileLabel),NAME('Cla$FILE_LOCKf')

      !!! <summary>
      !!! Locks a data file.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE opened for shared access.</param>
      !!! <param name="seconds">A numeric constant or variable which specifies the maximum wait time in seconds.</param>
      LOCK(FILE fileLabel, SIGNED seconds),NAME('Cla$FILE_LOCKfi')

      !!! <summary>
      !!! Opens a file.
      !!! </summary>
      !!! <param name="entity">The label of a FILE.</param>
      !!! <param name="accessMode">A numeric constant, variable, or expression which determines the level of access granted to both the user opening the FILE entity, and other users in a multi-user system. If omitted, the default value is 22h (Read/Write + Deny Write). Valid only when the entity parameter names a FILE structure.</param>
      OPEN(FILE entity, UNSIGNED accessMode=022H),NAME('Cla$FILE_OPEN')

      !!! <summary>
      !!! Opens a window.
      !!! </summary>
      !!! <param name="entity">The label of a WINDOW.</param>
      OPEN(WINDOW entity),NAME('Cla$OPENwindow')

      !!! <summary>
      !!! Opens a window.
      !!! </summary>
      !!! <param name="entity">The label of a WINDOW.</param>
      !!! <param name="owner">The label of the APPLICATION or WINDOW structure which "owns" the window entity being opened. Normally, this parameter would be an &WINDOW reference variable. Valid only when the entity parameter names an APPLICATION or WINDOW structure.</param>
      OPEN(WINDOW entity, WINDOW owner),NAME('Cla$OPENowned')

      !!! <summary>
      !!! Returns relative record position.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration. This specifies physical record order within the file.</param>
      !!! <returns>Returns the relative record position within the data file (in file sequence).</returns>
      POINTER(FILE fileLabel),POINTER_T,NAME('Cla$FILE_POINTERf')

      !!! <summary>
      !!! Returns relative record position within the KEY.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY or INDEX declaration. This specifies the entry order within the KEY or INDEX file.</param>
      !!! <returns>Returns the relative record position within the KEY or INDEX file.</returns>
      POINTER(KEY keyLabel),POINTER_T,NAME('Cla$FILE_POINTERk')

      !!! <summary>
      !!! Returns the entry number of the last entry accessed in a QUEUE.
      !!! </summary>
      !!! <param name="queueLabel">The label of a QUEUE structure, or the label of a passed QUEUE parameter.</param>
      !!! <returns>Returns a LONG integer specifying the entry number of the last QUEUE entry accessed by ADD, GET, or PUT.</returns>
      POINTER(QUEUE queueLabel),POINTER_T,NAME('Cla$POINTERqueue')

      !!! <summary>
      !!! Identifies a record's unique position in a FILE. 
      !!! </summary>
      !!! <param name="sequence">The label of a FILE declaration.</param>
      !!! <returns>A STRING which identifies a record's unique position within the FILE</returns>
      POSITION(FILE sequence),STRING,NAME('Cla$FILE_POSITIONf')

      !!! <summary>
      !!! Identifies a record's unique position in a KEY. 
      !!! </summary>
      !!! <param name="sequence">The label of a KEY declaration.</param>
      !!! <returns>A STRING which identifies a record's unique position within the KEY</returns>
      POSITION(KEY sequence),STRING,NAME('Cla$FILE_POSITIONk')

      !!! <summary>
      !!! Identifies a record's unique position in a QUEUE. 
      !!! </summary>
      !!! <param name="sequence">The label of a QUEUE declaration.</param>
      !!! <returns>returns a LONG integer pointer to the first queue record with a matching key value 
      !!! (for the current active sort order). </returns>
      POSITION(QUEUE sequence),POINTER_T,NAME('Cla$POSITIONqueue')

      !!! <summary>
      !!! Returns the number of records in a FILE. 
      !!! </summary>
      !!! <param name="entity">The label of a FILE declaration.</param>
      !!! <returns>A LONG integer containing the number of entries in the FILE</returns>
      RECORDS(FILE entity),COUNT_T,NAME('Cla$FILE_RECORDSf')

      !!! <summary>
      !!! Returns the number of records in a KEY. 
      !!! </summary>
      !!! <param name="entity">The label of a KEY declaration.</param>
      !!! <returns>A LONG integer containing the number of entries in the KEY</returns>
      RECORDS(KEY entity),COUNT_T,NAME('Cla$FILE_RECORDSk')

      !!! <summary>
      !!! Returns the number of records in a QUEUE. 
      !!! </summary>
      !!! <param name="entity">The label of a QUEUE declaration.</param>
      !!! <returns>A LONG integer containing the number of entries in the QUEUE</returns>
      RECORDS(QUEUE entity),COUNT_T,NAME('Cla$RECORDSqueue')

      !!! <summary>
      !!! Regets a specific record.
      !!! </summary>
      !!! <param name="sequence">The label of a FILE declaration.</param>
      !!! <param name="str">The string returned by the POSITION procedure.</param>
      REGET(FILE sequence, STRING str),NAME('Cla$FILE_REGETf')

      !!! <summary>
      !!! Regets a specific record.
      !!! </summary>
      !!! <param name="sequence">The label of a KEY declaration.</param>
      !!! <param name="str">The string returned by the POSITION procedure.</param>
      REGET(KEY sequence, STRING str),NAME('Cla$FILE_REGETk')

      !!! <summary>
      !!! Resets sequential processing to a specific record.
      !!! </summary>
      !!! <param name="sequence">The label of a FILE declaration.</param>
      !!! <param name="str">The string returned by the POSITION procedure.</param>
      RESET(FILE sequence, STRING str),NAME('Cla$FILE_RESETf')

      !!! <summary>
      !!! Resets sequential processing to a specific record.
      !!! </summary>
      !!! <param name="sequence">The label of a KEY declaration.</param>
      !!! <param name="str">The string returned by the POSITION procedure.</param>
      RESET(KEY sequence, STRING str),NAME('Cla$FILE_RESETk')

      !!! <summary>
      !!! Resets sequential processing to a specific record.
      !!! </summary>
      !!! <param name="viewLabel">The label of a VIEW.</param>
      !!! <param name="fileLabel">The label of a component file of the VIEW.</param>
      RESET(VIEW viewLabel, FILE fileLabel),NAME('Cla$VIEW_RESETvf')

      !!! <summary>
      !!! Restores a file's savedstate state including the record buffer, file pointer, file sequencing order, and the error state of the system.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <param name="savedState">A LONG numeric variable which represents the value returned by the GETSTATE procedure.</param>
      !!! <param name="restoreFlag">A BYTE value that sets whether or not the file’s record buffer is to be restored. The default value is FALSE (0), which indicates that the file buffer is not restored.</param>
      RESTORESTATE(FILE fileLabel, POINTER_T savedState, BYTE restoreFlag=0),NAME('Cla$FILE_RESTORESTATE')

      !!! <summary>
      !!! Initializes sequential processing of a FILE.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration. This parameter specifies processing in the physical order in which records occur in the data file.</param>
      SET(FILE fileLabel),NAME('Cla$FILE_SETf')

      !!! <summary>
      !!! Initializes sequential processing of a FILE.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration. This parameter specifies processing in the physical order in which records occur in the data file.</param>
      !!! <param name="keyLabel">The label of a KEY or INDEX declaration.</param>
      SET(FILE fileLabel, KEY keyLabel),NAME('Cla$FILE_SETfk')

      !!! <summary>
      !!! Initializes sequential processing of a FILE.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration. This parameter specifies processing in the physical order in which records occur in the data file.</param>
      !!! <param name="filePointer">A numeric constant, variable, or expression for the value returned by the POINTER(file) procedure.</param>
      SET(FILE fileLabel, POINTER_T filePointer),NAME('Cla$FILE_SETfl')

      !!! <summary>
      !!! Initializes sequential processing of a VIEW.
      !!! </summary>
      !!! <param name="viewLabel">The label of a VIEW declaration.</param>
      !!! <param name="number">An integer constant, variable or expression that specifies the start position based on the first number of components of the ORDER attribute. If omitted, all ORDER attribute components are used.</param>
      SET(VIEW viewLabel, POINTER_T number=0),NAME('Cla$VIEW_SETvl')
      
      !!! <summary>
      !!! Initializes sequential processing of a KEY.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY or INDEX declaration. When used in the first parameter position, key specifies processing in the sort sequence of the KEY or INDEX.</param>
      SET(KEY keyLabel),NAME('Cla$FILE_SETk')

      !!! <summary>
      !!! Initializes sequential processing of a KEY.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY or INDEX declaration. When used in the first parameter position, key specifies processing in the sort sequence of the KEY or INDEX.</param>
      !!! <param name="sequence">The label of a KEY or INDEX declaration.</param>
      SET(KEY keyLabel, KEY sequence),NAME('Cla$FILE_SETkk')

      !!! <summary>
      !!! Initializes sequential processing of a KEY.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY or INDEX declaration. When used in the first parameter position, key specifies processing in the sort sequence of the KEY or INDEX.</param>
      !!! <param name="sequence">The label of a KEY or INDEX declaration.</param>
      !!! <param name="filePointer">A numeric constant, variable, or expression for the value returned by the POINTER(file) procedure.</param>
      SET(KEY keyLabel, KEY sequence, POINTER_T filePointer),NAME('Cla$FILE_SETkkl')

      !!! <summary>
      !!! Initializes sequential processing of a KEY.
      !!! </summary>
      !!! <param name="keyLabel">The label of a KEY or INDEX declaration. When used in the first parameter position, key specifies processing in the sort sequence of the KEY or INDEX.</param>
      !!! <param name="keyPointer">A numeric constant, variable, or expression for the value returned by the POINTER(key) procedure.</param>
      SET(KEY keyLabel, POINTER_T keyPointer),NAME('Cla$FILE_SETkl')

      !!! <summary>
      !!! Set NULL state of columns in a table.
      !!! </summary>
      !!! <param name="table">The label of a FILE declaration.</param>
      !!! <param name="nullValue">Null "value" to assign to the table..</param>
      SETNULLS(FILE table, STRING nullValue),NAME('Cla$FILE_SETNULLS')

      !!! <summary>
      !!! Returns the passed file status.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration..</param>
      !!! <returns>Zero (0) if the file is not open, and the file's access mode if it is open. If the access mode is actually zero (Read Only / Any Access), 40h (Read Only / Deny None) is returned (see OPEN).</returns>
      STATUS(FILE fileLabel),LONG,NAME('Cla$STATUSfile')

      !!! <summary>
      !!! Frees variables from use in runtime expression strings.
      !!! </summary>
      !!! <param name="name">A string constant that specifies the identifier used by the dynamic expression evaluator. If omitted, all bound variables are unbound.</param>
      UNBIND(STRING name),NAME('Cla$UnbindV')

      !!! <summary>
      !!! Frees variables from use in runtime expression strings.
      !!! </summary>
      UNBIND,NAME('Cla$UnbindA')

      !!! <summary>
      !!! Change state of a dynamic file to unfixed
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE structure, which must be a reference variable.</param>
      UNFIXFORMAT(FILE fileLabel),NAME('Cla$FILE_UNFIXFORMAT')

      _PROC(),TYPE
      _PROC1(STRING),TYPE
      _PROC2(STRING,STRING),TYPE
      _PROC3(STRING,STRING,STRING),TYPE

      !!! <summary>
      !!! Returns the field number of the control on which an EVENT:Accepted event occurred.
      !!! </summary>
      !!! <returns>A SIGNED integer of the field number of the control on which an EVENT:Accepted event occurred.</returns>      
      ACCEPTED(),SIGNED,NAME('Cla$ACCEPTED')

      !!! <summary>
      !!! Returns inverse cosine.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable, or expression for the value of the cosine.</param>
      !!! <returns>A REAL containing the inverse cosine.</returns>      
      ACOS(REAL exp),REAL,NAME('Cla$ACOS')

      !!! <summary>
      !!! Specifies keys that generate an event.
      !!! </summary>
      !!! <param name="firstKeycode">A numeric keycode or keycode equate label. This may be the lower limit in a range of keycodes.</param>
      !!! <param name="lastKeycode">The upper limit keycode, or keycode equate label, in a range of keycodes.</param>
      ALERT(SIGNED firstKeycode=0, SIGNED lastKeycode=0),NAME('Cla$ALERT')

      !!! <summary>
      !!! Changes the keycode generated when the original key is pressed.
      !!! </summary>
      !!! <param name="keycode">A numeric keycode or keycode EQUATE. If both parameters are omitted, all ALIASed keys are reset to their original values.</param>
      !!! <param name="newKeycode">A numeric keycode or keycode EQUATE. If omitted, the keycode is reset to its original value.</param>
      ALIAS(UNSIGNED keycode=0, UNSIGNED newKeycode=0),NAME('Cla$ALIAS')

      !!! <summary>
      !!! Returns repeated characters.
      !!! </summary>
      !!! <param name="str">A string expression containing the character sequence to be repeated.</param>
      !!! <param name="length">The length of the return string. If omitted the length of the return string is 255 characters.</param>
      !!! <returns>A string containing repetitions of the character sequence string.</returns>      
      ALL(STRING str, UNSIGNED length=255),STRING,NAME('Cla$StackALL')

      !!! <summary>
      !!! Draws an arc of an ellipse on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.</param>      
      !!! <param name="height">An integer expression that specifies the height</param>
      !!! <param name="startAngle">An integer expression that specifies the starting point of the arc, in tenths of degrees (10 = 1 degree) measured counter-clockwise from three o'clock.</param>
      !!! <param name="endAngle">An integer expression that specifies the ending point of the arc, in tenths of degrees (10 = 1 degree) measured counter-clockwise from three o'clock</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      ARC(SIGNED x, SIGNED y, SIGNED width, SIGNED height, SIGNED startAngle, SIGNED endAngle, <STRING attributeList>),NAME('Cla$ARC')
      
      !!! <summary>
      !!! Returns inverse sine.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable, or expression for the value of the sine.</param>
      !!! <returns>A REAL that contains the inverse sine.</returns>      
      ASIN(REAL exp),REAL,NAME('Cla$ASIN')

      !!! <summary>
      !!! Reads a single keystroke from the keyboard buffer.
      !!! </summary>
      ASK(),NAME('Cla$ASK')

      !!! <summary>
      !!! Returns inverse tangent.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable, or expression for the value of the tangent.</param>
      !!! <returns>A REAL that contains the inverse tangent.</returns>      
      ATAN(REAL exp),REAL,NAME('Cla$ATAN')

      !!! <summary>
      !!! Generates a sound through the system speaker.
      !!! </summary>
      !!! <param name="sound">A numeric constant, variable, expression, or EQUATE for the Windows sound to issue.</param>
      BEEP(UNSIGNED sound=150, UNSIGNED=16),NAME('Cla$BEEP')

      !!! <summary>
      !!! Identifies variables allowed to be used in dynamic expressions.
      !!! </summary>
      !!! <param name="name">A string constant containing the identifier used in the dynamic expression.</param>
      !!! <param name="exp">A mathematical formula containing any valid combination of variables, functions, operators, and constants.</param>
      BINDEXPRESSION (STRING name, STRING exp),NAME('Cla$BindE')

      !!! <summary>
      !!! Erases all graphics written to the specified area of the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point. If omitted, the default is zero.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point. If omitted, the default is zero.</param>
      !!! <param name="width">An integer expression that specifies the width. If omitted, the default is the width of the window.</param>      
      !!! <param name="height">An integer expression that specifies the height. If omitted, the default is the height of the window.</param>
      BLANK(<SIGNED x>, <SIGNED y>, <SIGNED width>, <SIGNED height>),NAME('Cla$BLANK')

      !!! <summary>
      !!! Flags the beginning of the FILE during sequential processing.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <returns>A LONG non-zero value (true) when the first record in relative file sequence has been read by PREVIOUS or passed by SKIP. Otherwise, the return value is zero (false).</returns>      
      BOF(FILE fileLabel),LONG,NAME('Cla$FILE_BOF')

      !!! <summary>
      !!! Draws a rectangular box on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.</param>      
      !!! <param name="height">An integer expression that specifies the height</param>
      !!! <param name="fill">A LONG integer constant, constant EQUATE, or variable containing the red, green, and blue components that create the color in the three low-order bytes (bytes 0, 1, and 2) or an EQUATE for a standard Windows color value.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      BOX(SIGNED x, SIGNED y, SIGNED width, SIGNED height, LONG fill=COLOR:None, <STRING attributeList>),NAME('Cla$BOX')

      !!! <summary>
      !!! Specifies FILE or VIEW paging.
      !!! </summary>
      !!! <param name="entity">The label of a FILE or VIEW structure.</param>
      !!! <param name="pageSize">An integer constant or variable which specifies the number of records in a single "page" of records (PROP:FetchSize). If omitted, the default value is one (1).</param>
      !!! <param name="behind">An integer constant or variable which specifies the number of "pages" of records to store after they've been read. If omitted, the default value is zero (0).</param>      
      !!! <param name="ahead">An integer constant or variable which specifies the number of additional "pages" of records to read ahead of the currently displayed page. If omitted, the default value is zero (0).</param>
      !!! <param name="timeout">An integer constant or variable which specifies the number of seconds the buffered records are considered not to be obsolete in a network environment. If omitted, the default value is zero (0), which indicates no time limit.</param>
      BUFFER(FILE entity, UNSIGNED pageSize, UNSIGNED behind=0, UNSIGNED ahead=0, UNSIGNED timeout=0),NAME('Cla$FILE_BUFFER')

      !!! <summary>
      !!! Returns number of bytes in FILE, or most recently read.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <returns>The size of a FILE in bytes or the number of bytes in the last record successfully accessed.</returns>      
      BYTES(FILE fileLabel),COUNT_T,NAME('Cla$FILE_BYTES')

      !!! <summary>
      !!! Calls a procedure that has not been prototyped in the application's MAP structure from a Windows standard .DLL.
      !!! </summary>
      !!! <param name="fileName">A string constant, variable, or expression containing the name (including extension) of the .DLL to open. This may include a full path.</param>
      !!! <param name="procName">A string constant, variable, or expression containing the name of the procedure to call (which may not receive parameters or return a value). This can also be the ordinal number indicating the procedure's position within the .DLL.</param>
      !!! <param name="flags">An UNSIGNED integer constant, variable, or expression containing bitmap flag settings.</param>      
      !!! <returns>A SIGNED value. Zero (0) for a successful procedure call. If unsuccessful, it can return one of the available mapped error values</returns>      
      CALL(STRING fileName, STRING procName, <UNSIGNED flags>),SIGNED,PROC,NAME('Cla$CALL')

      !!! <summary>
      !!! Returns centered string.
      !!! </summary>
      !!! <param name="str">A string constant, variable or expression.</param>
      !!! <param name="length">The length of the return string. If omitted, the length of the string parameter is used.</param>
      !!! <returns>A centered string</returns>      
      CENTER(STRING str, UNSIGNED length=-1),STRING,NAME('Cla$StackCENTER')

      !!! <summary>
      !!! Terminates the current program and executes another.
      !!! </summary>
      !!! <param name="progName">A string constant or variable containing the name of the program and optional command line parameters to execute. This may be any .EXE or .COM program.</param>
      CHAIN(STRING progName),NAME('Cla$CHAIN')

      !!! <summary>
      !!! Changes the value displayed in a control in an APPLICATION or WINDOW structure.
      !!! </summary>
      !!! <param name="feq">Field number or field equate label of a window control field.</param>
      !!! <param name="value">A constant, variable, or valid expression containing the control's new value.</param>
      CHANGE(SIGNED feq, STRING value),NAME('Cla$CHANGE')

      !!! <summary>
      !!! Returns a "hash" value for the QUEUE.
      !!! </summary>
      !!! <param name="queueLabel">The label of a QUEUE structure, or the label of a passed QUEUE parameter.</param>
      !!! <returns>A LONG integer containing a unique "hash" value for the current QUEUE contents.</returns>      
      CHANGES(QUEUE queueLabel),LONG,NAME('Cla$NUMCHANGESqueue')

      !!! <summary>
      !!! Returns a user selection number.
      !!! </summary>
      !!! <param name="feq">A field equate label of a LIST, COMBO, SHEET, or OPTION control.</param>
      !!! <returns>The sequence number of a selected item in an OPTION structure, SHEET structure, LIST box, or COMBO control.</returns>      
      CHOICE(SIGNED feq=0),SIGNED,NAME('Cla$CHOICE')

      !!! <summary>
      !!! Draws a closed sector of an ellipse on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.</param>      
      !!! <param name="height">An integer expression that specifies the height</param>
      !!! <param name="startAngle">An integer expression that specifies the starting point of the chord, in tenths of degrees (10 = 1 degree) measured counter-clockwise from three o'clock.</param>
      !!! <param name="endAngle">An integer expression that specifies the ending point of the chord, in tenths of degrees (10 = 1 degree) measured counter-clockwise from three o'clock</param>
      !!! <param name="fill">A LONG integer constant, constant EQUATE, or variable containing the red, green, and blue components that create the color in the three low-order bytes (bytes 0, 1, and 2) or an EQUATE for a standard Windows color value.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      CHORD(SIGNED x, SIGNED y, SIGNED width, SIGNED height, SIGNED startAngle, SIGNED endAngle, LONG fill=COLOR:None, <STRING attributeList>),NAME('Cla$CHORD')

      !!! <summary>
      !!! Removes trailing spaces.
      !!! </summary>
      !!! <param name="str">A string expression.</param>
      !!! <returns>A substring with no trailing spaces.</returns>      
      CLIP(STRING str),STRING,NAME('Cla$StackCLIP')

      !!! <summary>
      !!! Removes trailing spaces.
      !!! </summary>
      !!! <param name="format">An integer constant or variable that defines the format of the clipboard's contents. If omitted, the default is CF_TEXT.</param>
      !!! <returns>The current contents of the windows clipboard.</returns>      
      CLIPBOARD(SIGNED format=0),STRING,NAME('Cla$CLIPBOARD')

      !!! <summary>
      !!! Return system time.
      !!! </summary>
      !!! <returns>The time of day from the operating system time in standard time (expressed as hundredths of a second since midnight, plus one).</returns>      
      CLOCK(),LONG,NAME('Cla$CLOCK')

      !!! <summary>
      !!! Duplicates an existing control.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control to create.</param>
      !!! <param name="fromfeq">A field number or field equate label for the control to duplicate.</param>
      !!! <param name="parentfeq">A field number or field equate label that specifies the OPTION, GROUP, SHEET, TAB, MENU, HEADER, FOOTER, DETAIL, BREAK, or FORM to contain the new control. If omitted, the control has no parent.</param>      
      !!! <param name="position">An integer constant, expression, or variable that specifies the position within a MENU to place a new ITEM control. If omitted, the ITEM is added to the end.</param>
      !!! <param name="wnd">The label of an APPLICATION, WINDOW, or REPORT structure, or a reference to any of those structures where the control to duplicate exists.</param>
      !!! <returns>A SIGNED field equate label of the new control</returns>            
      CLONE(SIGNED feq, SIGNED fromfeq, SIGNED parentfeq=0, SIGNED position=-1, <WINDOW wnd>),SIGNED,PROC,NAME('Cla$CLONE')

      !!! <summary>
      !!! Displays the Windows standard color choice dialog box to allow the user to choose a color.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the color choice dialog. If omitted, a default title is supplied by Windows.</param>
      !!! <param name="rgb">A LONG integer variable to receive the selected color.</param>
      !!! <param name="suppress">An integer constant or variable containing either zero (0) or one (1). If one, the list of standard colors is suppressed. If omitted or zero (0) the list of standard colors is displayed.</param>      
      !!! <returns>The SIGNED color chosen by the user in the rgb parameter.</returns>            
      COLORDIALOG(<STRING title>, *? rgb, SIGNED suppress=0),SIGNED,PROC,NAME('Cla$ColorDialog')

      !!! <summary>
      !!! Returns command line parameters.
      !!! </summary>
      !!! <param name="flag">A string constant or variable containing the parameter for which to search, or the number of the command line parameter to return. If omitted or an empty string (''), all command parameters are returned as entered on the command line, appended to a leading space.</param>
      !!! <returns>The string value of the flag parameter from the command line</returns>      
      COMMAND(<STRING flag>),STRING,PROC,NAME('Cla$COMMAND')

      !!! <summary>
      !!! Terminates an active transaction.
      !!! </summary>
      COMMIT(),NAME('Cla$COMMIT')

      !!! <summary>
      !!! Compresses data from the passed buffer
      !!! </summary>
      !!! <param name="srcBuf">Buffer containing data to compress</param>
      !!! <param name="level">Desired level of compression: from 0 (no compression) to 9 (max compression)</param>
      !!! <param name="result">If compression is successful, passed variable is set to positive number of bytes written to output buffer. If error has occurred, variable is set to negative error code</param>
      !!! <returns>If compression is successfull, reference to buffer with compressed data. If error has occurred or source data is empty, function returns NULL. The program must DISPOSE returned buffer after usage</returns>
      COMPRESS (*STRING srcBuf, SIGNED level = -1, <*SIGNED result>),*STRING,NAME('Cla$zlibCompress')

      !!! <summary>
      !!! Translates ANSI strings to OEM ASCII.
      !!! </summary>
      !!! <param name="str">The label of the string to convert. This may be a single variable or a any structure that is treated as a GROUP (RECORD, QUEUE, etc.)</param>
      CONVERTANSITOOEM(*? str, SIGNED=0),NAME('Cla$ConvertAnsiToOem')

      !!! <summary>
      !!! Translates OEM ASCII strings to ANSI.
      !!! </summary>
      !!! <param name="str">The label of the string to convert. This may be a single variable or a any structure that is treated as a GROUP (RECORD, QUEUE, etc.).</param>
      CONVERTOEMTOANSI(*? str, SIGNED=0),NAME('Cla$ConvertOemToAnsi')

      !!! <summary>
      !!! Returns the value in the USE variable of a control.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label.</param>
      !!! <returns>A string containing the value in the USE variable of an ENTRY, OPTION RADIO, or TEXT control</returns>      
      CONTENTS(SIGNED feq),STRING,NAME('Cla$StackCONTENTS')

      !!! <summary>
      !!! Duplicates a file.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE structure.</param>
      !!! <param name="newFileName">A string constant or variable containing a file specification. If the file specification does not contain a drive and path, the current drive and directory are assumed. If only the path is specified, the filename and extension of the original file are used for the new file.</param>
      COPY(FILE fileLabel, STRING newFileName),NAME('Cla$FILE_COPY')

      !!! <summary>
      !!! Duplicates a file.
      !!! </summary>
      !!! <param name="fileName">A string constant or variable containing the file specification of the file to copy.</param>
      !!! <param name="newFileName">A string constant or variable containing a file specification. If the file specification does not contain a drive and path, the current drive and directory are assumed. If only the path is specified, the filename and extension of the original file are used for the new file.</param>
      COPY(STRING fileName, STRING newFileName),NAME('Cla$CopyFile')

      !!! <summary>
      !!! Returns the cosine.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable or expression for the angle in radians. p is a constant which represents the ratio of the circumference and radius of a circle. There are 2p radians (or 360 degrees) in a circle.</param>
      !!! <returns>The trigonometric cosine of an angle measured in radians.</returns>      
      COS(REAL exp),REAL,NAME('Cla$COS')

      !!! <summary>
      !!! Return standard date.
      !!! </summary>
      !!! <param name="month">A numeric constant, variable, or expression for the month.</param>
      !!! <param name="day">A numeric constant, variable, or expression for the day of the month.</param>
      !!! <param name="year">A numeric constant, variable or expression for the year. The valid range for a year value is 00 through 99 (using "Intellidate" logic), or within the Clarion standard date range.</param>
      !!! <returns>A standard date for a given month, day, and year.</returns>      
      DATE(UNSIGNED month, UNSIGNED day, UNSIGNED year),LONG,NAME('Cla$DATE')

      !!! <summary>
      !!! Returns day of month.
      !!! </summary>
      !!! <param name="stdDate">A numeric constant, variable, expression, or the label of a STRING, CSTRING, or PSTRING variable declared with a date picture token. The date must be a standard date. A variable declared with a date picture token is automatically converted to a standard date intermediate value.</param>
      !!! <returns>The day of the month (1 to 31) for a given standard date</returns>      
      DAY(LONG stdDate),LONG,NAME('Cla$DAY')

      !!! <summary>
      !!! Include a data element into the debugger stack trace list.
      !!! </summary>
      !!! <param name="element">Any valid addressable data element declared in your program that is visible from the scope where the DEBUGHOOK statement is used. Prefix notation or Field Qualification Syntax is permitted.</param>
      DEBUGHOOK(*? element),RAW,NAME('Cla$DEBUGHOOK')

      !!! <summary>
      !!! Deompresses data from the source buffer to deestination buffer
      !!! </summary>
      !!! <param name="destBuf">Buffer to write decompressed data to</param>
      !!! <param name="srcBuf">Buffer containing data to decompress</param>
      !!! <param name="result">If decompression is successful, passed variable is set to positive number of bytes written to destination buffer. If error has occurred, variable is set to negative error code</param>
      DECOMPRESS (*STRING destBuf, *STRING srcBuf, <*SIGNED result>),NAME('Cla$zlibDecompress')

      !!! <summary>
      !!! Deletes a named entry from the system registry.
      !!! </summary>
      !!! <param name="root">The root section of the registry from which to delete the value. Valid values for this are defined in equates.clw </param>
      !!! <param name="keyName">The name of the key to delete, or the key to delete from.</param>
      !!! <param name="valueName">The name of the value to delete. If omitted the whole key is deleted.</param>
      DELETEREG(LONG root, STRING keyName, <STRING valueName>),NAME('Cla$DELETEREG')

      !!! <summary>
      !!! Removes window controls.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      DESTROY(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$DESTROY')

      !!! <summary>
      !!! Gets a file directory listing.
      !!! </summary>
      !!! <param name="result">The label of the QUEUE structure that will receive the directory listing.</param>
      !!! <param name="path">A string constant, variable, or expression that specifies the path and filenames directory listing to get. This may include the wildcard characters (* and ?).</param>
      !!! <param name="attrib">An integer constant, variable, or expression that specifies the attributes of the files to place in the queue.</param>
      DIRECTORY(ff_:queue result,STRING path, BYTE attrib),NAME('Cla$DIRECTORY')

      !!! <summary>
      !!! Gets a file directory listing.
      !!! </summary>
      !!! <param name="result">The label of the QUEUE structure that will receive the directory listing.</param>
      !!! <param name="path">A string constant, variable, or expression that specifies the path and filenames directory listing to get. This may include the wildcard characters (* and ?).</param>
      !!! <param name="attrib">An integer constant, variable, or expression that specifies the attributes of the files to place in the queue.</param>
      DIRECTORY(FILE:Queue fileEntries, STRING path, BYTE attrib),NAME('Cla$LFNDIRECTORY')

      !!! <summary>
      !!! Dims controls on the window.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      DISABLE(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$DISABLE')

      !!! <summary>
      !!! Writes the contents of USE variables to their associated controls.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      DISPLAY(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$DISPLAY')

      !!! <summary>
      !!! Returns matching host and target signatures on a successful drag-and-drop operation.
      !!! </summary>
      !!! <param name="thread">The label of a numeric variable to receive the thread number of the host control. If the host control is in an external program, thread receives zero (0).</param>
      !!! <param name="control">The label of a numeric variable to receive the field equate label of the host control.</param>
      !!! <returns>The matching host and target control signatures on a successful drag-and-drop operation.</returns>      
      DRAGID(<*? thread>, <*? control>),STRING,NAME('Cla$DRAGID')

      !!! <summary>
      !!! Returns matching host and target signatures on a successful drag-and-drop operation.
      !!! </summary>
      !!! <param name="thread">The label of a numeric variable to receive the thread number of the target control. If the target control is in an external program, thread receives zero (0).</param>
      !!! <param name="control">The label of a numeric variable to receive the field equate label of the target control.</param>
      !!! <returns>The matching host and target control signatures on a successful drag-and-drop operation.</returns>      
      DROPID(<*? thread>, <*? control>),STRING,NAME('Cla$DROPID')

      !!! <summary>
      !!! Draws an ellipse on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.</param>      
      !!! <param name="height">An integer expression that specifies the height</param>
      !!! <param name="fill">A LONG integer constant, constant EQUATE, or variable containing the red, green, and blue components that create the color in the three low-order bytes (bytes 0, 1, and 2) or an EQUATE for a standard Windows color value.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      ELLIPSE(SIGNED x, SIGNED y, SIGNED width, SIGNED height, LONG fill=COLOR:None, <STRING attributeList>),NAME('Cla$ELLIPSE')

      !!! <summary>
      !!! Deletes all records from a FILE.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration</param>
      EMPTY(FILE fileLabel),NAME('Cla$FILE_EMPTY')

      !!! <summary>
      !!! Reactivates disabled controls.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      ENABLE(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$ENABLE')

      !!! <summary>
      !!! Forces page overflow.
      !!! </summary>
      !!! <param name="structure">The label of a REPORT structure</param>
      !!! <param name="printFooters">An integer constant or variable. If omitted or zero (0), this prints group footers by forcing a group BREAK (usually used to terminate a report for preview). If one (1), group BREAKs are left open and no group footers print (usually used during a continuing report).</param>
      ENDPAGE(WINDOW structure, SIGNED printFooters=0),NAME('Cla$ENDPAGE')

      !!! <summary>
      !!! Flags the end of the FILE during sequential processing.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <returns>A non-zero value (true) when the last record in relative file sequence has been read by NEXT or passed by SKIP. Otherwise, the return value is zero (false).</returns>      
      EOF(FILE fileLabel),LONG,NAME('Cla$FILE_CEOF')

      !!! <summary>
      !!! Blanks controls and clears their USE variables.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      ERASE(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$ERASE')

      !!! <summary>
      !!! Return an error message.
      !!! </summary>
      !!! <returns>A string containing a description of any error that was posted.</returns>      
      ERROR(),STRING,NAME('Cla$StackErrstr')

      !!! <summary>
      !!! Return an error code number.
      !!! </summary>
      !!! <returns>The code number for any error that was posted. If no error was posted, ERRORCODE returns zero.</returns>      
      ERRORCODE(),SIGNED,NAME('Cla$ERRORCODE')

      !!! <summary>
      !!! Return error file name.
      !!! </summary>
      !!! <returns>The name of the file for which an error was posted.</returns>      
      ERRORFILE(),STRING,NAME('Cla$ERRORFILE')

      !!! <summary>
      !!! Evaluates runtime expression strings.
      !!! </summary>
      !!! <param name="exp">A string constant or variable containing the expression to evaluate.</param>
      !!! <returns>The result as a STRING value. If the expression does not meet the rules of a valid Clarion expression, the result is a null string (''), and ERRORCODE is set.</returns>      
      EVALUATE(STRING exp),STRING,NAME('Cla$Evaluate')

      !!! <summary>
      !!! Return an event number.
      !!! </summary>
      !!! <returns> A SIGNED number indicating what caused ACCEPT to alert the program that something has happened that it may need to handle.</returns>      
      EVENT(),SIGNED,NAME('Cla$EVENT')

      !!! <summary>
      !!! Returns TRUE if the file is available on disk.
      !!! </summary>
      !!! <param name="filename">An expression containing the filename.</param>
      !!! <returns>Returns TRUE if the file is available on disk. If the file is not available, EXISTS returns false (0).</returns>      
      EXISTS(STRING fileName),LONG,NAME('Cla$FileExists')

      !!! <summary>
      !!! Return control with focus
      !!! </summary>
      !!! <returns> A SIGNED number indicating the field number of the control which has focus at the time of any field-specific event.</returns>      
      FIELD(),SIGNED,NAME('Cla$FIELD')

      !!! <summary>
      !!! Displays Windows standard file choice dialogs to allow the user to choose a file.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the dialog. If omitted, Windows supplies a default.</param>
      !!! <param name="filenames">The label of the string variable to receive the selected filename(s).</param>
      !!! <param name="extensions">A string constant or variable containing the available file extension selections for the "List Files of Type" drop list. If omitted, the default is all files (*.*).</param>      
      !!! <param name="flag">An integer constant or variable containing a bitmap to indicate the type of file action to perform.</param>
      FILEDIALOG(<STRING title>, *? fileNames, <STRING extensions>, SIGNED flag=0),PROC,BOOL,NAME('Cla$FileDialog')

      !!! <summary>
      !!! Displays Windows standard file choice dialogs to allow the user to choose a file.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the dialog. If omitted, Windows supplies a default.</param>
      !!! <param name="filenames">The label of the string variable to receive the selected filename(s).</param>
      !!! <param name="extensions">A string constant or variable containing the available file extension selections for the "List Files of Type" drop list. If omitted, the default is all files (*.*).</param>      
      !!! <param name="flag">An integer constant or variable containing a bitmap to indicate the type of file action to perform.</param>
      !!! <param name="index">A signed integer variable used to select a different default extension by specifying the index number.</param>
      FILEDIALOGA(<STRING title>, *? fileNames, <STRING extensions>, SIGNED flag=0, <*SIGNED index>),PROC,BOOL,NAME('Cla$FileDialogA')

      !!! <summary>
      !!! Return file driver error message
      !!! </summary>
      !!! <returns>A string containing the "native" error message from the file system (file driver) being used to access a data file.</returns>      
      FILEERROR(),STRING,NAME('Cla$FILEERRORMSG')

      !!! <summary>
      !!! Return file driver error code number
      !!! </summary>
      !!! <returns>A string containing the code number for the "native" error message from the file system (file driver) being used to access a data file.</returns>      
      FILEERRORCODE(),STRING,NAME('Cla$FILEERRORCODE')

      !!! <summary>
      !!! Return first window control
      !!! </summary>
      !!! <returns>A signed number containing the lowest field number in the currently active window (or REPORT) as specified by SETTARGET.</returns>      
      FIRSTFIELD(),SIGNED,NAME('Cla$FIRSTFIELD')

      !!! <summary>
      !!! Terminates a STREAM operation, flushing the operating system buffers to disk, or flushes the BUFFER statement's buffers.
      !!! </summary>
      !!! <param name="filelabel">The label of a FILE or VIEW.</param>
      FLUSH(FILE fileLabel),NAME('Cla$FILE_FLUSH')

      !!! <summary>
      !!! Return control with focus
      !!! </summary>
      !!! <returns>The field number of the control which has received the last EVENT:SELECTED event among the other controls (not menu items) of the active window.</returns>      
      FOCUS(),SIGNED,NAME('Cla$FOCUS')

      !!! <summary>
      !!! Displays the standard Windows font choice dialog box to allow the user to choose a font.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the font choice dialog. If omitted, a default title is supplied by Windows.</param>
      !!! <param name="typeface">A string variable to receive the name of the chosen font.</param>
      !!! <param name="size">An integer variable to receive the size (in points) of the chosen font.</param>      
      !!! <param name="color">A LONG integer variable to receive the red, green, and blue values for the color of the chosen font in the low-order three bytes.</param>
      !!! <param name="style">An integer variable to receive the strike weight and style of the chosen font.</param>
      !!! <param name="added">An integer constant or variable that specifies adding screen or printer fonts, or both, to the list of available fonts. Zero (0) adds screen fonts, one (1) adds printer fonts, and two (2) adds both. If omitted, only Windows registered fonts are listed.</param>
      FONTDIALOG(<STRING title>, *? typeface, <*? size>, <*? color>, <*? style>, SIGNED added=0),BOOL,PROC,NAME('Cla$FontDialog')

      !!! <summary>
      !!! Displays the standard Windows font choice dialog box to allow the user to choose a font and character set.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the font choice dialog. If omitted, a default title is supplied by Windows.</param>
      !!! <param name="typeface">A string variable to receive the name of the chosen font.</param>
      !!! <param name="size">An integer variable to receive the size (in points) of the chosen font.</param>      
      !!! <param name="color">A LONG integer variable to receive the red, green, and blue values for the color of the chosen font in the low-order three bytes.</param>
      !!! <param name="style">An integer variable to receive the strike weight and style of the chosen font.</param>
      !!! <param name="charset">A LONG integer variable to receive the character set value.</param>
      !!! <param name="added">An integer constant or variable that specifies adding screen or printer fonts, or both, to the list of available fonts. Zero (0) adds screen fonts, one (1) adds printer fonts, and two (2) adds both. If omitted, only Windows registered fonts are listed.</param>
      FONTDIALOGA(<STRING title>, *? typeface, <*? size>, <*? color>, <*? style>, <*? charset>, SIGNED added=0),BOOL,PROC,NAME('Cla$FontDialogA')

      !!! <summary>
      !!! Passes a keystroke to another control
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control to receive the passed keystroke.</param>
      FORWARDKEY(SIGNED feq),NAME('Cla$FORWARDKEY')

      !!! <summary>
      !!! Deletes all entries from a QUEUE.
      !!! </summary>
      !!! <param name="queueLabel">The label of a QUEUE structure, or the label of a passed QUEUE parameter.</param>
      FREE(QUEUE queueLabel),NAME('Cla$FREEqueuea')

      !!! <summary>
      !!! Suspends redrawing of the control in the current thread's top window
      !!! </summary>
      !!! <param name="ctl">The control to freeze
      !!! <returns>Drawing status which must be passed to the UNFREEZE procedure
      FREEZE(SIGNED ctl),LONG,NAME('Cla$FREEZEcontrol')

      !!! <summary>
      !!! Returns the exit code of the process
      !!! </summary>
      GETEXITCODE(),SIGNED,NAME('Cla$GETEXITCODE')

      !!! <summary>
      !!! Gets display font information.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control from which to get the information. If control is zero (0), it specifies the WINDOW.</param>
      !!! <param name="typeface">A string variable to receive the name of the chosen font.</param>
      !!! <param name="size">An integer variable to receive the size (in points) of the chosen font.</param>      
      !!! <param name="color">A LONG integer variable to receive the red, green, and blue values for the color of the chosen font in the low-order three bytes.</param>
      !!! <param name="style">An integer variable to receive the strike weight and style of the chosen font.</param>
      GETFONT(SIGNED feq, <*? typeface>, <*? size>, <*? color>, <*? style>),NAME('Cla$GETFONT')

      !!! <summary>
      !!! Gets display font information.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control from which to get the information. If control is zero (0), it specifies the WINDOW.</param>
      !!! <param name="typeface">A string variable to receive the name of the chosen font.</param>
      !!! <param name="size">An integer variable to receive the size (in points) of the chosen font.</param>      
      !!! <param name="color">A LONG integer variable to receive the red, green, and blue values for the color of the chosen font in the low-order three bytes.</param>
      !!! <param name="style">An integer variable to receive the strike weight and style of the chosen font.</param>
      !!! <param name="charset">A LONG integer variable to receive the character set value.</param>
      GETFONT(SIGNED feq, <*? typeface>, <*? size>, <*? color>, <*? style>, *? charset),NAME('Cla$GETFONTEX')

      !!! <summary>
      !!! Returns a reference to a specified group structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP or QUEUE declaration.</param>
      !!! <param name="number">An integer expression specifying the ordinal position of a GROUP or QUEUE in the specified group.</param>
      !!! <param name="dim">An optional dimension element number, if applicable.</param>
      !!! <returns> A GROUP reference to the number specified field from a target group structure.</returns>      
      GETGROUP(*GROUP label, SIGNED number, SIGNED dim=0),STRING,NAME('Cla$GETGROUP')

      !!! <summary>
      !!! Returns a reference to a specified group structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP or QUEUE declaration.</param>
      !!! <param name="number">An integer expression specifying the ordinal position of a GROUP or QUEUE in the specified group.</param>
      !!! <param name="dim">An optional dimension element number, if applicable.</param>
      !!! <returns> A GROUP reference to the number specified field from a target group structure.</returns>      
      GETGROUP(QUEUE label, SIGNED number, SIGNED dim=0),STRING,NAME('Cla$GETGROUPqueue')

      !!! <summary>
      !!! Gets the value of a specific key and/or value from the system registry.
      !!! </summary>
      !!! <param name="root">A LONG integer, variable or expression that contains the root section of the registry from which to obtain the value. Valid values for this are defined in equates.clw </param>
      !!! <param name="keyName">A STRING constant, variable or expression that contains the key name of the key whose value is to be queried. This may contain a path separated by backslash ‘\’ characters.</param>
      !!! <param name="valueName">A STRING constant, variable or expression that contains the name of the value to be queried, if omitted, the value associated directly with the key is returned.</param>
      !!! <param name="valueType">An optional integer variable that receives the type of value. It can be used for correct interpreting of the returned valuename.</param>
      !!! <returns>The string value of named entry in the system registry as a Clarion string.</returns>      
      GETREG(LONG root, STRING keyName, <STRING valueName>, <*? valueType>),STRING,NAME('Cla$GETREG')
 
      !!! <summary>
      !!! Collect names of sub-keys for given registry key
      !!! </summary>
      !!! <param name="root">A LONG integer, variable or expression that contains the root section of the registry from which to obtain the value. Valid values for this are defined in equates.clw </param>
      !!! <param name="keyName">A STRING constant, variable or expression that contains the key name of the key whose value is to be queried. This may contain a path separated by backslash ‘\’ characters.</param>
      !!! <param name="subkeyQueue">A QUEUE to write sub-keys of "keyName" key to
      GETREGSUBKEYS(LONG root, STRING keyName, *QUEUE subkeyQueue),NAME('Cla$GETREGSUBKEYS')

      !!! <summary>
      !!! Collect names of values for given registry key
      !!! </summary>
      !!! <param name="root">A LONG integer, variable or expression that contains the root section of the registry from which to obtain the value. Valid values for this are defined in equates.clw </param>
      !!! <param name="keyName">A STRING constant, variable or expression that contains the key name of the key whose value is to be queried. This may contain a path separated by backslash ‘\’ characters.</param>
      !!! <param name="valueQueue">A QUEUE to write names of values of "keyName" key to
      GETREGVALUES(LONG root, STRING keyName, *QUEUE valueQueue),NAME('Cla$GETREGVALUES')

      !!! <summary>
      !!! Returns the value for an INI file entry.
      !!! </summary>
      !!! <param name="INISection">A string constant or variable containing the name of the portion of the INI file which contains the entry.</param>
      !!! <param name="entry">A string constant or variable containing the name of the specific setting for which to return the value.</param>
      !!! <param name="default">A string constant or variable containing the default value to return if the entry does not exist (up to 1023 characters). If omitted and the entry does not exist, GETINI returns an empty string.</param>
      !!! <param name="fileName">A string constant or variable containing the name of the INI file to search. A file name without a full path (e.g. 'Autolog.ini') looks for the file in the Windows directory. To look for the file in the current directory enter a dot path (e.g. '.\Autolog.ini'). If the file does not exist, GETINI returns an empty string. If omitted, GETINI searches the WIN.INI file.</param>
      !!! <returns>The value of an entry in a Windows-standard INI file (maximum file size is 64K).</returns>      
      GETINI(STRING INIsection, STRING entry, <STRING default>, <STRING fileName>),STRING,NAME('Cla$GETINI')

      !!! <summary>
      !!! Gets the position and size of an APPLICATION, WINDOW, REPORT, or control.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control from which to get the information. If control is zero (0), it specifies the window.</param>
      !!! <param name="x">An integer variable to receive the horizontal position of the top left corner.</param>
      !!! <param name="y">An integer variable to receive the vertical position of the top left corner.</param>
      !!! <param name="width">An integer variable to receive the width.</param>
      !!! <param name="height">An integer variable to receive the height.</param>
      GETPOSITION(SIGNED feq, <*? x>, <*? y>, <*? width>, <*? height>),NAME('Cla$GETPOSITION')

      !!! <summary>
      !!! Gets the position and size of an APPLICATION, WINDOW, or REPORT.
      !!! </summary>
      !!! <param name="wnd">The label of an APPLICATION, WINDOW, or REPORT structure, or a reference to any of those structures.</param>
      !!! <param name="x">An integer variable to receive the horizontal position of the top left corner.</param>
      !!! <param name="y">An integer variable to receive the vertical position of the top left corner.</param>
      !!! <param name="width">An integer variable to receive the width.</param>
      !!! <param name="height">An integer variable to receive the height.</param>
      GETPOSITION(<WINDOW wnd>, <*? x>, <*? y>, <*? width>, <*? height>),NAME('Cla$GETPOSITIONw')

      !!! <summary>
      !!! Immediately terminates the program.
      !!! </summary>
      !!! <param name="errorLevel">A positive integer constant or variable which is the exit code to pass to DOS, setting the DOS ERRORLEVEL. If omitted, the default is zero.</param>
      !!! <param name="message">A string constant or variable which is typed on the screen after program termination.</param>
      HALT(UNSIGNED errorLevel=0, <STRING message>),NAME('Cla$HALT')

      !!! <summary>
      !!! Opens a help file and activates a help window.
      !!! </summary>
      !!! <param name="helpFile">A string constant or the label of a STRING variable that has the DOS directory file specification for the help file. If the file specification does not contain a complete path and filename, the help file is assumed to be in the current directory. If the file extension is omitted, ".HLP" is assumed. If the helpfile parameter is omitted, a comma is required to hold its position.</param>
      !!! <param name="windowId">A string constant or the label of a STRING variable that contains the key used to access the help system. This may be either a help keyword or a "context string." </param>
      HELP(<STRING helpFile>, <STRING windowId>),NAME('Cla$HELP')

      !!! <summary>
      !!! Hides window controls.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      HIDE(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$HIDE')

      !!! <summary>
      !!! Returns the number of dimensions for a designated element.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, QUEUE or RECORD.</param>
      !!! <param name="element">A LONG constant or variable that identifies the ordinal position of the target element to examine.</param>
      !!! <returns>The number of dimensions for the n'th field of a GROUP, QUEUE or RECORD.</returns>      
      HOWMANY(*GROUP label, SIGNED element),LONG,NAME('Cla$HOWMANY')

      !!! <summary>
      !!! Returns the number of dimensions for a designated element.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, QUEUE or RECORD.</param>
      !!! <param name="element">A LONG constant or variable that identifies the ordinal position of the target element to examine.</param>
      !!! <returns>The number of dimensions for the n'th field of a GROUP, QUEUE or RECORD.</returns>      
      HOWMANY(QUEUE label, SIGNED element),LONG,NAME('Cla$HOWMANYqueue')

      !!! <summary>
      !!! Arms a procedure that periodically executes.
      !!! </summary>
      !!! <param name="proclabel">The label of a PROCEDURE. The procedure may not take any parameters.</param>
      !!! <param name="separation">An integer that specifies the minimum wait time (in seconds) between calls to the procedure. A separation of 0 specifies continuous calls. If separation is omitted, the default value is 1 second.</param>
      IDLE(<_PROC procLabel>, UNSIGNED separation=1),NAME('Cla$IDLE')

      !!! <summary>
      !!! Places a graphic image on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.This may be a negative number. If omitted, defaults to the width of the graphic as it is stored.</param>      
      !!! <param name="height">An integer expression that specifies the height. This may be a negative number. If omitted, defaults to the height of the graphic as it is stored.</param>
      !!! <param name="fileName">A string constant or variable containing the name of the file to display.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      IMAGE(SIGNED x, SIGNED y, SIGNED width=0, SIGNED height=0, STRING fileName, <STRING attributeList>),NAME('Cla$IMAGE')

      !!! <summary>
      !!! Return empty REQ control
      !!! </summary>
      !!! <returns>A signed number containing the field number of the first control with the REQ attribute in the currently active window that has been left zero or blank, and gives input focus to that control.</returns>      
      INCOMPLETE(),SIGNED,NAME('Cla$INCOMPLETE')

      !!! <summary>
      !!! Returns the address of a variable or entity’s thread instance
      !!! </summary>
      !!! <param name="variable">Label of a variable, field, FILE, KEY or QUEUE or reference variable.</param>
      !!! <param name="threadno">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      !!! <returns>The address of the variable instance allocated for the active thread referenced by threadno</returns>      
      INSTANCE(*? variable, SIGNED threadno),POINTER_T,RAW,NAME('Cla$INSTANCE')

      !!! <summary>
      !!! Returns the address of a variable or entity’s thread instance
      !!! </summary>
      !!! <param name="variable">Label of a variable, field, FILE, KEY or QUEUE or reference variable.</param>
      !!! <param name="threadno">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      !!! <returns>The address of the variable instance allocated for the active thread referenced by threadno</returns>      
      INSTANCE(FILE variable, SIGNED threadno),POINTER_T,NAME('Cla$INSTANCE')

      !!! <summary>
      !!! Returns the address of a variable or entity’s thread instance
      !!! </summary>
      !!! <param name="variable">Label of a variable, field, FILE, KEY or QUEUE or reference variable.</param>
      !!! <param name="threadno">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      !!! <returns>The address of the variable instance allocated for the active thread referenced by threadno</returns>      
      INSTANCE(KEY variable, SIGNED threadno),POINTER_T,NAME('Cla$INSTANCE')

      !!! <summary>
      !!! Returns the address of a variable or entity’s thread instance
      !!! </summary>
      !!! <param name="variable">Label of a variable, field, FILE, KEY or QUEUE or reference variable.</param>
      !!! <param name="threadno">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      !!! <returns>The address of the variable instance allocated for the active thread referenced by threadno</returns>      
      INSTANCE(QUEUE variable, SIGNED threadno),POINTER_T,NAME('Cla$INSTANCE')

      !!! <summary>
      !!! Searches for a substring in a string.
      !!! </summary>
      !!! <param name="substring">A string constant, variable, or expression that contains the string for which to search. You should CLIP a variable substring so INSTRING will not look for a match that contains the trailing spaces in the variable.</param>
      !!! <param name="str">A string constant, or the label of the STRING, CSTRING, or PSTRING variable to be searched.</param>
      !!! <param name="step">A numeric constant, variable, or expression which specifies the step length of the search. A step of 1 will search for the substring beginning at every character in the string, a step of 2 starts at every other character, and so on. A negative step value (-1) will search from right to left within the string. If step is omitted, the step length defaults to the length of the substring.</param>
      !!! <param name="start">A numeric constant, variable, or expression which specifies where to begin the search of the string. If omitted, the search starts at the first character position. In Clarion#, start is zero (0) based (i.e., zero (0) starts at the first character position).</param>
      !!! <returns>The step number on which the substring was found. If the substring is not found in the string, INSTRING returns zero.</returns>      
      INSTRING(STRING substring, STRING str, UNSIGNED step=0, UNSIGNED start=1),UNSIGNED,NAME('Cla$StackINSTRING')

      !!! <summary>
      !!! Returns whether the string passed to it contains an alphabetic character.
      !!! </summary>
      !!! <param name="str">The label of the character string to test. If the string contains more than one character, only the first character is tested.</param>
      !!! <returns>TRUE if the string passed to it is alphabetic (an upper or lower case letter) and false otherwise.</returns>      
      ISALPHA(STRING str),SIGNED,NAME('Cla$IsAlpha')

      !!! <summary>
      !!! Returns true if the element is a GROUP data type.
      !!! </summary>
      !!! <param name="label">The label of a GROUP or QUEUE.</param>
      !!! <param name="element">An integer expression specifying the ordinal position of a field in the GROUP or QUEUE.</param>
      !!! <returns>TRUE if the element of a GROUP or QUEUE is a GROUP data type</returns>      
      ISGROUP(*GROUP label, SIGNED element),SIGNED,NAME('Cla$ISGROUP')

      !!! <summary>
      !!! Returns true if the element is a GROUP data type.
      !!! </summary>
      !!! <param name="label">The label of a GROUP or QUEUE.</param>
      !!! <param name="element">An integer expression specifying the ordinal position of a field in the GROUP or QUEUE.</param>
      !!! <returns>TRUE if the element of a GROUP or QUEUE is a GROUP data type</returns>      
      ISGROUP(QUEUE label, SIGNED element),SIGNED,NAME('Cla$ISGROUPqueue')

      !!! <summary>
      !!! Returns whether the string passed to it contains a lower case alphabetic character.
      !!! </summary>
      !!! <param name="str">The label of the character string to test. If the string contains more than one character, only the first character is tested.</param>
      !!! <returns>TRUE if the string passed to it is a lower case letter and false otherwise.</returns>      
      ISLOWER(STRING str),SIGNED,NAME('Cla$IsLower')

      !!! <summary>
      !!! Returns whether the string passed to it contains an upper case alphabetic character.
      !!! </summary>
      !!! <param name="str">The label of the character string to test. If the string contains more than one character, only the first character is tested.</param>
      !!! <returns>TRUE if the string passed to it is an upper case letter and false otherwise.</returns>      
      ISUPPER(STRING str),SIGNED,NAME('Cla$IsUpper')

      !!! <summary>
      !!! Returns true if the field is a GROUP, STRING, CSTRING, or PSTRING data type.
      !!! </summary>
      !!! <param name="field">The label of the field to test.</param>
      !!! <returns>TRUE if the field is a GROUP, STRING, CSTRING, or PSTRING data type.</returns>      
      ISSTRING(? field),SIGNED,NAME('Cla$IsUfoString')

      !!! <summary>
      !!! Return keystroke waiting
      !!! </summary>
      !!! <returns>The keycode of the first keystroke in the keyboard buffer.</returns>      
      KEYBOARD(),UNSIGNED,NAME('Cla$KEYBOARD')

      !!! <summary>
      !!! Return ASCII code
      !!! </summary>
      !!! <returns>The ASCII value of the last key pressed at the time the event occurred</returns>      
      KEYCHAR(),UNSIGNED, NAME('Cla$KEYCHAR')

      !!! <summary>
      !!! Return last keycode
      !!! </summary>
      !!! <returns>The keycode of the last key pressed at the time the event occurred, or the last keycode value set by the SETKEYCODE procedure.</returns>      
      KEYCODE(),UNSIGNED,NAME('Cla$KEYCODE')

      !!! <summary>
      !!! Return keyboard status
      !!! </summary>
      !!! <returns>An unsinged bitmap containing the status of the SHIFT, CTRL, ALT, any extended key</returns>      
      KEYSTATE(),UNSIGNED,NAME('Cla$KEYSTATE')

      !!! <summary>
      !!! Return last window control
      !!! </summary>
      !!! <returns>the highest field number in the currently active window (or REPORT) as specified by SETTARGET. This does not include any controls in a TOOLBAR or MENUBAR.</returns>      
      LASTFIELD(),SIGNED,NAME('Cla$LASTFIELD')
      
      !!! <summary>
      !!! Left justifies a string.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression.</param>
      !!! <param name="length">A numeric constant, variable, or expression for the length of the return string. If omitted, length defaults to the length of the string.</param>
      !!! <returns>A left justified string</returns>      
      LEFT(STRING str, UNSIGNED length=-1),STRING,NAME('Cla$StackLEFT')

      !!! <summary>
      !!! Returns length of a string
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression.</param>
      !!! <returns>Unsigned number designating the length of a string</returns>  
      LEN(STRING str),UNSIGNED,NAME('Cla$StackLen')
      
      !!! <summary>
      !!! Draws a straight line on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.This may be a negative number. </param>      
      !!! <param name="height">An integer expression that specifies the height. This may be a negative number. </param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      LINE(SIGNED x, SIGNED y, SIGNED width, SIGNED height, <STRING attributeList>),NAME('Cla$LINE')

      !!! <summary>
      !!! Allows the user to load a specific environment file (.ENV) at run-time and also to set individual environment settings.
      !!! </summary>
      !!! <param name="setting">A string constant or variable containing the name (including extension) of the environment file (.ENV) to load, or the keyword WINDOWS. This may be a fully-qualified DOS pathname.</param>
      !!! <param name="value">A string constant or variable containing the environment variable setting</param>
      LOCALE(STRING setting, <STRING value>),NAME('Cla$Locale')

      !!! <summary>
      !!! The LOCKTHREAD statement re-locks the current execution thread that has been unlocked with the UNLOCKTHREAD statement.
      !!! </summary>
      LOCKTHREAD(),NAME('Cla$LOCKTHREAD')

      !!! <summary>
      !!! Returns base 10 logarithm
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable, or expression. If the value of the expression is zero or less, the return value will be zero. The base 10 logarithm is undefined for values less than or equal to zero.</param>
      !!! <returns>Real number representing the base 10 logarithm of a numeric expression.</returns>  
      LOG10(REAL exp),REAL,NAME('Cla$LOG10')

      !!! <summary>
      !!! Returns the natural logarithm
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable, or expression. If the value of the expression is zero or less, the return value will be zero. The natural logarithm is undefined for values less than or equal to zero.</param>
      !!! <returns>Real number representing the natural logarithm of a numeric expression.</returns>  
      LOGE(REAL exp),REAL,NAME('Cla$LOGE')

      !!! <summary>
      !!! Returns the fully-qualified long filename for a given short filename.
      !!! </summary>
      !!! <param name="shortFileName">A string constant, variable, or expression that specifies the DOS standard filename to convert. This may include the complete path. If omitted, LONGPATH returns the current drive and directory in long name form.</param>
      !!! <returns>The long filename for a given short filename</returns>  
      LONGPATH(<STRING shortFileName>),STRING,NAME('Cla$LONGPATH')

      !!! <summary>
      !!! Converts a string to all lower case.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression for the string to be converted.</param>
      !!! <returns>A string with all letters converted to lower case</returns>  
      LOWER(STRING str),STRING,NAME('Cla$StackLOWER')

      !!! <summary>
      !!! Returns true or false based on a comparison of the first two parameters passed.
      !!! </summary>
      !!! <param name="first">A string containing data to compare against the second parameter. String constants must be enclosed in single quotes.</param>
      !!! <param name="second">A string containing data to compare against the first parameter. For Wild and Regular modes second must contain the pattern or expression to match. String constants must be enclosed in single quotes.</param>
      !!! <param name="mode">An integer constant or equate which specifies the method of comparison. If omitted, a wild card comparison is the default.</param>
      !!! <returns>True or false as to whether the first and second parameters match according to the comparison mode specified. The mode value EQUATEs are listed in EQUATES.CLW</returns>      
      MATCH(STRING first, STRING second, BYTE mode=Match:Wild),LONG,NAME('Cla$MATCH')

      !!! <summary>
      !!! Displays a message dialog box and returns the button the user pressed
      !!! </summary>
      !!! <param name="text">A string constant or variable containing the text to display in the message box. A vertical bar (|) in the text indicates a line break for multi-line messages. Including an ASCII 9 in the text inserts a tab for text alignment.</param>
      !!! <param name="caption">The dialog box title. If omitted, the dialog has no title.</param>
      !!! <param name="icon">A string constant or variable naming the .ICO file to display, or an EQUATE for one of Windows' standard icons (these EQUATEs are listed in EQUATES.CLW). If omitted, no icon is displayed on the dialog box.</param>      
      !!! <param name="buttons">Either an integer expression which indicates which Windows standard buttons (may indicate multiple buttons) to place on the dialog box, or a string expression containing a vertical bar (|) delimited list of the text for up to 8 buttons. If omitted, the dialog displays an Ok button.</param>
      !!! <param name="default">An integer constant, variable, EQUATE, or expression which indicates the default button on the dialog box. If omitted, the first button is the default.</param>
      !!! <param name="style">The style parameter is a bitmap integer constant, variable, EQUATE, or expression that specifies the type of modal behavior, and whether or not the text of the message can be copied to the Windows Clipboard.</param>
      !!! <returns>An unsigned number representing the number of the button the user presses to exit the dialog box.</returns>      
      MESSAGE(STRING text, <STRING caption>, <STRING icon>, <STRING buttons>, UNSIGNED default=0, UNSIGNED style=0),UNSIGNED,PROC,NAME('Cla$MessageBox')

      !!! <summary>
      !!! Returns month in year.
      !!! </summary>
      !!! <param name="stdDate">A numeric constant, variable, expression, or the label of a STRING, CSTRING, or PSTRING variable declared with a date picture token. The date must be a standard date. A variable declared with a date picture token is automatically converted to a standard date intermediate value.</param>
      !!! <returns>A LONG value representing the month of the year (1 to 12) for a given standard date</returns>  
      MONTH(LONG stdDate),LONG,NAME('Cla$MONTH')

      !!! <summary>
      !!! Return mouse horizontal position
      !!! </summary>
      !!! <returns>A SIGNED numeric value corresponding to the current horizontal position of the mouse cursor at the time of the event.</returns>      
      MOUSEX(),SIGNED,NAME('Cla$MOUSEX')

      !!! <summary>
      !!! Return mouse vertical position
      !!! </summary>
      !!! <returns>A SIGNED numeric value corresponding to the current vertical position of the mouse cursor at the time of the event.</returns>      
      MOUSEY(),SIGNED,NAME('Cla$MOUSEY')
      
      !!! <summary>
      !!! Returns name of a file.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <returns>A string containing the operating system device name for the structure identified by the label.</returns>  
      NAME(FILE fileLabel),STRING,NAME('Cla$FILE_NAME')

      !!! <summary>
      !!! Reads the next record in sequence from a FILE or VIEW.
      !!! </summary>
      !!! <param name="entity">The label of a FILE or VIEW declaration.</param>
      NEXT(FILE entity),NAME('Cla$FILE_NEXT')

      !!! <summary>
      !!! Arms "memoless" record retrieval.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      NOMEMO(FILE fileLabel),NAME('Cla$FILE_NOMEMO')

      !!! <summary>
      !!! Receives information from a sender thread.
      !!! </summary>
      !!! <param name="notifyCode">An UNSIGNED variable that receives a notify code value passed by the sender with a NOTIFY statement.</param>
      !!! <param name="thread">An optional SIGNED variable that receives the number of the sender's thread parameter.</param>
      !!! <param name="parameter">A LONG variable that receives the parameter passed by the sender with a NOTIFY statement</param>
      !!! <returns>FALSE (0) if the EVENT() function returns an event other than EVENT:Notify. If EVENT:Notify is posted, NOTIFICATION returns TRUE.</returns>  
      NOTIFICATION(*UNSIGNED notifyCode, <*SIGNED thread>, <*LONG parameter>),BOOL,PROC,NAME('Cla$NOTIFICATION')

      !!! <summary>
      !!! Sends an event and optional parameter to a receiving thread.
      !!! </summary>
      !!! <param name="notifyCode">An unsigned integer value that indicates the notification or request code.</param>
      !!! <param name="thread">A signed integer that identifies the number of the receiver thread; if omitted or equal to 0, the current thread is the receiver.</param>
      !!! <param name="parameter">An optional LONG value that is used as a parameter of the notification or request</param>
      NOTIFY(UNSIGNED notifyCode, SIGNED thread=0, LONG parameter=0),NAME('Cla$NOTIFY')

      !!! <summary>
      !!! Validates all numeric string.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression.</param>
      !!! <returns>TRUE if the string only contains a valid numeric value. It returns zero (false) if the string contains any non-numeric characters.</returns>  
      NUMERIC(STRING str),UNSIGNED,NAME('Cla$StackNUMERIC')

      !!! <summary>
      !!! Removes deleted records from a data file and rebuilds its keys.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      PACK(FILE fileLabel),NAME('Cla$FILE_PACK')

      !!! <summary>
      !!! Return current directory
      !!! </summary>
      !!! <returns>A string containing the current drive and directory</returns>  
      PATH(<STRING>),STRING,NAME('Cla$SHORTPATH')

      !!! <summary>
      !!! Return line draw color
      !!! </summary>
      !!! <returns>A LONG number representing the current pen color set by SETPENCOLOR</returns>  
      PENCOLOR(),LONG,NAME('Cla$PENCOLOR')

      !!! <summary>
      !!! Return line draw thickness
      !!! </summary>
      !!! <returns>A SIGNED number representing the current line draw thickness set by SETPENWIDTH</returns>  
      PENWIDTH(),SIGNED,NAME('Cla$PENWIDTH')

      !!! <summary>
      !!! Return line draw style
      !!! </summary>
      !!! <returns>A SIGNED number representing the current line draw style set by SETPENSTYLE.</returns>  
      PENSTYLE(),SIGNED,NAME('Cla$PENSTYLE')

      !!! <summary>
      !!! Draws a pie chart on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.</param>      
      !!! <param name="height">An integer expression that specifies the height.</param>
      !!! <param name="slices">A SIGNED array of values that specify the relative size of each slice of the pie.</param>
      !!! <param name="colors">A LONG array that specifies the fill color for each slice.</param>
      !!! <param name="depth">An integer expression that specifies the depth of the three-dimensional pie chart. If omitted, the chart is two-dimensional.</param>
      !!! <param name="wholeValue">A numeric constant or variable that specifies the total value required to create a complete pie chart. If omitted, the sum of the slices array is used.</param>
      !!! <param name="startangle">A numeric constant or variable that specifies the starting point of the first slice of the pie, measured as a fraction of the wholevalue. If omitted (or zero), the first slice starts at the twelve o'clock position.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      PIE(SIGNED x, SIGNED y, SIGNED width, SIGNED height, *SIGNED[] slices, *LONG[] colors, SIGNED depth=0, UNSIGNED wholeValue=0, SIGNED startAngle=0, <STRING attributeList>),NAME('Cla$PIE')

      !!! <summary>
      !!! Draws a multi-sided figure on the current window or report.
      !!! </summary>
      !!! <param name="array">An array of SIGNED integers that specify the x and y coordinates of each "corner point" of the polygon.</param>
      !!! <param name="fill">A LONG integer constant, constant EQUATE, or variable containing the red, green, and blue components that create the color in the three low-order bytes (bytes 0, 1, and 2) or an EQUATE for a standard Windows color value.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      POLYGON(*SIGNED[] array, LONG fill=COLOR:None, <STRING attributeList>),NAME('Cla$POLYGON')

      !!! <summary>
      !!! Restores the previous BIND statement's name space for variables and procedures previously bound.
      !!! </summary>
      POPBIND(),NAME('Cla$PopBind')

      !!! <summary>
      !!! Returns the last error information saved by PUSHERRORS()
      !!! </summary>
      POPERRORS(),NAME('Cla$poperrcodes')

      !!! <summary>
      !!! Returns an integer indicating the user's choice from the menu
      !!! </summary>
      !!! <param name="selections">A string constant, variable, or expression containing the text for the menu choices.</param>
      !!! <param name="x">An integer constant, variable, or expression that specifies the horizontal position of the menu’s top left corner. If omitted, the menu appears at the current cursor position.</param>
      !!! <param name="y">An integer constant, variable, or expression that specifies the vertical position of the menu’s top left corner. If omitted, the menu appears at the current cursor position.</param>
      !!! <param name="relPos">A BYTE value that if set to 1 specifies that the popup coordinates (2nd and 3rd parameters) are treated as relative to the upper left corner of the active window's client area. If set to 0 (the default), they are screen coordinates.</param>
      !!! <returns>A signed integer indicating the user's choice from the popup menu that appears when the procedure is invoked.</returns>  
      POPUP(STRING selections, SIGNED x=_nopos,SIGNED y=_nopos,BYTE relPos=FALSE),SIGNED,NAME('Cla$POPUP')

      !!! <summary>
      !!! Posts an event
      !!! </summary>
      !!! <param name="event">An integer constant, variable, expression, or EQUATE containing an event number. A value in the range 400h to 0FFFh is a User-defined event.</param>
      !!! <param name="feq">An integer constant, EQUATE, variable, or expression containing the field number of the control affected by the event. If omitted, the event is field-independent.</param>
      !!! <param name="thread">An integer constant, EQUATE, variable, or expression containing the execution thread number whose ACCEPT loop is to process the event. If omitted, the event is posted to the current thread.</param>      
      !!! <param name="toFront">An integer constant, EQUATE, variable, or expression containing either zero (0) or one (1). If one (1), the event message is placed at the front of the event message queue. If omitted or zero (0), the event message is placed at the end of the event message queue.</param>
      POST(UNSIGNED event, SIGNED feq=0, SIGNED thread=0, SIGNED toFront=0),NAME('Cla$POST')

      !!! <summary>
      !!! Places characters in the keyboard input buffer.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression.</param>
      PRESS(STRING str),NAME('Cla$StackPRESS')

      !!! <summary>
      !!! Places one keystroke in the keyboard input buffer.
      !!! </summary>
      !!! <param name="keyCode">An integer constant or keycode EQUATE label.</param>
      PRESSKEY(UNSIGNED keyCode),NAME('Cla$PRESSKEY')

      !!! <summary>
      !!! Reads the previous record in sequence from a FILE or VIEW.
      !!! </summary>
      !!! <param name="entity">The label of a FILE or VIEW declaration.</param>
      PREVIOUS(FILE entity),NAME('Cla$FILE_PREV')

      !!! <summary>
      !!! Displays the Windows standard printer choice dialog box to allow the user to select or configure a printer.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the file choice dialog. If omitted, a default title is supplied by Windows.</param>
      !!! <param name="mode">A numeric constant or variable containing the mode of the dialog: 0 - Print Dialog; 1 - Printer Setup; 2 - Page Setup.</param>
      !!! <returns>Zero (0) if the user pressed the Cancel button, or one (1) if the user pressed the Ok button on the dialog.</returns>  
      PRINTERDIALOG(<STRING title>, SIGNED mode=0),BOOL,PROC,NAME('Cla$PrinterDialog')

      !!! <summary>
      !!! Displays the Windows standard printer choice dialog box to allow the user to select or configure a printer.
      !!! </summary>
      !!! <param name="title">A string constant or variable containing the title to place on the file choice dialog. If omitted, a default title is supplied by Windows.</param>
      !!! <param name="mode">A numeric constant or variable containing the mode of the dialog: 0 - Print Dialog; 1 - Printer Setup; 2 - Page Setup.</param>
      !!! <param name="prn">A label or reference to opened REPORT or built-in variable PRINTER (default).</param>
      !!! <returns>Zero (0) if the user pressed the Cancel button, or one (1) if the user pressed the Ok button on the dialog.</returns>  
      PRINTERDIALOGA(<STRING title>, SIGNED mode=0, <*REPORT prn>),BOOL,PROC,NAME('Cla$PrinterDialogA')

      !!! <summary>
      !!! Creates a new scope for subsequent BIND statements.
      !!! </summary>
      !!! <param name="clearFlag">An integer constant or variable containing either zero (0) or one (1). When zero, the BIND statement's name space is cleared of all variables and procedures previously bound. When one, all variables and procedures previously bound are left in place. If omitted, the clearflag is zero.</param>
      PUSHBIND(SIGNED clearFlag=0),NAME('Cla$PushBind')

      !!! <summary>
      !!! Writes the last error information to an internal memory stack.
      !!! </summary>
      PUSHERRORS(),NAME('Cla$pusherrcodes')

      !!! <summary>
      !!! Writes a string value into the system registry.
      !!! </summary>
      !!! <param name="root">A LONG integer, variable or expression that contains the root section of the registry from which to write the value. Valid values for this are defined in equates.clw </param>
      !!! <param name="keyName">A STRING constant, variable or expression that contains the key name of the key whose value is to be written. This may contain a path separated by backslash ‘\’ characters.</param>
      !!! <param name="valueName">A STRING constant, variable or expression that contains the name of the value to be written. </param>
      !!! <param name="value">A STRING constant, variable or expression that contains the value to be written to the registry in the position given. If omitted, an empty string is written to the registry.</param> 
      !!! <param name="valueType">A LONG integer constant, variable or expression that specifies the how to store passed value. If omitted, the default value is REG_SZ </param>
      !!! <returns>If the write to the registry is successful, PUTREG returns 0. If the write to the registry is unsuccessful, the result is a non-zero value.</returns>      
      PUTREG(LONG root, STRING keyName, STRING valueName, <STRING value>, LONG valueType=REG_SZ),LONG,PROC,NAME('Cla$PUTREG')

      !!! <summary>
      !!! Sets the value for an INI file entry.
      !!! </summary>
      !!! <param name="INISection">A string constant or variable containing the name of the portion of the INI file which contains the entry.</param>
      !!! <param name="entry">A string constant or variable containing the name of the specific setting  to set.</param>
      !!! <param name="value">A string constant or variable containing the setting to place in the entry (up to 1023 characters). An empty string ('') leaves the entry empty. If omitted, the entry is deleted.</param>
      !!! <param name="fileName">A string constant or variable containing the name of the INI file to search. A file name without a full path (e.g. 'Autolog.ini') looks for the file in the Windows directory. To look for the file in the current directory enter a dot path (e.g. '.\Autolog.ini'). If the file does not exist, GETINI returns an empty string. If omitted, GETINI searches the WIN.INI file.</param>
      PUTINI(STRING INIsection, <STRING entry>, <STRING value>, <STRING fileName>),NAME('Cla$PUTINI')

      !!! <summary>
      !!! Expands (or unpacks) string data
      !!! </summary>
      !!! <param name="str">A string constant or variable containing the properties to parse.</param>
      !!! <param name="flag">An unsigned integer that controls the method of unpacking.</param>
      !!! <returns>The string contained in the symbol with all single quotes ('), un-paired left angle brackets("lt;"), and un-paired left curly braces ({) "doubled up" to prevent compiler errors.</returns>  
      QUOTE(STRING str, UNSIGNED flag=0),STRING,NAME('Cla$QUOTE')

      !!! <summary>
      !!! Returns a random integer.
      !!! </summary>
      !!! <param name="low">A numeric constant, variable, or expression for the lower boundary of the range.</param>
      !!! <param name="high">A numeric constant, variable, or expression for the upper boundary of the range</param>
      !!! <returns>A random LONG integer between the low and high values, inclusively</returns>  
      RANDOM(LONG low, LONG high),LONG,NAME('Cla$RANDOM')

      !!! <summary>
      !!! Registers an event handling procedure.
      !!! </summary>
      !!! <param name="event">An integer constant, variable, expression, or EQUATE containing an event number. A value in the range 400h to 0FFFh is a User-defined event.</param>
      !!! <param name="handler">A LONG variable, or expression containing the return value from ADDRESS for the PROCEDURE to handle the event. </param>
      !!! <param name="object">A LONG integer constant, variable, or expression containing any 32-bit unique value to identify the specific handler. This is generally the return value of ADDRESS(SELF) when the handler is a CLASS method.</param>
      !!! <param name="structure">The label of the WINDOW or REPORT whose event to handle. If omitted, the current target WINDOW or REPORT is assumed.</param> 
      !!! <param name="feq">An integer constant, EQUATE, variable, or expression containing the field number of the specific control whose event to handle. If omitted, the event is handled for every control on the window.</param>
      REGISTER(SIGNED event, POINTER_T handler, POINTER_T object, <WINDOW structure>, <SIGNED feq>),NAME('Cla$REGISTEREVENT')

      !!! <summary>
      !!! Registers an event handling procedure.
      !!! </summary>
      !!! <param name="event">An integer constant, variable, expression, or EQUATE containing an event number. A value in the range 400h to 0FFFh is a User-defined event.</param>
      !!! <param name="handler">A LONG variable, or expression containing the return value from ADDRESS for the PROCEDURE to handle the event. </param>
      !!! <param name="object">A LONG integer constant, variable, or expression containing any 32-bit unique value to identify the specific handler. This is generally the return value of ADDRESS(SELF) when the handler is a CLASS method.</param>
      !!! <param name="structure">The label of the WINDOW or REPORT whose event to handle. If omitted, the current target WINDOW or REPORT is assumed.</param> 
      !!! <param name="feq">An integer constant, EQUATE, variable, or expression containing the field number of the specific control whose event to handle. If omitted, the event is handled for every control on the window.</param>
      REGISTEREVENT(SIGNED event, POINTER_T handler, POINTER_T object, <WINDOW structure>, <SIGNED feq>),NAME('Cla$REGISTEREVENT')

      !!! <summary>
      !!! Releases the held record(s).
      !!! </summary>
      !!! <param name="entity">The label of a FILE or VIEW declaration.</param>
      RELEASE(FILE entity),NAME('Cla$FILE_RELEASE')

      !!! <summary>
      !!! Deletes a FILE.
      !!! </summary>
      !!! <param name="fileLabel">The label of the FILE to be removed.</param>
      REMOVE(FILE fileLabel),NAME('Cla$FILE_REMOVE')

      !!! <summary>
      !!! Deletes a file.
      !!! </summary>
      !!! <param name="fileName">A string constant or variable containing the filename of the file to erase. Wildcards are allowed.</param>
      REMOVE(STRING fileName),NAME('Cla$RemoveFile')

      !!! <summary>
      !!! Deletes files and/or directories.
      !!! </summary>
      !!! <param name="fileName">A string constant or variable containing the filename of the file to erase. Wildcards are allowed.</param>
      !!! <param name="options">A set of flags to control removing files.</param>
      REMOVE(STRING fileName, LONG options),NAME('Cla$RemoveFileA')

      !!! <summary>
      !!! Renames a FILE.
      !!! </summary>
      !!! <param name="fileLabel">The label of the FILE to be renamed.</param>
      !!! <param name="newname">A string constant or variable containing a file specification. If the file specification does not contain a drive and path, the current drive and directory are assumed. If only the path is specified, the filename and extension of the original file are used for the new file. Files cannot be renamed to a new drive.</param>
      RENAME(FILE fileLabel, STRING newName),NAME('Cla$FILE_RENAME')
      
      !!! <summary>
      !!! Renames a file.
      !!! </summary>
      !!! <param name="fileLabel">A string constant or variable containing the filename of the file to rename.</param>
      !!! <param name="newname">A string constant or variable containing a file specification. If the file specification does not contain a drive and path, the current drive and directory are assumed. If only the path is specified, the filename and extension of the original file are used for the new file. Files cannot be renamed to a new drive.</param>
      RENAME(STRING fileName, STRING newName),NAME('Cla$RenameFile')

      !!! <summary>
      !!! Resume thread execution.
      !!! </summary>
      !!! <param name="threadno">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      RESUME(SIGNED threadno),NAME('Cla$RESUME')

      !!! <summary>
      !!! Right justifies a string.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression.</param>
      !!! <param name="length">A numeric constant, variable, or expression for the length of the return string. If omitted, length defaults to the length of the string.</param>
      !!! <returns>A right justified string</returns>
      RIGHT(STRING str, UNSIGNED length=-1),STRING,NAME('Cla$StackRIGHT')

      !!! <summary>
      !!! Terminates an active transaction
      !!! </summary>
      ROLLBACK(),NAME('Cla$ROLLBACK')

      !!! <summary>
      !!! Draws a rectangular box with rounded corners on the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point.</param>
      !!! <param name="width">An integer expression that specifies the width.</param>      
      !!! <param name="height">An integer expression that specifies the height</param>
      !!! <param name="fill">A LONG integer constant, constant EQUATE, or variable containing the red, green, and blue components that create the color in the three low-order bytes (bytes 0, 1, and 2) or an EQUATE for a standard Windows color value.</param>
      !!! <param name="attributeList">A string constant, variable, or EQUATE containing an optional type of output document and its associated attributes. Only valid when the target is a REPORT.</param>
      ROUNDBOX(SIGNED x, SIGNED y, SIGNED width, SIGNED height, LONG fill=COLOR:None, <STRING attributeList>),NAME('Cla$ROUNDBOX')

      !!! <summary>
      !!! Executes a command as if it were entered on the O/S command line.
      !!! </summary>
      !!! <param name="command">A string constant or variable containing the command to execute. This may include a full path and command line parameters.</param>
      !!! <param name="waitFlag">An integer constant, variable, or EQUATE indicating whether RUN should launch the command and wait for its termination, or immediately return after launching. If omitted or zero (0), control immediately returns to the statement following the RUN. If one (1), control returns to the statement following the RUN only after the command has completed its execution.</param>
      RUN(STRING command, SIGNED waitFlag=0,SIGNED=0),NAME('Cla$RUN')

      !!! <summary>
      !!! Return program exit code
      !!! </summary>
      !!! <returns>A LONG value representing the exit code passed to the operating system from the command executed by the RUN statement.</returns>  
      RUNCODE(),LONG,NAME('Cla$RUNCODE')

      !!! <summary>
      !!! Return reject code number
      !!! </summary>
      !!! <returns>A SIGNED value representing the code number for the reason any EVENT:Rejected that was posted.</returns>  
      REJECTCODE(),SIGNED,NAME('Cla$REJECTCODE')

      !!! <summary>
      !!! Sets the next control to receive input focus.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label of the next control to process. If omitted, the SELECT statement initiates AcceptAll mode.</param>
      !!! <param name="pos">Specifies a position within the control to place the cursor. For an ENTRY or TEXT, SPIN, or COMBO control this is a character position, or a beginning character position for a marked block. For an OPTION structure, this is the selection number within the OPTION. For a LIST control, this is the QUEUE entry number. This parameter can also be specified using property syntax by PROP:Selected or PROP:SelStart.</param>
      !!! <param name="endpos">Specifies an ending character position within an ENTRY, TEXT, SPIN, or COMBO control. The character position specified by position and endposition are marked as a block, available for cut and paste operations. This parameter can also be specified using property syntax by PROP:SelEnd.</param>
      SELECT(SIGNED feq=0, SIGNED pos=0, SIGNED endPos=0),NAME('Cla$SELECT')

      !!! <summary>
      !!! Return control that has received focus
      !!! </summary>
      !!! <returns>A SIGNED value representing the field number of the control receiving input focus when an EVENT:Selected event occurs.</returns>  
      SELECTED(),SIGNED,NAME('Cla$SELECTED')

      !!! <summary>
      !!! Sends a message to the file driver.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration. The FILE's DRIVER attribute identifies the file driver to receive the message.</param>
      !!! <param name="message">A string constant or variable containing the information to supply to the file driver.</param>
      !!! <returns>A string that returns a file driver dependant message.</returns>
      SEND(FILE fileLabel, STRING message),STRING,PROC,NAME('Cla$FILE_SEND')

      !!! <summary>
      !!! Toggles three-dimensional look and feel.
      !!! </summary>
      !!! <param name="switchOn">An integer constant switching the 3D look off (0) and on (1).</param>
      SET3DLOOK(UNSIGNED switchOn),NAME('Cla$SET3DLOOK')

      !!! <summary>
      !!! Puts information in the Windows clipboard.
      !!! </summary>
      !!! <param name="str">A string constant or variable containing the information to place in the Windows clipboard.This should not contain any embedded null characters (ASCII 0). This is placed in the CF_TEXT format, only.</param>
      SETCLIPBOARD(STRING str),NAME('Cla$SETCLIPBOARD')

      !!! <summary>
      !!! Sets the system clock.
      !!! </summary>
      !!! <param name="stdTime">A numeric constant, variable, or expression for a standard time (expressed as hundredths of a second since midnight, plus one).</param>
      SETCLOCK(LONG stdTime),NAME('Cla$SETCLOCK')

      !!! <summary>
      !!! Internally sets command line parameters.
      !!! </summary>
      !!! <param name="commandLine">A string constant, variable, or expression containing the new command line parameters.</param>
      SETCOMMAND(STRING commandLine),NAME('Cla$SETCOMMAND')

      !!! <summary>
      !!! Specifies a temporary mouse cursor to display.
      !!! </summary>
      !!! <param name="cursorName">An EQUATE naming a Windows-standard mouse cursor, or a string constant naming a cursor resource linked into the project--the name of a .CUR file with a leading tilde ('~Mycur.CUR'). If omitted, turns off the temporary cursor.</param>
      SETCURSOR(<STRING cursorName>),NAME('Cla$SETCURSOR')

      !!! <summary>
      !!! Sets the DROPID procedure's return value.
      !!! </summary>
      !!! <param name="str">A string constant or variable containing the value the DROPID procedure will return.</param>
      SETDROPID(STRING str),NAME('Cla$SETDROPID')

      !!! <summary>
      !!! Sets the exit code returned on process termination
      !!! </summary>
      !!! <param name='status')An exit code
      SETEXITCODE(SIGNED status),NAME('Cla$SETEXITCODE')

      !!! <summary>
      !!! Sets display font information.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control to affect. If control is zero (0), it specifies the WINDOW.</param>
      !!! <param name="typeface">A string constant or variable containing the name of the font. If omitted, the system font is used.</param>
      !!! <param name="size">An integer constant or variable containing the size (in points) of the font. If omitted, the system default font size is used.</param>      
      !!! <param name="color">A LONG integer constant or variable containing the red, green, and blue values for the color of the font in the low-order three bytes, or an EQUATE for a standard Windows color value. If omitted, black is used.</param>
      !!! <param name="style">An integer constant, constant expression, EQUATE, or variable specifying the strike weight and style of the font. If omitted, the weight is normal.</param>
      !!! <param name="charset">A LONG integer variable specifying the character set value.</param>
      SETFONT(SIGNED feq, <STRING typeface>, SIGNED size=0, LONG color=COLOR:None, UNSIGNED style=-1, BYTE charset=CHARSET:Default),NAME('Cla$SETFONT')

      !!! <summary>
      !!! Sets display font information.
      !!! </summary>
      !!! <param name="wnd">Label of a WINDOW, APPLICATION or REPORT structure, or a value of the reference variable of the type &WINDOW, or a procedure parameter of the type WINDOW, or a built-in TARGET variable.</param>
      !!! <param name="typeface">A string constant or variable containing the name of the font. If omitted, the system font is used.</param>
      !!! <param name="size">An integer constant or variable containing the size (in points) of the font. If omitted, the system default font size is used.</param>      
      !!! <param name="color">A LONG integer constant or variable containing the red, green, and blue values for the color of the font in the low-order three bytes, or an EQUATE for a standard Windows color value. If omitted, black is used.</param>
      !!! <param name="style">An integer constant, constant expression, EQUATE, or variable specifying the strike weight and style of the font. If omitted, the weight is normal.</param>
      !!! <param name="charset">A LONG integer variable specifying the character set value.</param>
      SETFONT(WINDOW wnd, <STRING typeface>, SIGNED size=0, LONG color=COLOR:None, UNSIGNED style=-1, BYTE charset=CHARSET:Default),NAME('Cla$SETFONTEX')

      !!! <summary>
      !!! Sets the ASCII character returned by the KEYCHAR procedure.
      !!! </summary>
      !!! <param name="keyChar">An integer constant, variable, or expression containing the ASCII value of the character to set.</param>
      SETKEYCHAR(UNSIGNED keyChar),NAME('Cla$SETKEYCHAR')

      !!! <summary>
      !!! Sets the keycode returned by the KEYCODE procedure.
      !!! </summary>
      !!! <param name="keyCode">An integer constant or keycode EQUATE label.</param>
      SETKEYCODE(UNSIGNED keyCode),NAME('Cla$SETKEYCODE')

      !!! <summary>
      !!! Dynamically sets the window or report orientation.
      !!! </summary>
      !!! <param name="structure">Label of a WINDOW, APPLICATION or REPORT structure, or a value of the reference variable of the type WINDOW, or a procedure parameter of a WINDOW reference, or a built-in TARGET variable.</param>
      !!! <param name="layout">A BYTE value or variable that specifies the window orientation. Zero (0) sets a left-to-right layout and one (1) sets a right-to-left layout.</param>
      SETLAYOUT(WINDOW structure, BYTE layout),NAME('Cla$SETLAYOUT')

      !!! <summary>
      !!! Changes the current drive and directory.
      !!! </summary>
      !!! <param name="path">A string constant or the label of a STRING, CSTRING, or PSTRING variable containing a new drive and/or directory specification.</param>
      SETPATH(STRING path),NAME('Cla$SETPATH')

      !!! <summary>
      !!! Sets the current pen width.
      !!! </summary>
      !!! <param name="width">An integer expression that specifies the pen's thickness, measured in dialog units (unless overridden by the THOUS, MM, or POINTS attribute on a REPORT). If omitted, the default (one pixel) is set.</param>
      SETPENWIDTH(SIGNED width=0),NAME('Cla$SETPENWIDTH')

      !!! <summary>
      !!! Sets the current pen style.
      !!! </summary>
      !!! <param name="style">An integer constant, constant EQUATE, or variable that specifies the pen's style. If omitted, a solid line is set.</param>
      SETPENSTYLE(SIGNED style=PEN:Solid),NAME('Cla$SETPENSTYLE')

      !!! <summary>
      !!! Sets the current pen color.
      !!! </summary>
      !!! <param name="color">A LONG integer constant, constant EQUATE, or variable containing the red, green, and blue components that create the color in the three low-order bytes (bytes 0, 1, and 2) or an EQUATE for a standard Windows color value. If omitted, the Windows system color for window text is set.</param>
      SETPENCOLOR(LONG color=COLOR:WindowText),NAME('Cla$SETPENCOLOR')

      !!! <summary>
      !!! Dynamically specifies the position and size of an APPLICATION, WINDOW, REPORT, or control.
      !!! </summary>
      !!! <param name="feq">A field number or field equate label for the control to affect. If control is zero (0), it specifies the window.</param>
      !!! <param name="x">An integer constant, expression, or variable that specifies the horizontal position of the top left corner. If omitted, the x position is not changed.</param>
      !!! <param name="y">An integer constant, expression, or variable that specifies the vertical position of the top left corner. If omitted, the y position is not changed.</param>
      !!! <param name="width">An integer constant, expression, or variable that specifies the width. If omitted, the width is not changed.</param>
      !!! <param name="height">An integer constant, expression, or variable that specifies the height. If omitted, the height is not changed.</param>
      SETPOSITION(SIGNED feq, SIGNED x=_nopos, SIGNED y=_nopos, SIGNED width=_nopos, SIGNED height=_nopos),NAME('Cla$SETPOSITION')

      !!! <summary>
      !!! Dynamically specifies the position and size of an APPLICATION, WINDOW, or REPORT.
      !!! </summary>
      !!! <param name="target">The label of an APPLICATION, WINDOW, or REPORT structure, or a reference to any of those structures.</param>
      !!! <param name="x">An integer constant, expression, or variable that specifies the horizontal position of the top left corner. If omitted, the x position is not changed.</param>
      !!! <param name="y">An integer constant, expression, or variable that specifies the vertical position of the top left corner. If omitted, the y position is not changed.</param>
      !!! <param name="width">An integer constant, expression, or variable that specifies the width. If omitted, the width is not changed.</param>
      !!! <param name="height">An integer constant, expression, or variable that specifies the height. If omitted, the height is not changed.</param>
      SETPOSITION(<WINDOW target>, SIGNED x=_nopos, SIGNED y=_nopos, SIGNED width=_nopos, SIGNED height=_nopos),NAME('Cla$SETPOSITIONw')

      !!! <summary>
      !!! Sets the current window (or report) for drawing graphics and other window-interaction statements.
      !!! </summary>
      !!! <param name="target">The label of an APPLICATION, WINDOW, or REPORT structure, or a reference to any of those structures. The execution thread is always deduced from the target and any specified thread parmeter is ignored. If omitted, the last window opened and not yet closed in the specified thread is used.</param>
      !!! <param name="band">The control number or field equate label of the REPORT band (or IMAGE control in a window target) to draw graphics primitives to (ARC, CHORD, etc.).</param>
      SETTARGET(<WINDOW target>, UNSIGNED band=0),NAME('Cla$SETTARGET')

      !!! <summary>
      !!! Sets the O/S system date.
      !!! </summary>
      !!! <param name="stdDate">A numeric constant, variable, or expression for a standard date.</param>
      SETTODAY(LONG stdDate),NAME('Cla$SETTODAY')

      !!! <summary>
      !!! Opens a FILE structure for processing.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      !!! <param name="accessMode">A numeric constant, variable, or expression which determines the level of access granted to both the user opening the file, and other users in a multi-user system. If omitted, the default value is 42h (Read/Write, Deny None).</param>
      SHARE(FILE fileLabel, SIGNED accessMode=42H),NAME('Cla$FILE_SHARE')

      !!! <summary>
      !!! Returns the fully-qualified short filename for a given long filename.
      !!! </summary>
      !!! <param name="longFileName">A string constant, variable, or expression that specifies the long filename to convert. This may include the complete path. If omitted, SHORTPATH returns the current drive and directory in short name form.</param>
      !!! <returns>A string representing the DOS standard short filename for a given longfilename</returns>        
      SHORTPATH(<STRING longFileName>),STRING,NAME('Cla$SHORTPATH')

      !!! <summary>
      !!! Writes a string to the current window or report.
      !!! </summary>
      !!! <param name="x">An integer expression that specifies the horizontal position of the starting point, in dialog units.</param>
      !!! <param name="y">An integer expression that specifies the vertical position of the starting point, in dialog units.</param>
      !!! <param name="str">A string constant, variable, or expression containing the formatted text to place on the current window or report.</param>
      SHOW(SIGNED x, SIGNED y, STRING str, <STRING>),NAME('Cla$SHOW')

      !!! <summary>
      !!! Arms a procedure which is called when the program terminates.
      !!! </summary>
      !!! <param name="procName">The label of a PROCEDURE. If omitted, the SHUTDOWN process is disarmed.</param>
      SHUTDOWN(<_PROC procName>),NAME('Cla$SHUTDOWN')

      !!! <summary>
      !!! Returns the sine.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable or expression for the angle in radians. p is a constant which represents the ratio of the circumference and radius of a circle. There are 2p radians (or 360 degrees) in a circle.</param>
      !!! <returns>The trigonometric sine of an angle measured in radians.</returns>      
      SIN(REAL exp),REAL,NAME('Cla$SIN')

      !!! <summary>
      !!! Bypasses records during sequential processing.
      !!! </summary>
      !!! <param name="entity">The label of a FILE or VIEW declaration.</param>
      !!! <param name="count">A numeric constant or variable. The count specifies the number of records to bypass. If the value is positive, records are skipped in forward (NEXT) sequence; if count is negative, records are skipped in reverse (PREVIOUS) sequence.</param>
      SKIP(FILE entity, COUNT_T count),NAME('Cla$FILE_SKIP')

      !!! <summary>
      !!! Returns square root.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable, or expression. If the value of the expression is less than zero, the return value is zero.</param>
      !!! <returns>The square root of the expression</returns>      
      SQRT(REAL exp),REAL,NAME('Cla$SQRT')
      
      !!! <summary>
      !!! Begins a new execution thread.
      !!! </summary>
      !!! <param name="procName">The label of the first PROCEDURE to call on the new execution thread. This may not be an overloaded procedure.</param>
      !!! <param name="stack">An integer constant or variable containing the size of the stack to allocate to the new execution thread. If omitted, the default stack is 20,000 bytes.</param>
      !!! <returns>The signed number assigned to the new thread.</returns>        
      START(_PROC procName, UNSIGNED stack=0),SIGNED,PROC,NAME('Cla$START')

      !!! <summary>
      !!! Begins a new execution thread.
      !!! </summary>
      !!! <param name="procName">The label of the first PROCEDURE to call on the new execution thread. This may not be an overloaded procedure.</param>
      !!! <param name="stack">An integer constant or variable containing the size of the stack to allocate to the new execution thread. If omitted, the default stack is 20,000 bytes.</param>
      !!! <param name="passedValue">A string constant, variable, or expression containing the value to pass as a parameter to the procedure. There may be up to three passed values listed.</param>
      !!! <returns>The signed number assigned to the new thread.</returns>        
      START(_PROC1 procName, UNSIGNED stack=0, STRING passedValue),SIGNED,PROC,NAME('Cla$START1')

      !!! <summary>
      !!! Begins a new execution thread.
      !!! </summary>
      !!! <param name="procName">The label of the first PROCEDURE to call on the new execution thread. This may not be an overloaded procedure.</param>
      !!! <param name="stack">An integer constant or variable containing the size of the stack to allocate to the new execution thread. If omitted, the default stack is 20,000 bytes.</param>
      !!! <param name="passedValue">A string constant, variable, or expression containing the value to pass as a parameter to the procedure. There may be up to three passed values listed.</param>
      !!! <returns>The signed number assigned to the new thread.</returns>        
      START(_PROC2 procName, UNSIGNED stack=0, STRING, STRING),SIGNED,PROC,NAME('Cla$START2')

      !!! <summary>
      !!! Begins a new execution thread.
      !!! </summary>
      !!! <param name="procName">The label of the first PROCEDURE to call on the new execution thread. This may not be an overloaded procedure.</param>
      !!! <param name="stack">An integer constant or variable containing the size of the stack to allocate to the new execution thread. If omitted, the default stack is 20,000 bytes.</param>
      !!! <param name="passedValue">A string constant, variable, or expression containing the value to pass as a parameter to the procedure. There may be up to three passed values listed.</param>
      !!! <returns>The signed number assigned to the new thread.</returns>        
      START(_PROC3 procName, UNSIGNED stack=0, STRING, STRING, STRING),SIGNED,PROC,NAME('Cla$START3')

      !!! <summary>
      !!! Returns the passed file, window or report status.
      !!! </summary>
      !!! <param name="structure">The label of a FILE, WINDOW, APPLICATION, or REPORT structure, or valid built-in variables TARGET, PRINTER, or SYSTEM.</param>
      !!! <returns>Returns zero (0) if structure is not open, and the file's access mode or 1 or 2 if it is open. </returns>      
      STATUS(WINDOW structure),LONG,NAME('Cla$STATUSwindow')

      !!! <summary>
      !!! Suspends program execution and displays a message window.
      !!! </summary>
      !!! <param name="message">An optional string expression (up to 64K) which displays in the error window.</param>
      STOP(<STRING message>),NAME('Cla$STOP')

      !!! <summary>
      !!! Disables automatic FILE flushing.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE.</param>
      STREAM(FILE fileLabel),NAME('Cla$FILE_STREAM')

      !!! <summary>
      !!! Returns the starting position of a substring based on all parameters passed.
      !!! </summary>
      !!! <param name="first">A string containing data to compare against the second parameter.</param>
      !!! <param name="second">A string containing a regular expression to match the first parameter.</param>
      !!! <param name="nocase">An integer constant or equate which specifies if the comparison method is case sensitive. If FALSE (0), the method is case sensitive. If TRUE (1), the method is not case sensitive. The default value is case sensitive.</param>
      !!! <returns>A LONG value which represents the starting position where the first and second parameters match according to the comparison mode specified.</returns>        
      STRPOS(STRING first, STRING second, BYTE nocase=FALSE),LONG,NAME('Cla$REGULAR')

      !!! <summary>
      !!! Returns a portion of a string.
      !!! </summary>
      !!! <param name="str">A string constant, variable or expression.</param>
      !!! <param name="pos">A integer constant, variable, or expression. If positive, it points to a character position relative to the beginning of the string. If negative, it points to the character position relative to the end of the string (i.e., a position value of -3 points to a position 3 characters from the end of the string). If position is omitted or zero, the default in Clarion Win32 is 1. In Clarion#, position uses a zero-based index. A value of zero (0) points to character position 1, 1 points to 2, etc.</param>
      !!! <param name="length">A numeric constant, variable, or expression of number of characters to return.</param>
      !!! <returns>A portion of a string</returns>        
      SUB(STRING str, UNSIGNED pos, UNSIGNED length),STRING,NAME('Cla$StackSUB')

      !!! <summary>
      !!! Suspend thread execution.
      !!! </summary>
      !!! <param name="threadno">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      !!! <param name="quietMode">A numeric constant, variable, or expression that can be evaluated as a SIGNED integer.</param>
      !!! <returns>TRUE (1) if a thread has been suspended and returns FALSE (0) if the thread could not be suspended.</returns>        
      SUSPEND(SIGNED threadno, SIGNED quietMode=1),SIGNED,PROC,NAME('Cla$SUSPEND')

      !!! <summary>
      !!! Returns the tangent.
      !!! </summary>
      !!! <param name="exp">A numeric constant, variable or expression for the angle in radians. p is a constant which represents the ratio of the circumference and radius of a circle. There are 2p radians (or 360 degrees) in a circle.</param>
      !!! <returns>The trigonometric tangent of an angle measured in radians.</returns>      
      TAN(REAL exp),REAL,NAME('Cla$TAN')

      !!! <summary>
      !!! Return current execution thread
      !!! </summary>
      !!! <returns>A SIGNED number representing the currently executing thread number.</returns>  
      THREAD(),SIGNED, NAME('Cla$THREAD')

      !!! <summary>
      !!! Returns current execution thread locked state
      !!! </summary>
      !!! <returns>A SIGNED number representing current execution thread's locked/unlocked state.</returns>  
      THREADLOCKED(),SIGNED,NAME('Cla$THREADLOCKED')

      !!! <summary>
      !!! Associates a string value to an ASTRING.
      !!! </summary>
      !!! <param name="stringToken">A string constant or the label of a previously declared ASTRING token.</param>
      !!! <param name="index">A numeric constant or variable defining the index value of an ASTRING. The index value may be any LONG value greater than zero.</param>
      !!! <param name="value">A numeric constant or variable. The value may be any LONG value greater than zero.</param>
      !!! <returns>If the index is allocated at runtime, TIE will return the generated index number.</returns>        
      TIE(ASTRING stringToken, LONG index=0, LONG value),LONG,PROC,NAME('Cla$TIE')

      !!! <summary>
      !!! Retrieves a string value that was previously associated to an ASTRING token.
      !!! </summary>
      !!! <param name="stringToken">A string constant or the label of a previously declared ASTRING token.</param>
      !!! <param name="index">A numeric constant or variable defining the index value of an ASTRING. The index value may be any LONG value greater than zero.</param>
      !!! <returns>The value that was previously associated to a particular ASTRING stringToken and index. If a value is not TIED then 0 will be returned.</returns>        
      TIED(ASTRING stringToken, LONG index),LONG,NAME('Cla$TIED')

      !!! <summary>
      !!! Returns today's date
      !!! </summary>
      !!! <returns>The operating system date as a standard LONG date</returns>  
      TODAY(),LONG,NAME('Cla$TODAY')
      
      !!! <summary>
      !!! Writes a string to the current window or report.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression.</param>
      TYPE(STRING str),NAME('Cla$StackTYPE')
      
      !!! <summary>
      !!! Resumes redrawing of the control in the current thread's top window
      !!! suspended by the call to FREEZE
      !!! </summary>
      !!! <param name="ctl">The control to unfreeze
      !!! <param name="state")The drawing status returned by corresponding call to FREEZE
      UNFREEZE(SIGNED ctl, LONG state),NAME('Cla$UNFREEZEcontrol')

      !!! <summary>
      !!! Displays previously hidden controls.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      UNHIDE(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$UNHIDE')

      !!! <summary>
      !!! Unloads a Windows standard .DLL previously loaded by CALL.
      !!! </summary>
      !!! <param name="fileName">A string constant, variable, or expression containing the name (including extension) of the .DLL to unload. This may include a full path.</param>
      UNLOAD(STRING fileName),NAME('Cla$UNLOAD')

      !!! <summary>
      !!! Unlocks a previously locked data file.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE declaration.</param>
      UNLOCK(FILE fileLabel),NAME('Cla$FILE_UNLOCK')

      !!! <summary>
      !!! Unlock the current execution thread
      !!! </summary>
      UNLOCKTHREAD(),NAME('Cla$UNLOCKTHREAD')

      !!! <summary>
      !!! Contracts the value's string data, "singling up" doubled single quotes (''), and all doubled un-paired left angle brackets and left curly braces ({{).
      !!! </summary>
      !!! <param name="symbol">A string constant or variable containing the properties to parse.</param>
      !!! <returns>The string contained in the value with all doubled single quotes (''), doubled un-paired left angle brackets, and un-paired left curly braces ({{) "singled up" (returned to single instances instead of double instances of each character).</returns>  
      UNQUOTE(STRING symbol),STRING,NAME('Cla$UNQUOTE')

      !!! <summary>
      !!! Unregisters an event handling procedure.
      !!! </summary>
      !!! <param name="event">An integer constant, variable, expression, or EQUATE containing an event number. A value in the range 400h to 0FFFh is a User-defined event.</param>
      !!! <param name="handler">A LONG variable, or expression containing the return value from ADDRESS for the PROCEDURE to handle the event. </param>
      !!! <param name="object">A LONG integer constant, variable, or expression containing any 32-bit unique value to identify the specific handler. This is generally the return value of ADDRESS(SELF) when the handler is a CLASS method.</param>
      !!! <param name="structure">The label of the WINDOW or REPORT whose event to handle. If omitted, the current target WINDOW or REPORT is assumed.</param> 
      !!! <param name="feq">An integer constant, EQUATE, variable, or expression containing the field number of the specific control whose event to handle. If omitted, the event is handled for every control on the window.</param>
      UNREGISTER(SIGNED event=0, POINTER_T handler=0, <POINTER_T object>, <WINDOW structure>, <SIGNED feq>),NAME('Cla$UNREGISTEREVENT')

      !!! <summary>
      !!! Unregisters an event handling procedure.
      !!! </summary>
      !!! <param name="event">An integer constant, variable, expression, or EQUATE containing an event number. A value in the range 400h to 0FFFh is a User-defined event.</param>
      !!! <param name="handler">A LONG variable, or expression containing the return value from ADDRESS for the PROCEDURE to handle the event. </param>
      !!! <param name="object">A LONG integer constant, variable, or expression containing any 32-bit unique value to identify the specific handler. This is generally the return value of ADDRESS(SELF) when the handler is a CLASS method.</param>
      !!! <param name="structure">The label of the WINDOW or REPORT whose event to handle. If omitted, the current target WINDOW or REPORT is assumed.</param> 
      !!! <param name="feq">An integer constant, EQUATE, variable, or expression containing the field number of the specific control whose event to handle. If omitted, the event is handled for every control on the window.</param>
      UNREGISTEREVENT(SIGNED event=0, POINTER_T handler=0, <POINTER_T object>, <WINDOW structure>, <SIGNED feq>),NAME('Cla$UNREGISTEREVENT')

      !!! <summary>
      !!! Removes a value previously TIED for an ASTRING.
      !!! </summary>
      !!! <param name="stringToken">A string constant or the label of a previously declared ASTRING token.If omitted, all previously declared ASTRINGs are used.</param>
      !!! <param name="index">A numeric constant or variable defining the index value of an ASTRING. The index value may be any LONG value greater than zero.</param>
      UNTIE(<ASTRING stringToken>, LONG index=-1),NAME('Cla$UNTIE')

      !!! <summary>
      !!! Writes the contents of a control to its USE variable.
      !!! </summary>
      !!! <param name="firstfeq">Field number or field equate label of a control, or the first control in a range of controls. If omitted, defaults to zero (0).</param>
      !!! <param name="lastfeq">Field number or field equate label of the last control in a range of controls.</param>
      UPDATE(SIGNED firstfeq=0, SIGNED lastfeq=0),NAME('Cla$UPDATE')

      !!! <summary>
      !!! Converts a string to all upper case.
      !!! </summary>
      !!! <param name="str">A string constant, variable, or expression for the string to be converted.</param>
      !!! <returns>A string with all letters converted to upper case</returns>  
      UPPER(STRING str),STRING,NAME('Cla$StackUPPER')

      !!! <summary>
      !!! Arms automatic optimistic concurrency checking.
      !!! </summary>
      !!! <param name="fileLabel">The label of a FILE or VIEW declaration.</param>
      WATCH(FILE fileLabel),NAME('Cla$FILE_WATCH')

      !!! <summary>
      !!! Returns a specified field from a group structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, RECORD, CLASS, or QUEUE declaration.</param>
      !!! <param name="number">An integer expression specifying the ordinal position of a field in the group.</param>
      !!! <param name="dim">An optional dimension element number, if applicable.</param>
      !!! <returns>The number specified field from a group structure.</returns>        
      WHAT(*GROUP label, SIGNED number, SIGNED dim=0),*?,NAME('Cla$WHAT')

      !!! <summary>
      !!! Returns a specified field from a group structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, RECORD, CLASS, or QUEUE declaration.</param>
      !!! <param name="number">An integer expression specifying the ordinal position of a field in the group.</param>
      !!! <param name="dim">An optional dimension element number, if applicable.</param>
      !!! <returns>The number specified field from a group structure.</returns>        
      WHAT(QUEUE label, SIGNED number, SIGNED dim=0),*?,NAME('Cla$WHATqueue')

      !!! <summary>
      !!! Returns a field's ordinal position within a GROUP, RECORD, CLASS, or QUEUE structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, RECORD, CLASS, or QUEUE declaration.</param>
      !!! <param name="field">The label of a field in the group declaration.</param>
      !!! <returns>The ordinal position of a specified field in a group structure</returns>        
      WHERE(*GROUP label, *? field),SIGNED,NAME('Cla$WHERE')

      !!! <summary>
      !!! Returns a field's ordinal position within a GROUP, RECORD, CLASS, or QUEUE structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, RECORD, CLASS, or QUEUE declaration.</param>
      !!! <param name="field">The label of a field in the group declaration.</param>
      !!! <returns>The ordinal position of a specified field in a group structure</returns>        
      WHERE(QUEUE label,*? field),SIGNED,NAME('Cla$WHEREqueue')

      !!! <summary>
      !!! Returns a string containing the name of a specified field from a group structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, RECORD, CLASS, or QUEUE declaration.</param>
      !!! <param name="number">An integer expression specifying the ordinal position of a field in the group.</param>
      !!! <returns>String containing the name of the number specified field from a group structure</returns>        
      WHO(*GROUP label, SIGNED number),STRING,NAME('Cla$WHO')

      !!! <summary>
      !!! Returns a string containing the name of a specified field from a group structure.
      !!! </summary>
      !!! <param name="label">The label of a GROUP, RECORD, CLASS, or QUEUE declaration.</param>
      !!! <param name="number">An integer expression specifying the ordinal position of a field in the group.</param>
      !!! <returns>String containing the name of the number specified field from a group structure</returns>        
      WHO(QUEUE label, SIGNED number),STRING,NAME('Cla$WHOqueue')

      !!! <summary>
      !!! Returns the year.
      !!! </summary>
      !!! <param name="stdDate">A numeric constant, variable, expression, or the label of a STRING, CSTRING, or PSTRING variable declared with a date picture token. The date must be a standard date. A variable declared with a date picture token is automatically converted to a standard date intermediate value.</param>
      !!! <returns>A LONG value representing a four digit number for the year of a standard date (1801 to 9999).</returns>  
      YEAR(LONG stdDate),LONG,NAME('Cla$YEAR')

      !!! <summary>
      !!! Allow event processing
      !!! </summary>
      YIELD(),NAME('Cla$YIELD')
  END

