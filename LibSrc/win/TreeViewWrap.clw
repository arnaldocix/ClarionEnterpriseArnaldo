! $Header: /0192-7(XML support)/XMLSupport/src/TreeViewWrap.clw 8     10/21/03 2:21p Mal $
!****************************************************************************
!  FILE..........: TreeViewWrap.CLW
!  AUTHOR........: 
!  DESCRIPTION...: Library for XML Export/Import
!  COPYRIGHT.....: Copyright 2003 SoftVelocity Inc. All rights reserved
!  HISTORY.......: DATE       COMMENT
!                  ---------- ------------------------------------------------
!                  2003-04-16 Created by Anatoly Medyntsev
!****************************************************************************

    MEMBER

    include('cpxml.inc'), once
    include('xmlclass.inc'), once

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.construct          PROCEDURE

    CODE
    SELF._Child&=null
    SELF._Parent&=null
    SELF._Next&=null
    SELF.filePos&=NEW(StringClass)
    SELF.viewData&=null
    SELF.fileData&=null
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.destruct           PROCEDURE
tvNode  &TreeViewWrapper
tmpNode &TreeViewWrapper
    CODE
    dispose(SELF.filePos)
    tvNode&=SELF._Child
    loop while(not tvNode &= null)
       tmpNode&=tvNode  
       tvNode&=tvNode._Next
       dispose(tmpNode)       
    end

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.getCapacity PROCEDURE
    CODE
    return 0

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.getNextRow PROCEDURE
retCode     byte
    CODE
    retCode = CPXMLErr:NoError

    if self.viewData &= NULL then
        return CPXMLErr:IllegalFunctionCall
    end

    next(self.viewData)

    if errorcode() then
        retCode = CPXMLErr:EOF
    end

    return retCode

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.clearBuffer PROCEDURE
    CODE
        if not self.viewData &= NULL then
            clear(self.viewData)
        end
    return


!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.startIterate PROCEDURE
    code
    set(self.viewData)

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.setFieldValueByIndex    PROCEDURE(UNSIGNED cou,STRING value)
    code
    return CPXMLErr:NotSupported

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.setFieldValueByName     PROCEDURE(STRING field, STRING value)
    code
    return CPXMLErr:NotSupported

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.setFieldValueByXMLName  PROCEDURE(STRING field, STRING value)
    code
    return CPXMLErr:NotSupported
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

TreeViewWrapper.getCurrentValue           PROCEDURE!, string, PROTECTED,VIRTUAL 
blobIdx        UNSIGNED
memoIdx        UNSIGNED
!fld            LONG
fl             &FILE,AUTO  
sz             UNSIGNED
_              GROUP
FCB              &FILE
Num              USHORT
               END
G              GROUP
Ref              &BLOB
               END

        CODE
        IF(NOT self.Fields.fldType=CLType:MEMO)
            RETURN PARENT.getCurrentValue()
        END
        !BLOB/MEMO case
        blobIdx=self.Fields.blobMemoIdx
        memoIdx=self.Fields.memoIdx        
        fl&=self.fileData
        IF(self.Fields.fldSize=-1)
            !BLOB case
            
            _.FCB&=fl
            _.Num=blobIdx
            G=_
            
            sz=G.Ref{PROP:Size}
            IF(sz=0)
                RETURN ''
            ELSE
                RETURN G.Ref[0:sz-1]
            END
        ELSE
            !MEMO case            
            IF BAND(CLMEMO:BINARY,self.Fields.attr)
                RETURN clipNull(fl{PROP:TEXT,-memoIdx})
            ELSE
                RETURN clip(fl{PROP:TEXT,-memoIdx})
            END
        END                                        
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.setCurrentValue           PROCEDURE(string val)!, PROTECTED,VIRTUAL

            CODE
            return

! The following methods are necessary for support of hierarchical structures
! (such as VIEW for example. The result XML file will have tree-like structure).
!------------------------------------------------------------------------------
!!return the reference to the first child node
!------------------------------------------------------------------------------
TreeViewWrapper.getFirstChild       PROCEDURE!,*StructWrapper,VIRTUAL 
    CODE
    return SELF._Child
!------------------------------------------------------------------------------
!!return the reference to the parent node
!------------------------------------------------------------------------------
TreeViewWrapper.getParent           PROCEDURE!,*StructWrapper,VIRTUAL 
    CODE
    return SELF._Parent
!------------------------------------------------------------------------------
!!return the reference to the next sibling node
!------------------------------------------------------------------------------
TreeViewWrapper.getNextSibling      PROCEDURE!,*StructWrapper ,VIRTUAL
    CODE
    return SELF._Next
!------------------------------------------------------------------------------
!!store the position of current row
!------------------------------------------------------------------------------
TreeViewWrapper.updateRowPos        PROCEDURE!,VIRTUAL
    CODE
    if(SELF.fileData&=null)
       return
    end
    SELF.filePos.str(position(SELF.fileData))
!------------------------------------------------------------------------------
!!test if row position was changed since last call of updateRowPos methods
!------------------------------------------------------------------------------
TreeViewWrapper.isRowPosChanged     PROCEDURE!,BYTE,VIRTUAL
pos StringClass
    CODE
    if(SELF.fileData&=null)
       return false
    end
    pos.str(position(SELF.fileData))
    if(SELF.filePos.str()=pos.str())
       return false
    end
    return true        

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.Init PROCEDURE(*VIEW v, BYTE removePrefix = true)
retVal unsigned(CPXMLErr:NoError)
xJI    XMLJoinInfo
bRet   BYTE
    CODE    
    bRet = xJI.LoadInfo(v)
    bRet = xJI.startIterate()
    retVal=SELF.initNode(v,xJI,removePrefix)
    return retVal


!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
TreeViewWrapper.InitNode            PROCEDURE(*VIEW v,XMLJoinInfo xJI,BYTE removePrefix=true)!,BYTE
retVal  unsigned(CPXMLErr:NoError)
!field   unsigned(1)
f   &FILE
fi  XMLFileInfo
cc  byte
child   &TreeViewWrapper,auto
prev    &TreeViewWrapper,auto   
childs  SIGNED(0)
cou SIGNED(0)
record &group, auto
rc     UNSIGNED
fldN   SIGNED
fldNp  LONG 
bRet   BYTE
typ     UNSIGNED
sz      UNSIGNED
prc     UNSIGNED
coulp   LONG
memoIdx LONG
blobIdx LONG
atr     LONG
    CODE
    cc=xJI.getNextJoin()
    if(not cc)
       return CPXMLErr:InvalidArgument
    end     
    self.viewData &= v
    if not self.viewData &= NULL
        !clear
        self.kill()
        !store value
        !enumerate fields
        f&=xJI.GetFile()        
        rc=fi.init(f)
        SELF.fileData&=f
        record &= f{prop:Record}
        bRet = xJI.startFieldsIterate()
        fldN=xJI.getNextField()
        coulp=1
        loop while(fldN <> 0)            
           clear(self.Fields)
           self.Fields.use &= null 
           self.Fields.index   = coulp
           if(fldN>0)              
              !assign field's name
              self.Fields.label = self.fixPrefix(lower(who(record, fldN)), removePrefix)
              self.Fields.use &= what(record, fldN) !assign reference to the field              
           else
            !blob/memo case
            fldNp=-fldN
            rc=fi.startFieldsIterate(ITER:MEMOONLY)
            LOOP fldNp TIMES
                 rc=fi.getNextField()
                 IF rc<>0
                    RETURN CPXMLErr:InternalError
                 END
            END
            if fi.getFieldType(typ, sz, prc) <> XIErr:NoError
                break !finish the enumeration
            end                      
            rc=fi.getFieldInfo(blobIdx,memoIdx,atr)
            self.Fields.label = self.fixPrefix(lower(fi.getFieldLabel()), removePrefix)          
            self.Fields.blobMemoIdx=blobIdx
            self.Fields.memoIdx=memoIdx
            self.Fields.attr=atr
            self.Fields.fldType = typ
            self.Fields.fldSize = sz
            self.Fields.fldInfo = prc
           end
           add(self.Fields) 
           fldN=xJI.getNextField()
           coulp+=1
        end
        !creating childs
        prev&=null
        childs=xJI.getChilds()
        loop cou=1 to childs
           child&=NEW(TreeViewWrapper)
           child._Parent&=SELF
           if(cou=1)
              SELF._Child&=child
           else
              prev._Next&=child
           end              
           rc=child.InitNode(v,xJI,removePrefix)
           if(rc<>0)
              return rc
           end
           prev&=child
        end        
    else
        retVal = CPXMLErr:InvalidArgument
    end
    return retVal

!===  StringClass implementation ========
!Auxiliary class to support dynamic string
StringClass.construct procedure

  code
  self.s &= new string(1)

StringClass.destruct procedure

  code
  dispose(self.s)

StringClass.str procedure(string s)

  code
  dispose(self.s)
  self.s &= new string(len(s) )
  self.s = s
  return self.s

StringClass.str procedure

  code
  return self.s

StringClass.cat procedure(string s)

  code
  self.str(self.str() & s)
