          MEMBER

          MAP
          END

          INCLUDE ('CWSYNCHC.INC'),ONCE
          INCLUDE ('MDISync.INC'),ONCE

GlobalMDISyncroLook CriticalSection,PRIVATE

MDISynchronization.Construct           PROCEDURE()
 CODE
    SELF.Wait()
    SELF.GainFlag = True

MDISynchronization.Destruct            PROCEDURE()
 CODE
    YIELD()
    SELF.Release()

MDISynchronization.Wait                PROCEDURE()
 CODE
    IF SELF.Locked = False
       SELF.Locked = True
       GlobalMDISyncroLook.Wait()
    END

MDISynchronization.Release             PROCEDURE()
 CODE
    IF SELF.Locked = True
       SELF.Locked = False
       GlobalMDISyncroLook.Release()
    END

MDISynchronization.TakeEvent           PROCEDURE()
NCode   UNSIGNED
NThread SIGNED
NParameter LONG
RetVal  BYTE
EventToPost LONG
 CODE
    RetVal = False
    IF NOT SELF.CloseFlag
       CASE EVENT()
       OF EVENT:Notify
         IF NOTIFICATION (NCode,NThread,NParameter)
            CASE NCode
            OF EVENT:OpenWindow
               IF NOT 0{PROP:Active} AND NOT 0{PROP:ICONIZE}
                  0{PROP:Active}=1
                  NOTIFY(EVENT:OpenWindow,NThread)
                  RetVal = True
               ELSE
                  YIELD()
                  SELF.Release()
                  POST(EVENT:OpenWindow,0,NThread)
                  SELF.OpenFlag = True
               END
            OF EVENT:CloseWindow
               SELF.CloseFlag = true
               POST(EVENT:CloseWindow,0,NThread)
               RetVal = True
            OF EVENT:GainFocus
               IF NOT 0{PROP:Active} AND NOT 0{PROP:ICONIZE}
                  YIELD()
                  0{PROP:Active}=1
                  NOTIFY(EVENT:GainFocus,NThread)
                  RetVal = True
               ELSE
                  SELF.Release()
                  SELF.GainFlag = True
                  POST(EVENT:GainFocus,0,NThread)
                  RetVal = True
               END
            END
         END
       OF EVENT:OpenWindow
          IF SELF.OpenFlag
             RetVal = False
             SELF.OpenFlag = False
          ELSE
             NOTIFY(EVENT:OpenWindow)
             RetVal = True
          END
       OF EVENT:GainFocus
          IF NOT SELF.GainFlag
             SELF.Wait()
             NOTIFY(EVENT:GainFocus)
             RetVal = True
          ELSE
             SELF.GainFlag = False
             RetVal = False
          END
       OF EVENT:CloseWindow
          NOTIFY(EVENT:CloseWindow)
          RetVal = True
       END
    END
    RETURN RetVal
