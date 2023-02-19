               MEMBER

               MAP
               END

               INCLUDE ('CWTOUCH.INC'),ONCE

! === Dummy implementation of Next methods of classes declared in CWTOUCH.INC

InputPoint.Next  PROCEDURE()
  CODE
  RETURN NULL;

TouchPoint.Next  PROCEDURE()
  CODE
  RETURN NULL

InputData.Next  PROCEDURE()
  CODE
  RETURN NULL

TouchData.Next  PROCEDURE()
  CODE
  RETURN NULL

