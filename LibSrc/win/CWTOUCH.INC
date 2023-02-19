	      INCLUDE ('CWTOUCHDEF.INC'),ONCE

TPOINT        GROUP,TYPE !Touch point coordinates
x               LONG
y               LONG
              END

! --- Time fields ----------------------------------------------------

! All time fields in structures declared below are in milliseconds since
! OS start. They can be used to find elapse between events, e.g. time
! since last pointer movement.

! --- Position structures --------------------------------------------

InputPoint    CLASS,TYPE,MODULE('CWTOUCH.CLW') ! ,FINAL

Cookie          LONG,PRIVATE                ! INTERNAL
pt              LIKE(TPOINT)                 ! Coordinates of point in screen coordinates
ptLocal         LIKE(TPOINT)                 ! Coordinates of point in window/control coordinates

Next            PROCEDURE(),*InputPoint,VIRTUAL
              END

! --- Information about touch points ---------------------------------

TouchPoint    CLASS,TYPE,MODULE('CWTOUCH.CLW') ! ,FINAL

Cookie          LONG,PRIVATE                ! INTERNAL
pt              LIKE(TPOINT)                 ! Coordinates of point in screen coordinates
ptLocal         LIKE(TPOINT)                 ! Coordinates of point in window/control coordinates
ID              UNSIGNED                    ! System ID of touch point
PTType          SIGNED                      ! Value from PointerTypes
ia              SIGNED                      ! Value from InputAction
target          UNSIGNED                    ! HWND of target of touch input
PTFlags         LONG                        ! PointFlags
buttons         USHORT                      ! Pressed buttons
param           USHORT                      ! Touch parameters
distance        LONG                        ! Parameter of TOUCH_WHEEL
pressure        ULONG                       ! Touch or pen pressure
tiltX           LONG                        ! Pen tilt angle along the X-axis
tiltY           LONG                        ! Pen tilt angle along the Y-axis
rotation        ULONG                       ! Angle of pen rotation
orientation     ULONG,OVER(rotation)        ! Angle of touch point orientation
downtime        LONG                        ! Time of go to contact
lastmovetime    LONG                        ! Time of last detected movement
updatetime      LONG                        ! Time of last update

Next            PROCEDURE(),*TouchPoint,VIRTUAL
              END

! --- Input data passed from the RTL ---------------------------------

InputData     CLASS,TYPE,MODULE('CWTOUCH.CLW') ! ,FINAL

Cookie          LONG,PRIVATE                ! INTERNAL
W               &WINDOW                     ! WINDOW receiving touch input
Ctl             SIGNED                      ! control
ia              SIGNED                      ! InputAction
ptAction        &InputPoint                 ! Action point
p1              &InputPoint                 ! Point1 - depends from ia
pt2             &InputPoint                 ! Point2 - depends from ia
buttons         USHORT                      ! InputFlags - buttons pressed during input event
param           USHORT                      ! InputFlags - parameters of input event

angle           REAL                        ! Rotate parameter
ratio           REAL,OVER(angle)            ! Zoom-in / Zoom-out parameter
delta           LIKE(TPOINT),OVER(angle)     ! Press-and-tap parameter
distance        LONG,OVER(angle)            ! Two-finger tap and pan parmaeter

scroll          LIKE(TPOINT)                 ! Mouse-wheel and pan parameter
speed           LIKE(TPOINT)                 ! Pan parameter

Next            PROCEDURE(),*InputData,VIRTUAL
              END

! --- Touch data passed from the RTL ---------------------------------


TouchData     CLASS,TYPE,MODULE('CWTOUCH.CLW') ! ,FINAL

Cookie          LONG,PRIVATE                ! INTERNAL
W               &WINDOW                     ! WINDOW receiving touch input
Ctl             SIGNED                      ! control

touchtime       LONG                        ! Time of touch event

ptAction        &TouchPoint                 ! New state of touch point caused event
points          &TouchPoint                 ! States of all points before event

Next            PROCEDURE(),*TouchData,VIRTUAL
              END

! -------------------------------------------------------------------
!     Notification interfaces
! -------------------------------------------------------------------

! Event() methods of Responder interfaces must return FALSE if input
! notification is not processed in the program => the RTL can apply
! its own processing

IGestureResponder INTERFACE,COM
Event               PROCEDURE (*InputData),BYTE
                  END

IPointerResponder INTERFACE,COM
Event               PROCEDURE (*TouchData),BYTE
                  END

! --- Access to touch handler in the RTL -----------------------------

! An interface returned in response to Window{PROP:TouchInterface}

ITouchNotifier    INTERFACE,COM
InputResponder      PROCEDURE (<*IGestureResponder>)
TouchResponder      PROCEDURE (<*IPointerResponder>)
AllowZoomGesture    PROCEDURE (SIGNED feq, BYTE on)
AllowRotateGesture  PROCEDURE (SIGNED feq, BYTE on)
                  END
