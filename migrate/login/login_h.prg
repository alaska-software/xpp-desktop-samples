//////////////////////////////////////////////////////////////////////
//
//  LOGIN_H.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Simple login routine for hybrid mode. This file is the continuation
//     for a small example that shows the migration of Clipper code. See
//     LOGIN_T.PRG (Text mode) and LOGIN_G.PRG (GUI) to get an idea about
//     code changes.
//   
//   Changes:
//     - Gets are mouse aware
//     - Inkey() is replaced with AppEvent()
//     - GetApplyKey() is replaced with GetHandleEvent()
//     - Company logo is a bitmap
//     - @ CLEAR TO is replaced with GraBox()
//     - Alert() is replaced with MsgBox()
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Gra.ch"
#include "Xbp.ch"


PROCEDURE Main
   LOCAL cPassWord, cUserID, oLogo

   SET SCOREBOARD OFF
   SET CURSOR OFF
   SetBlink( .F. )
  /*
   * Enable mouse for text mode functions
   */
   SetMouse( .T. )

  /*
   * Display and store logo
   */
   oLogo     := DisplayLogo()
   cUserID   := Space(15)
   cPassWord := Space(15)

   PassWord( @cUserID, @cPassWord )

   IF cPassWord == "ALASKA"            // Alert() is replaced by MsgBox()
      MsgBox( "Password is correct"+Chr(10)+"Access granted" )
   ELSE
      MsgBox( "Password is wrong,"+Chr(10)+ ;
              "Access denied!" )
   ENDIF

  /*
   * Release resources for logo
   */
   oLogo:destroy()

RETURN


/*
 * Display a company logo
 *
 * - XbpStatic is used to display a bitmap showing a logo
 *
 * - The bitmap is 600 x 200 pixel in size and has the numeric ID 2001
 *   The ID is defined in an RC file
 *
 * - aSize = Size of the "drawingArea" of an XbpCrt window
 */
FUNCTION DisplayLogo
   LOCAL oLogo, aPos, aSize
   LOCAL cColor := SetColor("B/W*")

   CLS
   SetColor( cColor )

   aSize         := { SetAppWindow():xSize, SetAppWindow():ySize }
   aPos          := { (aSize[1]-600)/2, aSize[2]-200 }
   oLogo         := XbpStatic():new(,,aPos)
   oLogo:autoSize:= .T.
   oLogo:type    := XBPSTATIC_TYPE_BITMAP
   oLogo:caption := 2001
   oLogo:create()
RETURN oLogo



/*
 * Login routine for entering pass word and user ID
 *
 * - Replaces @ 13, 25 CLEAR TO 21, 55 with UDF CrtClear() -> GraBox()
 *
 */
PROCEDURE PassWord( cUserID, cPassWord )
   LOCAL GetList := {}

   CrtClear( 13, 25, 21, 55 )

   SET CURSOR ON
   @ 15, 27 SAY "Enter your ID and password!"
   @ 17, 28 SAY " User ID:" GET cUserID
   @ 19, 28 SAY "Password:" GET cPassWord SEND reader := {|o| HiddenReader(o) }
   READ
   SET CURSOR OFF

   cUserID   := AllTrim( Upper( cUserID ) )
   cPassWord := AllTrim( Upper( cPassWord ) )
RETURN



/*
 * Example for the transformation of row/col coordinates to x/y coordinates
 *
 * - Graphic coordinates are calculated from the font size of an XbpCrt window
 *     Col coordinates = x
 *     Row coordinates = y
 */
PROCEDURE CrtClear( nT, nL, nB, nR )
   LOCAL nWidth  := SetAppWindow():fontWidth
   LOCAL nHeight := SetAppWindow():fontHeight
   LOCAL aPos1, aPos2

   aPos1 := { (nL  ) * nWidth, (MaxRow()-nB  ) * nHeight }
   aPos2 := { (nR+1) * nWidth, (MaxRow()-nT+1) * nHeight }
   GraBox( , aPos1, aPos2, GRA_FILL, 30, 30 )
RETURN


/*
 * Get reader for hidden input of a string
 *
 * - The reader is changed from Inkey() to AppEvent()
 *
 * - Instead of K_* from Inkey.ch xbeK_* constants are used from Appevent.ch
 *
 * - GetHandleEvent() replaces GetApplyKey()
 */
PROCEDURE HiddenReader( oGet )
   LOCAL cBuffer, cChar, nEvent, mp1 := NIL, mp2 := NIL, oXbp := NIL

   IF GetPreValidate( oGet )
      CLEAR TYPEAHEAD
      oGet:setFocus()

      cBuffer := oGet:buffer
      cChar   := Replicate( "*", Len(Trim(cBuffer)) )
      oGet:varPut( PadR(cChar, Len(cBuffer)) )
      oGet:updateBuffer()

      DO WHILE oGet:exitState == GE_NOEXIT

         IF oGet:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

         DO WHILE oGet:exitState == GE_NOEXIT
            nEvent := Appevent( @mp1, @mp2, @oXbp, 0 )

            DO CASE
            CASE nEvent == xbeM_LbClick
               GetHandleEvent( oGet, nEvent, mp1, mp2, oXbp )

            CASE nEvent == xbeP_Keyboard
               nEvent := mp1

               DO CASE
               CASE mp1 >= 32 .AND. mp1 <= 255
                  GetHandleEvent( oGet, Asc( "*" ) )
                  cChar := Chr( mp1 )

                  IF oGet:typeOut
                     cBuffer := Stuff( cBuffer, oGet:pos, 1, cChar )
                  ELSEIF Set( _SET_INSERT )
                     cBuffer := Stuff( cBuffer, oGet:pos-1, 0, cChar )
                     cBuffer := SubStr( cBuffer, Len(cBuffer)-1 )
                  ELSE
                     cBuffer := Stuff( cBuffer, oGet:pos-1, 1, cChar )
                  ENDIF

               CASE mp1 == xbeK_DEL .OR. mp1 == xbeK_BS
                  GetHandleEvent( oGet, nEvent, mp1, mp2, oXbp )
                  cBuffer := Stuff( cBuffer, oGet:pos, 1, "" )
                  cBuffer += " "

               OTHERWISE
                  GetHandleEvent( oGet, nEvent, mp1, mp2, oXbp )

               ENDCASE
            ENDCASE

         ENDDO

         IF ! GetPostValidate( oGet )
            oGet:exitState:= GE_NOEXIT
         ENDIF
      ENDDO

      oGet:killFocus()

      oGet:varPut( cBuffer )
   ENDIF

RETURN
