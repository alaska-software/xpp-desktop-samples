//////////////////////////////////////////////////////////////////////
//
//  LOGIN_G.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Simple login routine for GUI. This file is the end
//     for a small example that shows the migration of Clipper code. See
//     LOGIN_T.PRG (Text mode) and LOGIN_H.PRG (GUI) to get an idea about
//     code changes.
//   
//   Changes :
//     - The application window is created in AppSys() as XbpDialog window
//     - The PassWord() function creates its own dialog that was painted
//       with the Xbase++ FormDesigner
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"

PROCEDURE AppSys
   LOCAL oDlg, aPos[2], aSize

  /*
   * Get size of the desktop to display the application window centered
   */
   aSize         := SetAppWindow():currentSize()
   aPos[1]       := (aSize[1] - 640) / 2
   aPos[2]       := (aSize[2] - 400) / 2

   oDlg          := XbpDialog():new( SetAppWindow(), , aPos, {640,400}, , .F.)
   oDlg:title    := "GUI Demo"
   oDlg:taskList := .T.
   oDlg:icon     := 1
   oDlg:close    := {|| AppQuit() }
   oDlg:create()
   oDlg:drawingArea:SetColorBG( GRA_CLR_WHITE )
   SetAppWindow( oDlg )
   oDlg:show()
RETURN

PROCEDURE AppQuit
   QUIT
RETURN

PROCEDURE Main
   LOCAL cPassWord, cUserID, oLogo

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
   LOCAL oLogo, aPos, aSize, drawingArea := SetAppWindow():drawingArea

   aSize         := drawingArea:currentSize()
   aPos          := { (aSize[1]-600)/2, aSize[2]-200 }
   oLogo         := XbpStatic():new( drawingArea,,aPos,{600,200})
   oLogo:type    := XBPSTATIC_TYPE_BITMAP
   oLogo:caption := 2001
   oLogo:create()
RETURN oLogo



/*
 * Login routine for entering pass word and user ID
 *
 * - More than 90% of this code is written by the Xbase++ FormDesigner.
 */
PROCEDURE PassWord( cUserID, cPassWord )
   LOCAL nEvent, mp1 := NIL, mp2 := NIL, oXbp, lExit, aPos, aWinPos
   LOCAL oDlg, oID, oPW, drawingArea, aSize

   aSize            := {250,150}
   aPos             := CenterPos( aSize, SetAppWindow():drawingArea:currentSize() )
   aWinPos          := SetAppWindow():currentPos()
   aPos[1]          += aWinPos[1]
   aPos[2]          += aWinPos[2]

  /*
   * Parent is desktop, owner is application window
   */
   oDlg             := XbpDialog():new( SetAppWindow():setParent(), SetAppWindow(), aPos, aSize, , .F.)
   oDlg:border      := XBPDLG_RAISEDBORDERTHIN_FIXED
   oDlg:title       := "Enter your ID and password!"
   oDlg:sysMenu     := .F.
   oDlg:maxButton   := .F.
   oDlg:minButton   := .F.
   oDlg:sysMenu     := .F.
   oDlg:visible     := .F.
   oDlg:close       :=  {|| PostAppEvent( xbeP_Close ) }
   oDlg:create()
   oDlg:setFontCompoundName( "8.Helv.normal" )

   drawingArea      := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv.normal" )

   oXbp             := XbpStatic():new( drawingArea, , {8,84}, {80,20} )
   oXbp:caption     := "User ID:"
   oXbp:options     := XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oID              := XbpSLE():new( drawingArea, , {101,83}, {116,22} )
   oID:group        := XBP_BEGIN_GROUP
   oID:tabStop      := .T.
   oID:bufferLength := Len( cUserID )
   oID:create()

   oXbp             := XbpStatic():new( drawingArea, , {8,52}, {80,20} )
   oXbp:caption     := "Password:"
   oXbp:options     := XBPSTATIC_TEXT_RIGHT
   oXbp:create()

   oPW              := XbpSLE():new( drawingArea, , {101,51}, {116,22} )
   oPW:group        := XBP_WITHIN_GROUP
   oPW:tabStop      := .T.
   oPW:bufferLength := Len( cPassWord )
   oPW:unReadable   := .T.
   oPW:create()

   oXbp             := XbpPushButton():new( drawingArea, , {101,11}, {75,25} )
   oXbp:caption     := "OK"
   oXbp:tabStop     := .T.
   oXbp:group       := XBP_END_GROUP
   oXbp:activate    := {|| lExit := .T. }
   oXbp:keyboard    := {|nKey,x,obj| IIf( nKey == xbeK_ESC .OR. nKey == xbeK_RETURN, ;
                                     PostAppEvent( xbeP_Activate,,, obj ), NIL ) }
   oXbp:create()

   oDlg:show()
  /*
   * This makes the login window modal
   */
   SetAppWindow():disable()
   SetAppFocus( oID )

   lExit := .F.
   DO WHILE ! lExit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   cUserID   := AllTrim( Upper( oID:getData() ) )
   cPassWord := AllTrim( Upper( oPW:getData() ) )

   SetAppWindow():enable()
   oDlg:destroy()

  /*
   * Clean up event queue
   */
   CLEAR TYPEAHEAD
RETURN



/*
 * Calculate the center position for a window from its size and a
 * reference size.
 */
STATIC FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }
