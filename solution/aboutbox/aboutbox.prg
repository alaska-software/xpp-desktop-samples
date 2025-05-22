//////////////////////////////////////////////////////////////////////
//
//  ABOUTBOX.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Function AboutBox() for display of program information.
//   
//////////////////////////////////////////////////////////////////////


#include "Aboutbox.ch"
#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"


#define  BITMAP_MAX_WIDTH          200
#define  BITMAP_MAX_HEIGHT         240


#ifdef   DEBUG

/*
 * Test function for ABoutBox()
 */

PROCEDURE Main
   LOCAL cYear := Str(Year(Date()),4)

   AboutBox( "Program Information"                     , ; // title
             "The Number Cruncher"                     , ; // program
             "Version 1.00.15"                         , ; // version
             "Copyright (c) "+cYear+", Smart Guys Inc.", ; // copyright
             "Supports latest technology" + Chr(13) + ;
             "for number crunching"                 )      // miscellaneous

   AboutBox( "Alaska Xbase++ Version Information" , ; // title
             "Alaska Xbase++ "                    , ; // program
             "Version " + Version(1) + "." + ;
                          Version(2) + "." + ;
                          Version(3)              , ; // Version
             "Alaska Software"      + Chr(13) + ;     // copyright
             "Copyright (c) 1996-"+cYear+""       , ;
             "Multi-threaded"       + Chr(13) + ;
             "database development" + Chr(13) + ;
             "for 32bit operating systems"        , ; // miscellaneous
             ID_ABOUT_BITMAP                        ) // bitmap max. 200x240

RETURN

#endif  // DEBUG



/*
 * Display program information and an optional bitmap logo
 */

PROCEDURE AboutBox( cTitle, cProgram, cVersion, cCopyright, cMisc, nBitmap )
   LOCAL aSize, aSize2, nDX, drawingArea
   LOCAL oDlg, oLogo, oBtn, aPos, oXbp

   DEFAULT cTitle TO "" , ;
         cProgram TO "" , ;
         cVersion TO "" , ;
       cCopyright TO "" , ;
            cMisc TO ""

   aSize := { 250, 280 }
   aPos  := CenterPos( aSize, AppDesktop():currentSize() )

   /*
    * ESC will close the modal dialog
    */
   oDlg := XbpDialog():new( AppDesktop(), SetAppWindow(), aPos, aSize, , .F.)
   oDlg:taskList := .F.
   oDlg:minButton:= .F.
   oDlg:maxButton:= .F.
   oDlg:border   := XBPDLG_DLGBORDER
   oDlg:title    := cTitle
   oDlg:keyboard := {|nKey| IIF( nKey == xbeK_ESC, ;
                            oDlg:modalResult := XBP_MRESULT_CANCEL,  ) }
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   oXbp         := XbpStatic():new( drawingArea, , {16,204}, {216,24} )
   oXbp:caption := cProgram
   oXbp:setFontCompoundName( FONT_HELV_MEDIUM + FONT_STYLE_BOLD )
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oXbp:create()

   IF nBitmap <> NIL
      aPos    := oXbp:currentPos()
      aPos[2] += oXbp:currentSize()[2]
   ENDIF

   oXbp := XbpStatic():new( drawingArea, , {16,180}, {216,12} )
   oXbp:caption := cVersion
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {16,132}, {216,36} )
   oXbp:caption := cCopyRight
   oXbp:options := XBPSTATIC_TEXT_WORDBREAK+XBPSTATIC_TEXT_TOP+XBPSTATIC_TEXT_CENTER
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {16,120}, {216,2} )
   oXbp:type := XBPSTATIC_TYPE_RAISEDLINE
   oXbp:create()

   oXbp := XbpStatic():new( drawingArea, , {16,60}, {216,48} )
   oXbp:caption := cMisc
   oXbp:options := XBPSTATIC_TEXT_WORDBREAK+XBPSTATIC_TEXT_TOP+XBPSTATIC_TEXT_CENTER
   oXbp:create()

  /*
   * The pushbutton will close the modal dialog
   */
   oBtn := XbpPushButton():new( drawingArea, , {86,12}, {67,24} )
   oBtn:caption  := "Ok"
   oBtn:default := .T.
   oBtn:create()

   IF nBitmap <> NIL
      oXbp      := XbpStatic():new( drawingArea )
      oXbp:type := XBPSTATIC_TYPE_RAISEDBOX
      oXbp:create()

      oLogo          := XbpStatic():new( oXbp, , {2,2} )
      oLogo:type     := XBPSTATIC_TYPE_BITMAP
      oLogo:caption  := nBitmap
      oLogo:autoSize := .T.
      oLogo:create()

     /*
      * Size of the bitmap is limited
      */
      aSize    := oLogo:currentSize()
      aSize[1] := Min( BITMAP_MAX_WIDTH , aSize[1] )
      aSize[2] := Min( BITMAP_MAX_HEIGHT, aSize[2] )
      nDX      := aSize[1] + 12

      aSize2   := oDlg:currentSize()
      oDlg:setSize( { aSize2[1] + nDX + 4, aSize2[2] } )
      ChangePos( oDlg, { -nDX / 2, 0 } )

      aSize2   := { nDX, 0 }
      AEval( drawingArea:childList(), {|o| ChangePos( o, aSize2 ) } )
      aSize[1] += 4
      aSize[2] += 4
      oXbp:setSize( aSize )
      oXbp:setPos( { 12, aPos[2] -aSize[2] } )
   ENDIF

   oDlg:showModal()

RETURN



/*
 * Change the position of an XBP by distance
 */
STATIC PROCEDURE ChangePos( oXbp, aDistance )
   LOCAL aPos := oXbp:currentPos()

   aPos[1] += aDistance[1]
   aPos[2] += aDistance[2]

   oXbp:setPos( aPos )

RETURN



/*
 * Calculate the center position from size and reference size
 */

STATIC FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }
