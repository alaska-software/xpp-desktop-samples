//////////////////////////////////////////////////////////////////////
//
//  CRTBOX.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The example demonstrates with Achoice() and MemoEdit() how
//      single procedures of a program can be encapsulated in a nodal
//      XbpCrt window when an existing application is migrated to GUI.
//   
//   
//////////////////////////////////////////////////////////////////////


#include "Common.ch"
#include "Inkey.ch"
#include "Gra.ch"
#include "Xbp.ch"



STATIC  saMonth := { "January", "February", "March", "April", "May", ;
                     "June", "July", "August", "September", "October", ;
                     "November", "December" }


PROCEDURE Main
   LOCAL cMonth1, cMonth2, GetList := {}

   SetBlink( .F. )
   SetMouse( .T. )
   SET CURSOR ON
   SET SCOREBOARD OFF

   DO WHILE .T.
      SetColor( "N/BG,W+/B" )
      cMonth1 := ;
      cMonth2 := Space( 9 )
      CLS

     /*
      * GET validation using AChoice()
      */
      @  8, 10 SAY "Common popup window"
      @  9, 10 SAY "     Enter month:" GET cMonth1 VALID {|oGet| ValidGet1( oGet ) }

      @ 12, 10 SAY "XbpCrt popup window"
      @ 13, 10 SAY "     Enter month:" GET cMonth2 VALID {|oGet| ValidGet2( oGet ) }

      READ

      @ 4, 4 TO Maxrow()-4, MaxCol()-4 DOUBLE

      SetColor( "W+/B" )

     /*
      * Text editing
      */
      MemoEdit( "Press Ctrl+W to see MemoEdit() in a popup window", 5, 5, Maxrow()-5, MaxCol()-5 )
      CrtMemoEdit( "Press Ctrl+W to terminate MemoEdit()", 5, 5, Maxrow()-5, MaxCol()-5 )

      CLS
      @  MaxRow(), 0 SAY "press Esc to QUIT, any other key repeats the example"
      Inkey(0)
      IF Lastkey() == K_ESC
         QUIT
      ENDIF
   ENDDO
RETURN



/*
 * Validate a month name:
 *    Popup window uses SaveScreen() / Restscreen()
 */
FUNCTION ValidGet1( oGet )
   LOCAL cMonth  := Alltrim( Upper( oGet:varGet() ) )
   LOCAL cScreen, lValid, nMonth, nT, nL, nB, nR
   LOCAL cColor := SetColor( "N/W*, W+/B" )

   lValid := ( AScan( saMonth, {|c| Upper(c) == cMonth } ) ) > 0

   IF ! lValid
      nT := oGet:row + 1
      nL := oGet:col + 1
      nB := nT + 6
      nR := nL + 12
      cScreen := SaveScreen(  nT-1, nL-1, nB+1, nR+1 )

      @ nT-1, nL-1 TO nB+1, nR+1 DOUBLE
      nMonth := Achoice(  nT, nL, nB, nR, saMonth )

      IF nMonth > 0
         oGet:varPut( Padr( saMonth[ nMonth ], 9 ) )
         lValid := .T.
      ENDIF

      RestScreen( nT-1, nL-1, nB+1, nR+1 , cScreen )
   ENDIF

   SetColor( cColor )
RETURN lValid



/*
 * Validate a month name:
 *    XbpCrt popup window
 */
FUNCTION ValidGet2( oGet )
   LOCAL cMonth  := Alltrim( Upper( oGet:varGet() ) )
   LOCAL lValid, nMonth, nT, nL, nB, nR
   LOCAL cColor := SetColor( "N/W*, W+/B" )

   lValid := ( AScan( saMonth, {|c| Upper(c) == cMonth } ) ) > 0

   IF ! lValid
      nT := oGet:row + 1
      nL := oGet:col + 1
      nB := nT + 6
      nR := nL + 12
      nMonth := CrtAchoice( nT-1, nL-1, nB+1, nR+1, saMonth )

      IF nMonth > 0
         oGet:varPut( Padr( saMonth[ nMonth ], 9 ) )
         lValid := .T.
      ENDIF
   ENDIF

   SetColor( cColor )
RETURN lValid



/*
 * Wrapper function for a popup window that runs Achoice().
 * The parameter interface is the same as Achoice()'s. One parameter
 * is added to display a window title.
 */
FUNCTION CrtAchoice( nTop        , ;
                     nLeft       , ;
                     nBottom     , ;
                     nRight      , ;
                     aItems      , ;
                     alCanSelect , ;
                     bcUserFunc  , ;
                     nStartItem  , ;
                     nStartRow   , ;
                     cTitle        )

   LOCAL bAction, xReturn

   DEFAULT cTitle TO "Select Item"

   bAction := {|| Achoice( 1, 1, MaxRow()-1, MaxCol()-1, aItems, alCanSelect, ;
                           bcUserFunc, nStartItem, nStartRow ) }

   xReturn := CrtBox( nTop, nLeft, nBottom, nRight, cTitle, bAction )

RETURN xReturn



/*
 * Wrapper function for a popup window that runs Memoedit()
 */
FUNCTION CrtMemoEdit( cString   , ;
                      nTop      , ;
                      nLeft     , ;
                      nBottom   , ;
                      nRight    , ;
                      lEditMode , ;
                      bcUserFunc, ;
                      nLineLen  , ;
                      nTabSize  , ;
                      nBufferRow, ;
                      nBufferCol, ;
                      nWindowRow, ;
                      nWindowCol, ;
                      cTitle      )

   LOCAL bAction, xReturn

   DEFAULT cTitle TO "Edit Text"

   bAction := {|| Memoedit( cString, 1, 1, MaxRow()-1, MaxCol()-1, lEditmode, ;
                            bcUserFunc, nLineLen, nTabSize,  nBufferRow, ;
                            nBufferCol, nWindowRow,  nWindowCol ) }

   xReturn := CrtBox( nTop, nLeft, nBottom, nRight, cTitle, bAction )

RETURN xReturn



/*
 * All procedures of a program using a popup window (i.e. embedded within
 * Savescreen() / Restscreen()) can be run in a modal XbpCrt window.
 */
FUNCTION CrtBox( nT, nL, nB, nR, cTitle, bAction )
   LOCAL oAppWindow, cColor, nCursor, oError
   LOCAL oCrt, nRowCount, nColCount, nX, nY, xReturn

   oAppWindow := SetAppWindow()

   IF .NOT. oAppWindow:isDerivedFrom( "XbpCrt" )
     /*
      * This function requires SetAppWindow() to return a XbpCrt window
      */
      oError               := Error():new()
      oError:description   := "SetAppwindow() must return an XbpCrt window"
      oError:canDefault    := .F.
      oError:canRetry      := .F.
      oError:canSubstitute := .F.
      oError:operation     := "CrtBox"
      oError:args          := { nT, nL, nB, nR, cTitle, bAction }
      Break( oError )
   ENDIF

   DEFAULT nT      TO 0        , ;
           nL      TO 0        , ;
           nB      TO MaxRow() , ;
           nR      TO MaxCol() , ;
           cTitle  TO " "

  /*
   * Calculate graphical coordinates from text coordinates (row/col -> x/y)
   */
   nRowCount  := nB - nT + 1
   nColCount  := nR - nL + 1
   nX         := nL * oAppWindow:fontWidth
   nY         := ( MaxRow() - nB ) * oAppWindow:fontHeight
   nX         += oAppWindow:currentPos()[1]
   nY         += oAppWindow:currentPos()[2]
   cColor     := SetColor()
   nCursor    := SetCursor()

  /*
   * Create new XbpCrt window
   */
   oCrt := XbpCrt():new( AppDeskTop(), oAppWindow, {nX,nY}, nRowCount, nColCount, cTitle )
   oCrt:minmax       := .F.
   oCrt:sysmenu      := .F.
   oCrt:closeable    := .F.
   oCrt:clipChildren := .F.
   oCrt:border       := XBPDLG_RECESSEDBORDERTHICK_FIXED
   oCrt:fontName     := oAppwindow:fontName
   oCrt:fontHeight   := oAppWindow:fontHeight
   oCrt:fontWidth    := oAppWindow:fontWidth
   oCrt:create()

   SetAppWindow( oCrt )
   SetColor( "N/W" )
   CLS

   SetColor ( cColor  )
   SetCursor( nCursor )
   SetMouse ( .T. )

  /*
   * Switch to modal state and give focus
   */
   oCrt:setModalState( XBP_DISP_APPMODAL )
   SetAppFocus( oCrt )

  /*
   * Procedure is called from the received code block
   */
   xReturn := Eval( bAction )

  /*
   * Clean up
   */
   oCrt:setModalState( XBP_DISP_MODELESS )
   oCrt:destroy()

   SetAppWindow( oAppWindow )
   SetAppFocus ( oAppWindow )

RETURN xReturn
