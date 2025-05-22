//////////////////////////////////////////////////////////////////////
//
//  WINDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Demonstration of XbaseTools Window-Functions.
//   
//////////////////////////////////////////////////////////////////////


#include "Inkey.ch"
#include "Common.ch"

PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL cString, i, nWin1, nWin2

   // IMPORTANT:
   // Initialize XbTools window sytem.
   // Create main window (RootCRT) and activate it.
   Winit()

   SetBlink ( .F. )
   SetMouse ( .T. )
   SetClearB( 176 )                    // set default character for CLS
   SetColor ( "W/B" )

   CLS                                 // fill RootCrt
   cString := Replicate( "XbTools", 12 )
   FOR i:=0 TO MaxRow()
      @ i, 0 SAY cString
   NEXT

   WinMsg( { "A non-writeable area of the main-window (RootCrt)", ;
             "will now be set with WBoard()"                    , ;
             ""                                                 , ;
             "Press any key to continue..."}, 0, .T. )

   @  4, 4 CLEAR TO 20, 75             // clear WBoard area
   WBoard( 4, 4, 20, 75)               // define WBoard area
   WSetShadow( 8 )                     // color for shadows -> "N+/N"
   WMode( .T., .T., .T., .T. )         // windows can be moved beyond the
                                       // borders of WBoard area

                                       // open first window
   nWin1 := WOpen( 6, 6, 15, 20, .T., "N/BG", 0 )
   WBox( 3 )
   WSetTitle( 0, 1, "Window 1"   , "W+/BG" )
   WSetTitle( 1, 1, "Status Row" , "GR+/BG")
   @ 0, 0 SAY "Hello World"

                                       // open second window
   nWin2 := WOpen( 10, 15, 22, 35, .T., "R/W", 2 )
   WBox( 1 )
   WSetTitle( 0, 1, "Window 2" )
   WSetColor( "W+/R" )
   @ 2, 3 SAY "Hello World"
   WSetColor( "W+/G" )
   @ 3, 3 SAY "Hello World"
   WSetColor( "W+/B" )
   @ 4, 3 SAY "Hello World"

   WinMsg( { "Left-Button-Click in a window activates the window", ;
             "Left-Button-Click on the window-frame let you move", ;
             "the window." , ;
             "If you press Alt+F7 you can move the window with"  , ;
              "the cursor keys."                                 , ;
             ""                                                  , ;
             "Press any key to continue..."}, 0, .T. )

   // save first line in RootCrt
   cString := WSaveScreen( 0, 0, 0, 0, 80 )

   // print message in first line of RootCrt
   WSay( 0, 0, 10, " Press ESC to continue...", "N/W*" )

   // move windows as long as ESC has been pressed
   DO WHILE Inkey(0) <> K_ESC
   ENDDO

   // restore first line of RootCrt
   WRestScreen( 0, 0, 0, 0, 80, cString )

   WinMsg( { "Set the RootCrt window frame via WBox()", ;
             ""                                       , ;
             "Press any key to continue..." }, 0, .T.)

   WSelect( 0 )                       // show box in window 0
   WBox  ( 12 )

   StatusLine( "==> Press any key to continue..." )

   WinMsg( {"Window 1 will be moved with a WMove(0, 0)", ;
            "beyond the visible window area"           , ;
             ""                                        , ;
            "Press any key continue..."}, 0, .T.)

   WSelect( nWin1 )
   WMove  ( 0, 0  )

   WinMsg( { "Make Window 1 completely visible with a", ;
             "call to WCenter( .F. )", ;
             ""                                 , ;
            "Press any key continue..."}, 0, .T.)

   WCenter( .F. )
   StatusLine( "==> Press any key to continue..." )

   WinMsg( { "Center Window 1 with WCenter( .T. )", ;
             ""                                 , ;
            "Press any key continue..."}, 0, .T.)

   WCenter( .T. )
   StatusLine( "==> Press any key to continue..." )

   WinMsg( { "Close Window 1 with WClose()", ;
             ""                                       , ;
            "Press any key continue..."}, 0, .T.)

   WSelect( nWin1 )
   WClose()

   @ 0,0 SAY "Now active"
   StatusLine( "==> Press any key to continue..." )

   WinMsg( {"All Windows will be closed with WAClose()", ;
            ""                                         , ;
            "Press any key continue..."}, 0, .T.)

   WAClose()
   CLS
RETURN

******************************************************************************
* show window with message until time exceeds timeout or a
* key is pressed
******************************************************************************
PROCEDURE WinMsg( aText, nTimeOut, lTone)
   LOCAL nRow, nCol, nSaveFill, i
   LOCAL nRowCount := Len( aText )
   LOCAL aWBoard   := GetWBoard()
   LOCAL nLen      := 0
   LOCAL nHeight   := aWBoard[3] - aWBoard[1]
   LOCAL nWidth    := aWBoard[4] - aWBoard[2]

   DEFAULT lTone    TO .F. , ;
           nTimeOut TO  0

   // get coordinates to center window
   // screen row:
   nRow := aWBoard[1] + (nHeight-nRowCount)/2

   // get length of longest text row
   AEval( aText, {|c| nLen := Max( Len(c), nLen ) } )

   // screen column:
   nCol := aWBoard[2] + (nWidth-nLen)/2

   nSaveFill := WSetClearB( 32 )       // create new window
   WOpen( nRow-1, nCol-1, nRow+nRowCount, nCol+nLen, .T., "N/W*" )
   WBox()                              // draw frame
   WMouseSelect(.F.)                   // select mouse
   FOR i:=0 to nRowCount-1             // show text in new
      @ i, 0 SAY aText[i+1]            // window
   NEXT
   IF lTone
      Tone(440,1)
   ENDIF

   Inkey( nTimeOut )                   // inkey() with timeout

   WClose()                            // close window
   WSetClearB( nSaveFill )             // restore CLEAR attribute
   WMouseSelect( .T. )                 // activate mouse selection

RETURN


******************************************************************************
* display status line in RootCrt with timeout
******************************************************************************
PROCEDURE StatusLine( cStatus, nTimeOut )
LOCAL cScreen, nWin := WSelect(), nMaxRow, nMaxCol

   DEFAULT nTimeOut TO 0

   WSelect( 0 )                        // activate RootCrt

                                       // display message
   cStatus := PadC( cStatus, MaxCol()+1 )
   nMaxRow := MaxRow()
   nMaxCol := MaxCol()
   cScreen := WSaveScreen( 0, nMaxRow, 0, nMaxRow, nMaxCol )
   WSay( 0, nMaxRow, 0, cStatus, "N/W*")
   Inkey( nTimeOut )                   // inkey() with timeout

                                       // restore screen
   WRestScreen( 0, nMaxRow, 0, nMaxRow, nMaxCol, cScreen )

   WSelect( nWin )                     // activate previously active window

RETURN
