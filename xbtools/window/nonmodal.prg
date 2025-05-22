//////////////////////////////////////////////////////////////////////
//
//  NONMODAL.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       This program demonstrates nonmodal windows which can be
//       selected with a mouse click. Two windows are displayed:
//       Window 1 -> AChoice()   - lists PRG files
//       Window 2 -> MemoEdit()  - displays the selected PRG file contents
//   
//////////////////////////////////////////////////////////////////////


#include "Xbtwin.ch"
#include "Appevent.ch"

STATIC snFocusWin, snTextWin, snFileWin
STATIC saPrgFiles, scPrgText, scPrgFile

PROCEDURE Main

   // Create array with PRG file names
   saPrgFiles := Directory("*.PRG")

   IF Empty( saPrgFiles )
      CLS
      ?
      ? "No PRG files found"
      QUIT
   ENDIF


   AEval( saPrgFiles, {|x| x:=x[1] },,, .T. )
   scPrgFile := saPrgFiles[1]
   scPrgText := Memoread( scPrgFile )

   // Initialize XbTools window sytem.
   WInit()

   // Select root window and fill it
   WSelect(0)
   SetColor( "N/BG,W+/B" )
   CLS
   @ MaxRow(),0 SAY PadC("<Esc>-Change focus  <Alt+X>-Exit", MaxCol()+1) COLOR "W+/B"
   SET SCOREBOARD OFF

   // Protect last screen row from being overwritten by the following windows
   WBoard(0,0,MaxRow()-1,MaxCol())

   // Initial display of PRG file (text)
   snTextWin := WOpen( 4, 8, 22, 76, .T., "N/W,W/N", 0 )
   WBox(5)
   WSelect( snTextWin )
   MemoEdit( scPrgText, 0, 0, MaxRow(), MaxCol(), .F., .F., 80 )

   // Initial display of PRG files (array)
   snFileWin := WOpen( 1, 1, 12, 15, .T., "W+/N,W+/B", 0 )
   WBox(4)
   WSelect( snFileWin )
   AChoice( 0, 0, MaxRow(), MaxCol(), saPrgFiles, .F. )

   // The event code xbeXT_WSelect is created when the user
   // clicks an unselected window with the mouse
   SetAppEvent( xbeXT_WSelect, {|mp1,mp2| WinSetFocus( mp1, mp2 ) } )

   // Terminate the program with Alt+X
   SetAppEvent( xbeK_ALT_X   , {|| AppQuit() } )

   // Allow mouse selection
   WMouseSelect( .T. )
   WMode( .T., .T., .T., .T. )
   snFocusWin := snFileWin
   WinSetFocus( snFileWin )
RETURN

PROCEDURE AppQuit
   CLS
   QUIT
RETURN

PROCEDURE WinSetFocus( nNewWin )
   STATIC snRecursion := 0
   LOCAL nFile

   // Keep track of the recursion level
   // Recursion occurs when the user clicks an unselected window
   snRecursion ++

   DO WHILE .T.
      IF snRecursion == 1
         WSelect( snFocusWin )

         DO CASE
         CASE snFocusWin == snFileWin         
            nFile := AChoice( 0, 0, MaxRow(), MaxCol(), saPrgFiles )
            IF nFile > 0
               scPrgFile  := saPrgFiles[ nFile ]
            ENDIF
            snFocusWin := nNewWin := snTextWin

         CASE snFocusWin == snTextWin
            scPrgText := Memoread( scPrgFile )
            SetCursor(1)
            Memoedit( scPrgText, 0, 0, MaxRow(), MaxCol(), .T., , 80 )
            SetCursor(0)
            snFocusWin := nNewWin := snFileWin

         ENDCASE

      ELSEIF snFocusWin <> nNewWin
         // nNewWin is passed via xbeXT_WSelect event codeblock
         // Then terminate either Achoice() or MemoEdit()
         PostAppEvent( xbeK_ESC )
         snFocusWin := nNewWin
         EXIT
      ENDIF

   ENDDO

   snRecursion --
RETURN
