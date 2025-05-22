//////////////////////////////////////////////////////////////////////
//
//  KBD_REC.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       XBToolsIII Keyboard recording Sample
//   
//////////////////////////////////////////////////////////////////////


#include "inkey.ch"
#include "xbtscan.ch"
#include "fileio.ch"


* *****************************************************************************
* Reset Keyread buffer and start keyboard recording.
* *****************************************************************************
PROCEDURE KbdStartRec()

  // clear keysend buffer by using keysend()
  KeySend ( KS_ESC )

  // clear message queue
  WHILE NextAppEvent() <> 0
    AppEvent(,,,0.1)
  ENDDO

RETURN


* *****************************************************************************
* End Keyboard recording
* - Store keyboard recording to file if filename is specified
* - Return keyboard recording as string
* *****************************************************************************
FUNCTION KbdEndRec( cFile, lAppend )

  // the keyboard recording buffer is the keyread() buffer which contains all
  // pressed keys since the last call of KeySend() (and KbdStartRec()).
  // It does not contain any mouse events, though.
  LOCAL cRec := KeyRead(), nFile

  // When a filename was spcified, save the keyboard recording to this file
  // with the given append mode.
  IF cFile <> NIL

     // The default value for the append mode is overwrite.
     IF lAppend == NIL
        lAppend := .F.
     ENDIF

     // Open the file and move to the EOF
     IF lAppend == .F.
        nFile := FCreate ( cFile, FC_NORMAL )    // always create file
     ELSE
                                                 // open file
        nFile := FOpen ( cFile, FO_READWRITE + FO_EXCLUSIVE )
        IF nFile == -1
           nFile := FCreate ( cFile, FC_NORMAL ) // create file if necessary
        ENDIF
        FSeek ( nFile, 0, FS_END )               // move to EOF
     ENDIF

     // Store keyboard recording to file
     FWrite ( nFile, cRec )

     FClose ( nFile )                            // close file

  ENDIF

RETURN cRec                                      // return keyboard recording


* *****************************************************************************
* Play keyboard recording using the given file name or data buffer with
* the given timer interval.
* *****************************************************************************
PROCEDURE KbdPlayRec( cData, nTimer, lIsFile )

  LOCAL cRec, nFile, nLRec

  // When cData specifies a file, load this file
  IF lIsFile <> NIL
     IF lIsFile
        nFile := FOpen ( cData, FO_READ )        // open file
        IF nFile == -1
           RETURN
        ENDIF
        nLRec := FSeek ( nFile, 0, FS_END )      // get file length by moving to EOF
        cRec  := SPACE ( nLRec )                 // initialize buffer string
        FSeek ( nFile, 0, FS_SET )               // move to BOF
        FRead ( nFile, @cRec, nLRec )            // load data
        FClose ( nFile )                         // close file
     ELSE
        cRec := cData                            // data was passed directly
     ENDIF
  ELSE
     cRec := cData                               // data was passed directly
  ENDIF

  // Now play the keyboard recording by using a different thread whith a 
  // timer.
  Thread():New():Start ( "_KbdPlayRec", cRec, nTimer )

RETURN


* *****************************************************************************
* Internal function to play the specified keyboard string
* *****************************************************************************
PROCEDURE _KbdPlayRec ( cRec, nTimer )

  LOCAL nPos := 1, lRec := LEN ( cRec )

  WHILE nPos < lRec

     // Write the current pos in the string to the applications message queue
     KeySend ( SUBSTR ( cRec, nPos, 2 ) )
     nPos += 2

     // Sleep some time
     Sleep ( nTimer )

  ENDDO

RETURN


* *****************************************************************************
* Test procedure
* *****************************************************************************
PROCEDURE Main()

   LOCAL cRec

   SET SCOREBOARD OFF

   // prepare screen
   CLS
   DispBox ( 0, 0, MAXROW(), MAXCOL(), "같같같같�" )
   DISPOUTAT ( 1, 1, "XBToolsIII Keyboard recording Sample" )
   DISPOUTAT ( 6, 1, "press any key to start keyboard recording" )

   // start recording
   KbdStartRec()

   // now invoke memoedit to test some keyboard input
   DISPOUTAT ( 3, 1, "Recording Keyboard input: press CTRL-W or ESC to proceed." )
   Memoedit( "", 5, 1, MAXROW() - 1, MAXCOL() - 1 )

   // end recording
   cRec := KbdEndRec( , .F. )

   // now I have to reset the INSERT switch as it was from the beginning of the
   // program
   SET ( _SET_INSERT, .F. )

   // the CTRL-W has been added the the end of the recording and must therefore
   // be removed
   cRec := SUBSTR ( cRec, 1, LEN ( cRec ) - 2 )

   // now watch what the user did when the F10 (start key was pressed)
   DISPOUTAT ( 3, 1, "Now press F10 play recorded input and press CTRL-W or ESC to exit." )
   SetKey ( K_F10, { || KbdPlayRec ( cRec, 5, .F. ) } )
   Memoedit( "", 5, 1, MAXROW() - 1, MAXCOL() - 1 )

RETURN

// EOF
