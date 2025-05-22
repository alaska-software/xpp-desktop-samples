//////////////////////////////////////////////////////////////////////
//
//  COMLINK.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       This program contains a file send/receive utility to exchange
//       files between workstations which are connected with a serial
//       null-modem cable.
//   
//////////////////////////////////////////////////////////////////////



#include "common.ch"
#include "appevent.ch"
#include "xbp.ch"
#include "gra.ch"
#include "transmit.ch"

#pragma Library( "xppui2.lib" )

STATIC soDlg, soDesktop

MEMVAR lAutoReceive

// Create main application dialog.
PROCEDURE AppSys
   soDeskTop := SetAppWindow()

   soDlg := MainDialog():new( soDeskTop, soDeskTop, {32, 32}, {460, 200}, , .F.)
   soDlg:taskList:= .T.
   soDlg:create()

   SetAppWindow( soDlg )
   SetAppFocus( soDlg )
RETURN


PROCEDURE DbeSys
// We don't need any databases for this application.
RETURN


******************************************************************************
* Main event loop.
******************************************************************************
PROCEDURE Main()
   LOCAL nEvent, mp1, mp2, oXbp

   PUBLIC lAutoReceive := .T.          // Use PUBLIC variable for thread
                                       // synchronization. Auto receive
                                       // thread runs as long as this
                                       // variable contains .T.

   soDlg:receive()                     // Start auto receiver
   DO WHILE nEvent <> xbeP_Close
       nEvent := AppEvent( @mp1, @mp2, @oXbp )
       oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN


******************************************************************************
* MainDialog class.
******************************************************************************
CLASS MainDialog FROM XbpDialog
   EXPORTED:
      VAR oMenu                        // Menubar object
      VAR oFileSelect                  // FileSelect dialog
      VAR oConfigDialog                // Configuration dialog
      VAR oProgress                    // Progress box
      VAR oReceiveThread               // Auto receive thread

      METHOD init
      METHOD create
      METHOD send
      METHOD receive
ENDCLASS


******************************************************************************
* Initialize MainDialog.
******************************************************************************
METHOD MainDialog:init(oParent, oOwner, aPos, aSize, aPresParam, lVisible)

    ::XbpDialog:init(oParent, oOwner, aPos, aSize, aPresParam, lVisible)

    ::oReceiveThread := Thread():new()
    ::oFileSelect    := XbpFiledialog():new(::drawingArea, ::drawingArea)
    ::oProgress      := ProgressBox()  :new(::drawingArea, ::drawingArea, {6,6}, {240, 72})
    ::oConfigDialog  := ConfigDialog() :new( soDeskTop, soDeskTop )
    ::oConfigDialog:oRootDlg := self

RETURN


******************************************************************************
* Request system resources for MainDialog.
******************************************************************************
METHOD MainDialog:create(oParent, oOwner, aPos, aSize, aPresParam, lVisible)
   LOCAL aItems, oMenuBar

   ::XbpDialog:title := "Xbase++ File Send/Receive Utility"
   ::XbpDialog:icon  := ID_APPNAME
   ::XbpDialog:create(oParent, oOwner, aPos, aSize, aPresParam, lVisible)

   ::drawingArea :SetColorBG( GRA_CLR_WHITE )
   ::drawingArea :SetFontCompoundName( "8.Helv.normal" )

   ::oFileSelect:create()
   ::oProgress  :create()

   aItems := { ;
       { "~Configure", {|| ::oConfigDialog:popup(::currentPos()) }, 0, 0 }, ;
       { "~Send File", {|| ::send() }                             , 0, 0 }, ;
       {             ,                     , XBPMENUBAR_MIS_SEPARATOR, 0 }, ;
       { "~Exit"     , {|| PostAppEvent(xbeP_Close) }             , 0, 0 }  }
   ::oMenu := MenuNew( ::drawingArea, "~File", aItems )


   oMenuBar := ::MenuBar()
   oMenuBar:SetFontCompoundName( "8.Helv.normal" )
   oMenuBar:addItem( { ::oMenu, NIL } )

   ::oConfigDialog:create()

   ::show()
RETURN


******************************************************************************
* Send a file.
******************************************************************************
METHOD MainDialog:send()
   LOCAL cFile, oConfig, bProgress, nErrorCode

   ::oFileSelect:title := "Select file to send"
   cFile               := ::oFileSelect:open( "*" )

   IF cFile == NIL                     // No file selected
      RETURN self                      // ** RETURN **
   ENDIF

   StopReceiver()                      // Stop auto receive thread.
   SetAppFocus ( ::oProgress )         // Set focus to progress window

   Message( "Sending file: " + FileName( cFile ), 1 )
   oConfig    := ::oConfigDialog
   bProgress  := {|nMax, nCurrent| ::oProgress:showProgress( nMax, nCurrent ) }

   nErrorCode := ;                     // Start sending file
      SendFile( cFile             , ;
                oConfig:nComPort  , ;
                oConfig:nBaudRate , ;
                oConfig:cParity   , ;
                oConfig:nDataBits , ;
                oConfig:nStopBits , ;
                oConfig:lUseCRC   , ;
                bProgress           )

   IF nErrorCode != SENDER_OK          // Something went wrong
      MsgBox( "Error "+Str(nErrorCode,2)+" while sending file" )
   ENDIF

   ::oProgress:reset()                 // Reset progress bar and
   ::receive()                         // start auto receive thread again
RETURN


******************************************************************************
* Start file receiver.
******************************************************************************
METHOD MainDialog:receive()
   LOCAL oConfig, bProgress, bFileExist

   oConfig    := ::oConfigDialog
   bProgress  := {|nMax, nCurrent| ::oProgress:showProgress(nMax, nCurrent)}

   IF oConfig:lPromptUser
      bFileExist := {|cFile| MsgBox("File "+cFile+" already exists!"),cFile}
   ENDIF

   Message( "Auto-Receive-Mode" , 1 )
   Message( "Starting Receiver" )

   SetAppFocus( soDlg:oProgress )

   lAutoReceive := .T.                 // Let receive thread run.
   ::oReceiveThread:start( "AutoReceiveFile", ;
                           "lAutoReceive"   , ;
                           oConfig:nComPort , ;
                           oConfig:nBaudRate, ;
                           oConfig:cParity  , ;
                           oConfig:nDataBits, ;
                           oConfig:nStopBits, ;
                           oConfig:lUseCRC  , ;
                           bProgress        , ;
                           bFileExist         )

   Message( "Receiver started" )
RETURN


******************************************************************************
* Stop receive thread.
******************************************************************************
PROCEDURE stopReceiver

   Message( "Stopping receiver ..." )

   lAutoReceive := .F.                 // Set sync variable and wait
   soDlg:oReceiveThread:synchronize(0) // for receive thread to stop.

   Message( "Receiver stopped" )
   soDlg:oProgress:reset()             // Reset progress bar
RETURN


******************************************************************************
* Some utility functions...
******************************************************************************
// Set captions in progress box.
PROCEDURE Message( cString, nLine )
   IF nLine == 1
      soDlg:oProgress:Mess1Text:setcaption( cString )
   ELSE
      soDlg:oProgress:Mess2Text:setcaption( cString )
   ENDIF
RETURN

// Reset progress box to initial state.
PROCEDURE ResetProgress
   soDlg:oProgress:reset()
RETURN

// Create a menu and add items in <aItems>.
STATIC FUNCTION MenuNew( oParent, cTitle, aItems, bActivateBlock )
   LOCAL oMenu, i

   // create new menu
   oMenu := XbpMenu():new(oParent)
   oMenu:create()
   oMenu:SetFontCompoundName( "8.Helv.normal" )

   oMenu:title := cTitle

   // Add items
   IF aItems != NIL
      FOR i := 1 TO Len( aItems )
         IF ValType( aItems[i,2] ) == "A"
            oMenu:addItem( {MenuNew(oMenu, aItems[i,1], aItems[i,2]), , 0, 0} )
         ELSE
            oMenu:addItem( aItems[i] )
         ENDIF
      NEXT
   ENDIF

   // set activate codeblock
   oMenu:activateItem := bActivateBlock
RETURN oMenu

