//////////////////////////////////////////////////////////////////////
//
//  MAILREAD.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       This file contains the class MailReadDialog() which provides
//       a simple dialog to read mails sent via NetBIOS session services.
//   
//  Remarks:
//       The file is part of NBMAIL.EXE -> eMail in a network via NetBIOS
//       It implements the READ part for eMails and requires the file
//       MAILSEND.PRG
//   
//       MAILREAD.PRG contains the MAIN procedure of NBMAIL.EXE
//   
//       NBMAIL.EXE is a data driven program that requires a .DAT file
//       which specifies all users participating in eMail
//   
//     Usage    :
//       You must specify your user ID and an ASCII file name on the command
//       line that contains user IDs and group names -> see MAIL.DAT
//   
//       Try this first:>  nbmail MJ MAIL.DAT
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"

#define CRLF                           Chr(13)+Chr(10)
#define BUFFER_SIZE                    4096

MEMVAR  lStopThread, oBusyBox

PROCEDURE AppSys()
   // We dont't need an application window here
RETURN


PROCEDURE DbeSys()
   // Neither do we need a database engine
RETURN


PROCEDURE AppQuit()
   QUIT
RETURN


PROCEDURE Main( cUserID, cSetUpFile )
   LOCAL nEvent, mp1, mp2, oXbp
   PUBLIC oBusyBox := NIL

   IF Empty( cUserID )
      Confirmbox( , "User ID not specified!" +  CRLF + ;
                    "Setup file not specified!" +  CRLF + ;
                    "Try :> nbmail MJ MAIL.DAT", ;
                    "FATAL ERROR", ;
                    XBPMB_CANCEL   )
      QUIT
   ENDIF

   IF Empty( cSetupFile )
      Confirmbox( , "Setup file not specified!" +  CRLF , ;
                    "FATAL ERROR", ;
                    XBPMB_CANCEL   )
      QUIT
   ELSEIF ! File( cSetupFile )
      Confirmbox( , "Setup file not found: " +cSetupFile +  CRLF , ;
                    "FATAL ERROR", ;
                    XBPMB_CANCEL   )
      QUIT
   ENDIF

   PUBLIC lStopThread := .F.

   oXbp := MailReadDialog():new( SetAppWindow() )
   oXbp:setupFile := cSetupFile
   oXbp:whoAmI    := Upper( cUserID )

   oXbp:create()
   oXbp:show()
   SetAppFocus( oXbp:mailList )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   IF NbNameNum( Upper(cUserID) ) != -1
      NbDelName( Upper(cUserID) )
   ENDIF

RETURN


CLASS MailReadDialog FROM XbpDialog
   PROTECTED:
      VAR oThread

   EXPORTED:
      VAR mailSendDialog READONLY
      VAR aMails         READONLY
      VAR staticText     READONLY
      VAR mailList       READONLY
      VAR buttonRead     READONLY
      VAR buttonSend     READONLY
      VAR buttonClear    READONLY
      VAR buttonCancel   READONLY
      VAR aUsers         READONLY
      VAR aGroups        READONLY
      VAR setupFile      
      VAR whoAmI
      VAR nConnection
      VAR newMessages

      METHOD init
      METHOD create
      METHOD destroy
      METHOD readMail
      METHOD sendMail
      METHOD clearMail
      METHOD keyHandler
ENDCLASS

******************************************************************************
* Initialiize dialog
******************************************************************************
METHOD MailReadDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO SetAppWindow(), ;
           aPos     TO {52,100}, ;
           aSize    TO {488,405}, ;
           lVisible TO .F.

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::staticText    := XbpStatic():new( ::drawingArea, , {40,334}, {398,26} )
   ::mailList      := XbpListBox():new( ::drawingArea, , {15,69}, {447,252} )
   ::buttonRead    := XbpPushButton():new( ::drawingArea, , {15,17}, {95,32} )
   ::buttonSend    := XbpPushButton():new( ::drawingArea, , {127,17}, {101,32} )
   ::buttonClear   := XbpPushButton():new( ::drawingArea, , {244,17}, {103,32} )
   ::buttonCancel  := XbpPushButton():new( ::drawingArea, , {362,17}, {103,32} )

   ::oThread       := Thread():new()
   ::aMails        := {}
   ::mailSendDialog:= MailSendDialog():new( oParent, self )
   ::newMessages   := 0
RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD MailReadDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL i, aData, cTitle

   aData := ReadSetupFile( ::setupFile, ::whoAmI, @cTitle )
   IF cTitle <> NIL
      cTitle += "'s Mail"
   ELSE
      cTitle := "Mail List Dialog"
   ENDIF

   ::aUsers                 := aData[1]
   ::aGroups                := aData[2]
   ::mailSendDialog:aUsers  := aData[1]
   ::mailSendDialog:aGroups := aData[2]
   ::mailSendDialog:whoAmI  := ::whoAmI

   ::XbpDialog:border       :=  XBPDLG_RAISEDBORDERTHICK_FIXED
   ::XbpDialog:icon         :=  1
   ::XbpDialog:taskList     := .T.
   ::XbpDialog:maxButton    := .F.
   ::XbpDialog:title        := cTitle
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, .F. )
   ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY   )
   ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )
   ::XbpDialog:drawingArea:keyboard := {|mp1| ::keyhandler( mp1 ) }
// Uncomment when you want to have the dialog window minimized at startup
*  ::XbpDialog:setFrameState( XBPDLG_FRAMESTAT_MINIMIZED )
   ::mailSendDialog:create()

   ::staticText:caption     := "No new messages"
   ::staticText:options     := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   ::staticText:create()
   ::staticText:setColorFG( GRA_CLR_RED )

   ::mailList:tabStop       := .T.
   ::mailList:create()
   ::mailList:itemSelected  := {|| ::readMail() }

   ::buttonRead:tabStop     := .T.
   ::buttonRead:caption     := "Read Mail"
   ::buttonRead:activate    := {|| ::readMail() }
   ::buttonRead:create()
   ::buttonRead:disable()

   ::buttonSend:tabStop     := .T.
   ::buttonSend:caption     := "Send Mail"
   ::buttonSend:activate    :=  {|| ::sendMail() }
   ::buttonSend:create()

   ::buttonClear:tabStop    := .T.
   ::buttonClear:caption    := "Clear List"
   ::buttonClear:activate   := {|| ::clearMail() }
   ::buttonClear:create()

   ::buttonCancel:tabStop   := .T.
   ::buttonCancel:caption   := "Cancel"
   ::buttonCancel:activate  := {|| ::destroy(), AppQuit() }
   ::buttonCancel:create()

   ::oThread:start( "_ListenForMail", self )
RETURN self


******************************************************************************
* Stop listen thread, destroy local NetBIOS name table, and destroy windows
******************************************************************************
METHOD MailReadDialog:destroy

   lStopThread := .T.                  // Stop listen thread 
   ::oThread:synchronize(0)

   ::mailSendDialog:destroy() 
   ::XbpDialog:destroy()

RETURN self


******************************************************************************
* Read selected mail -> Send dialog displays text in read only mode
******************************************************************************
METHOD MailReadDialog:readMail
   LOCAL aItem := ::mailList:getData()
   LOCAL cItem
   IF Empty( aItem )
      Tone( 1000 )
   ELSE
      cItem := ::mailList:getItem( aItem[1] )
      // Remove leading "*"
      cItem := " "+SubStr( cItem, 2 )
      ::mailList:setItem( aItem[1], cItem )

      ::newMessages := Max( 0, ::newMessages - 1 )
      IF ::newMessages == 0
         ::staticText:caption := "No new messages"
         ::staticText:configure()
      ENDIF

      ::mailSendDialog:setPos( ::currentPos(), .F. )
      ::mailSendDialog:mleMail:setData( ::aMails[ aItem[1] ] )
      ::mailSendDialog:mleMail:setFirstChar( 1 )
      ::mailSendDialog:show( .F. )
   ENDIF
RETURN self


******************************************************************************
* Clear previous mail in send dialog and display it
******************************************************************************
METHOD MailReadDialog:sendMail
   ::mailSendDialog:setPos( ::currentPos(), .F. )
   ::mailSendDialog:mleMail:clear()
   ::mailSendDialog:show( .T. )
RETURN self


******************************************************************************
* Clear mail list
******************************************************************************
METHOD MailReadDialog:clearMail
   ::buttonRead:disable()
   ::mailList:clear()
   ::aMails := {}
   ::newMessages := 0
   ::staticText:caption := "No new messages"
   ::staticText:configure()
RETURN self


******************************************************************************
* Handle xbeK_RETURN
******************************************************************************
METHOD MailReadDialog:keyHandler( nKey )
   LOCAL oXbp, i

   IF nKey == xbeK_RETURN
      i := AScan( ::drawingArea:childList(), SetAppFocus() )
      IF i > 0
         oXbp := ::drawingArea:childList()[i]
         IF "XBPPUSHBUTTON" == Upper( oXbp:className() ) 
            PostAppEvent( xbeP_Activate,,, oXbp )
         ELSEIF ! ::buttonRead:isEnabled()
            PostAppEvent( xbeP_Activate,,, ::buttonSend )
         ELSE
            PostAppEvent( xbeP_Activate,,, ::buttonRead )
         ENDIF
      ELSE
         PostAppEvent( xbeLB_ItemSelected,,, ::mailList )
      ENDIF 
   ENDIF
RETURN self


******************************************************************************
* Read setup data for mailing dialog (-> User and group information )
******************************************************************************
STATIC FUNCTION ReadSetupFile( cSetupFile, cWhoAmI, cMyName )
   LOCAL aUsers  := {{},{}}
   LOCAL aGroups := {{},{},{}}
   LOCAL aLines  := {}
   LOCAL cData   := MemoRead( cSetupFile )
   LOCAL n, i, imax, nPos
   LOCAL cID, cEntry, aList

   // Parse CRLF, ignore comments and empty lines
   TokenInit( @cData, CRLF )
   DO WHILE ! TokenEnd()
      cEntry := TokenNext( @cData )
      IF Asc( cEntry ) == 59 .OR. ;         // ";"
         Empty( cEntry )
         LOOP
      ENDIF
      AAdd( aLines, Alltrim(cEntry) )
   ENDDO

   // Get user data
   IF ( n := AScan( aLines, {|c| "[USERS]" == Upper( c ) } ) ) == 0
      Confirmbox( , "Error in setup file: " + cSetupFile + CRLF + ;
                    "Section [USERS] not found!", "FATAL ERROR", ;
                    XBPMB_CANCEL )
      QUIT
   ENDIF

   IF ! ReadSection( aLines, aUsers, n+1, cWhoAmI, @cMyName )
      Confirmbox( , "Invalid user ID: " + cWhoAmI + CRLF + ;
                    "User ID not found in setup file: "+ cSetupFile, ;
                    "FATAL ERROR", ;
                    XBPMB_CANCEL )
      QUIT
   ENDIF

   // Get group data
   IF ( n := AScan( aLines, {|c| "[GROUPS]" == Upper( c ) } ) ) == 0
      Confirmbox( , "Error in setup file: " + cSetupFile + CRLF + ;
                    "Section [GROUPS] not found!", "FATAL ERROR", ;
                    XBPMB_CANCEL )
      QUIT
   ENDIF
   ReadSection( aLines, aGroups, n+1 )

   // Extract group name and user IDs from group data
   imax := Len( aGroups[1] )  
   ASize( aGroups[3], imax )
   FOR i:=1 TO imax
      IF "ALL" == Upper( aGroups[1,i] )     // All user IDs belong 
         aGroups[3,i] := aUsers[1]          // to this group
         LOOP
      ENDIF
      cData := aGroups[2,i]
      aList := {}
      TokenInit( @cData, "," )
      aGroups[2,i] := TokenNext( @cData )   // Group name  
      DO WHILE ! TokenEnd()              
         AAdd( aList, ;                    
               Upper( TokenNext(@cData) ) ; // User ID
             )
      ENDDO
      aGroups[3,i] := aList
   NEXT

RETURN { aUsers, aGroups }


******************************************************************************
* Collect a single section from setup file in an array
******************************************************************************
STATIC FUNCTION ReadSection( aLines, aSection, nStart, cWhoAmI, cMyName )
   LOCAL cLine, cID, cEntry, nPos, i, imax, lOk := .F.

   imax := Len( aLines )
   FOR i := nStart TO imax
      cLine  := aLines[i]
      IF Asc( cLine ) == 91                 // "["
         EXIT
      ENDIF
      nPos   := At( "=", cLine )
      cID    := Upper( Trim( SubStr( cLine, 1, nPos-1 ) ) )
      cEntry := LTrim( SubStr(cLine, nPos+1) )
      IF  ! cID == cWhoAmI 
         AAdd( aSection[1], cID )
         AAdd( aSection[2], cEntry )
      ELSE
         cMyname := cEntry
         lOk := .T.
      ENDIF
   NEXT
RETURN lOk


******************************************************************************
* Listen for incoming mails
* Note: This procedure runs in a separate thread
******************************************************************************
PROCEDURE _ListenForMail( self )
   LOCAL lOk     := NbReset( 5, 5 )
   LOCAL cBuffer := "", cPacket

   IF oBusyBox == NIL 
      oBusyBox := XbpBusyBox():new( ::setParent(), self ):create()
   ENDIF
   oBusyBox:show( "Requesting session services..." )

   IF ! lOK
      cBuffer += "Error: NbReset()" + Chr(13)
   ENDIF
                                        // Add user ID to NetBIOS
   ::nConnection := NbAddName(::whoAmI) // name table
   IF ::nConnection == 0
      cBuffer += "Error: NbAddName() - " + ::whoAmI + Chr(13) + ;
                 "       NbError()   = "+ LTrim( Str(NbError()) ) + Chr(13)
   ELSE                                 // Listen for connection requests
      ::nConnection := NbSListCon( ::whoAmI, BUFFER_SIZE )
      IF ::nConnection  == 0
         cBuffer += "Error: NbSListCon() - " + ::whoAmI + Chr(13) + ;
                    "       NbError()   = "+ LTrim( Str(NbError()) )
      ENDIF
   ENDIF

   oBusyBox:hide()

   IF ! Empty( cBuffer )
      MsgBox( cBuffer )
      lStopThread := .T.                // Error occured, can't listen
   ENDIF

   DO WHILE ! lStopThread               // Run this thread until it is
                                        // interrupted by the main thread

      IF PpcRecCnt( ::nConnection ) > 0 // Something's waiting in the line
         cBuffer := ""                  // Read all until nothing comes in anymore
            IF ! Empty( cPacket := PpcRead( ::nConnection ) )
                cBuffer += cPacket
            ELSE
                Sleep(20)
            ENDIF

         AAdd( ::aMails, cBuffer )      // Store received mail
                                        // Put mail header to listbox
         ::mailList:addItem( CutStr( CRLF, cBuffer ) )
         ::mailList:setData( { ::mailList:numItems() }, .T. )
         ::buttonRead:enable()
         ::newMessages ++
         ::staticText:caption := "You have new messages"
         ::staticText:configure()
         cBuffer := NIL
         ::nConnection := NbSListCon( ::whoAmI, BUFFER_SIZE )
                                        // Display window in normal size
         ::setFrameState( XBPDLG_FRAMESTAT_NORMALIZED )
         SetAppFocus( ::mailList )
      ENDIF

      Sleep( 100 )                      // Nothing arrived, take a nap
   ENDDO

RETURN


******************************************************************************
* Parse a string
******************************************************************************
FUNCTION CutStr( cCut, cString )
LOCAL cLeftPart, i := At( cCut, cString )

   IF i > 0
      cLeftPart := Left( cString, i-1 )
      cString   := SubStr( cString, i + Len(cCut) )
   ELSE
      cLeftPart := cString
      cString   := ""
   ENDIF

RETURN cLeftPart

