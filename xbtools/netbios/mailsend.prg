//////////////////////////////////////////////////////////////////////
//
//  MAILSEND.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       This file contains the class MailSendDialog() which provides
//       a simple dialog to send mail via NetBIOS session services.
//   
//  Remarks:
//       The program is part of NBMAIL.EXE -> See notes in MAILREAD.PRG
//   
//////////////////////////////////////////////////////////////////////



#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"

#define CRLF  Chr(13)+Chr(10)

MEMVAR  oBusyBox                       // PUBLIC declared in MAILREAD.PRG

CLASS MailSendDialog FROM XbpDialog
   PROTECTED:
      VAR lIsUser
      VAR cSenderID
   EXPORTED:
      VAR aUsers          
      VAR aGroups         
      VAR receiverList    READONLY
      VAR radioUser       READONLY
      VAR radioGroup      READONLY
      VAR mleMail         READONLY
      VAR buttonSend      READONLY
      VAR buttonClear     READONLY
      VAR buttonCancel    READONLY
      VAR sendMode        READONLY
      VAR whoAmI

      METHOD init         
      METHOD create
      METHOD show
      METHOD sendMail
      METHOD reply
      METHOD setUserList
      METHOD keyHandler
ENDCLASS


******************************************************************************
* Initialize Dialog 
******************************************************************************
METHOD MailSendDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO SetAppWindow():setParent(), ;
           aPos     TO {52,100}, ;
           aSize    TO {552,374}, ;
           lVisible TO .F.

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::receiverList := XbpCombobox():new( ::drawingArea, , {21,104}, {230,217} )
   ::radioUser    := XbpRadiobutton():new( ::drawingArea, , {288,297}, {120,24} )
   ::radioGroup   := XbpRadiobutton():new( ::drawingArea, , {427,297}, {120,24} )
   ::mleMail      := XbpMle():new( ::drawingArea, , {21,64}, {501,210} )
   ::buttonSend   := XbpPushButton():new( ::drawingArea, , {20,16}, {100,30} )
   ::buttonClear  := XbpPushButton():new( ::drawingArea, , {142,16}, {100,30} )
   ::buttonCancel := XbpPushButton():new( ::drawingArea, , {422,16}, {100,30} )
   ::lIsUser      := .T.
   ::sendMode     := .T.

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD MailSendDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:border      :=  XBPDLG_RAISEDBORDERTHICK_FIXED
   ::XbpDialog:icon        :=  1
   ::XbpDialog:taskList    := .T.
   ::XbpDialog:maxButton   := .F.
   ::XbpDialog:title       := "Mailing Dialog"
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, .F. )
   ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY )
   ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )
   ::XbpDialog:drawingArea:keyboard := {|mp1| ::keyhandler( mp1 ) }

   ::receiverList:tabStop  := .T.
   ::receiverList:create()
   ::receiverList:itemSelected := {|| SetAppFocus( ::mleMail ) }

   ::radioUser:tabStop     := .T.
   ::radioUser:caption     := "User mail"
   ::radioUser:selection   := .T.
   ::radioUser:selected    := {|| ::setUserList( .T. ) }
   ::radioUser:create()

   ::radioGroup:tabStop    := .T.
   ::radioGroup:caption    := "Group mail"
   ::radioGroup:selected   := {|| ::setUserList( .F. ) }
   ::radioGroup:create()

   ::mleMail:tabStop       := .T.
   ::mleMail:ignoreTab     := .T.
   ::mleMail:keyBoard      := {|mp1,mp2,obj| IIf( Empty( obj:editbuffer() ) , ;
                                                  ::buttonSend:disable()    , ;
                                                  ::buttonSend:enable()     ) }
   ::mleMail:create()

   ::buttonSend:tabStop    := .T.
   ::buttonSend:caption    := "Send mail"
   ::buttonSend:activate   := {|| ::sendMail() }
   ::buttonSend:create()
   ::buttonSend:disable()

   ::buttonClear:tabStop   := .T.
   ::buttonClear:caption   := "Clear mail"
   ::buttonClear:activate  := {|| ::mleMail:clear(), ::buttonSend:disable() }
   ::buttonClear:create()

   ::buttonCancel:tabStop  := .T.
   ::buttonCancel:caption  := "Cancel"
   ::buttonCancel:activate := {|| ::setOwner():enable()     , ;
                                  ::hide()                  , ;
                                  SetAppFocus( ::setOwner():mailList ) }
   ::buttonCancel:create()

RETURN self


******************************************************************************
* Display dialog and en/disable controls according to send/read mode
******************************************************************************
METHOD MailSendDialog:show( lSendMode )
   LOCAL cMessage, cString

   DEFAULT lSendMode TO .T.

   ::sendMode := lSendMode
 
   IF ::sendMode
      ::receiverList:enable()
      ::radioUser   :enable()
      ::buttonClear :enable()
      ::mleMail     :setEditable( .T. )
      ::buttonSend  :caption := "Send mail"
      ::buttonSend  :configure()

      IF Empty( ::aGroups )
        ::lIsUser := .T.
        ::radioGroup:disable()
      ELSE
        ::radioGroup:enable()
      ENDIF

      IF ! Empty( ::mleMail:editBuffer() )
         ::buttonSend:enable()
      ELSE
         ::buttonSend:disable()
      ENDIF

   ELSE
      ::cSenderID := NIL
      cMessage    := ::mleMail:getData()

      IF At( "*  Message from:", cMessage ) == 1
         // Message was received from this program running in
         // another process. Get sender ID and enter Reply mode
         cString := cMessage
         CutStr( ":", @cString )
         ::cSenderID :=  AllTrim( CutStr( "  Date:", @cString ) )
         ::reply()
      ELSE
         // I received mail from another program calling my user ID!
         // View mail and disable everything except Cancel button 
         ::receiverList:disable()
         ::radioUser   :disable()
         ::radioGroup  :disable()
         ::buttonSend  :disable()
         ::buttonClear :disable()
         ::mleMail     :setEditable( .F. )
      ENDIF
   ENDIF


   ::setUserList( ::lIsUser )
   ::setOwner():disable()
   ::XbpDialog:show()
   SetAppFocus( ::mleMail )
RETURN self


******************************************************************************
* Send mail to selected user or group
******************************************************************************
METHOD MailSendDialog:sendMail
   LOCAL nHandle, nRest, aID, aName, i, imax, j
   LOCAL aItem   := ::receiverList:getData()
   LOCAL cText   := ::mleMail:getData()
   LOCAL aFailed := {}

   IF Empty( aItem )
      MsgBox( "Message receiver not specified!", "ERROR" )
      RETURN self
   ENDIF

   IF ::lIsUser
      aID   := { ::aUsers[1, aItem[1] ] }
      aName := { ::aUsers[2, aItem[1] ] }
   ELSE
      aID   := ::aGroups[3, aItem[1] ]
      imax  := Len( aID )
      aName := Array( imax )           // Get user names from user IDs

      FOR i := 1 TO imax
         j := AScan( ::aUsers[1], aID[i] )
         IF j > 0
            aName[i] := ::aUsers[2,j]  // This is the user name as defined
         ELSE                          // in the .DAT configuration file
            aName[i] := aID[i]
         ENDIF
      NEXT
   ENDIF
                                       // Display busy box until mail
   oBusyBox:setOwner( self )           // is sent to all recipients
   imax     := Len( aID )
   cText    := "*  Message from: " + ::whoAmI + ;
                "  Date: " + DtoC( Date() )    + ;
                "  Time: "+Time() + CRLF + cText

   FOR i := 1 TO imax
                                       // Note: oBusyBox is PUBLIC
      oBusyBox:show( "Sending mail to: " + aName[i] )
                                       // Establish NetBIOS session
      nHandle := NbSCall( ::whoAmI,,, aID[i] )

      IF nHandle == 0             
         AAdd( aFailed, aName[i] )     // This recipient did not answer
         LOOP
      ENDIF
                                       // Make sure NetBIOS is ready
      Sleep( 20 )                      // Normally this Sleep() is not
                                       // required. But things happen...
      nRest := Len( cText )
      DO WHILE nRest > 0               // Send the mail
         nRest := PpcWrite( nHandle, Right( cText, nRest ) ) 
         Sleep(20)
      ENDDO

      DO WHILE PpcSndCnt( nHandle ) > 0
         Sleep( 20 )                   // Wait until send buffer is empty
      ENDDO
      Sleep(100)
      PpcCancel( nHandle )             // Disconnect
   NEXT

   oBusyBox:hide()

   IF ! Empty( aFailed )               // Display user names for
      cText := ""                      // all failed connections
      AEval( aFailed, {|c| cText += c + Chr(13) } )
      MsgBox( cText , "The following persons did not receive your message" )
   ENDIF

RETURN self


******************************************************************************
* Reply to current mail
******************************************************************************
METHOD MailSendDialog:reply
   LOCAL i

   IF Empty( ::cSenderID )             // Don't know where the mail
      RETURN self                      // comes from ** RETURN **
   ENDIF

   i := AScan( ::aUsers[1], ::cSenderID )
   IF i == 0                           // Sender unknown
      RETURN self                      // ** RETURN **
   ENDIF

   ::setUserList( .T. )                // Display users, not groups
   ::receiverList:setData( {i}, .T. )  // Display sender in combobox

   ::receiverList:disable()

   ::radioUser  :setdata( .T. )
   ::radioUser  :disable()
   ::radioGroup :disable()
   ::buttonSend :enable()
   ::buttonSend :caption := "Reply"
   ::buttonSend :configure()
   ::buttonClear:enable()
   ::mleMail    :setEditable( .T. )

RETURN self


******************************************************************************
* Display users or groups in combobox
******************************************************************************
METHOD MailSendDialog:setUserList( lIsUser )
   LOCAL aArray

   DEFAULT lIsUser TO .T.

   IF ::lIsUser == lIsUser .AND. ::receiverList:numItems() > 0
      RETURN self                      // Nothing to do  ** RETURN **
   ENDIF

   IF lIsUser
      aArray := ::aUsers[2]            // User names
   ELSE
      aArray := ::aGroups[2]           // Group names
   ENDIF

   ::receiverList:clear()
   AEval( aArray, {|c| ::receiverList:addItem( c ) } )
   IF ! Empty( aArray )
     // ::receiverList:XbpSle:setData( aArray[1] )
     ::receiverList:SetData( { 1 } )
   ENDIF
   ::lIsUser := lIsUser
RETURN self


******************************************************************************
* Handle xbeK_RETURN and xbeK_ESC
******************************************************************************
METHOD MailSendDialog:keyHandler( nKey )
   LOCAL oXbp, i

   IF nKey == xbeK_RETURN
      i := AScan( ::drawingArea:childList(), SetAppFocus() )
      IF i > 0
         oXbp := ::drawingArea:childList()[i]
         IF "XBPPUSHBUTTON" == Upper( oXbp:className() ) 
            PostAppEvent( xbeP_Activate,,, oXbp )
         ENDIF
      ENDIF
   ELSEIF nKey == xbeK_ESC
      PostAppEvent( xbeP_Activate,,, ::buttonCancel )
   ENDIF

RETURN self

