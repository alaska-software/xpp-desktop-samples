//////////////////////////////////////////////////////////////////////
//
//  MAIL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      Demonstration how to use ReceiveMail() / SendMail()
//   
//  Remarks:
//      This sample sends an eMail to Alaska Software Technology AG
//      and an automated reply can be retrieved.
//   
//////////////////////////////////////////////////////////////////////



#include "Xbp.ch"
#include "Appevent.ch"

#define CRLF CHR(13)+CHR(10)

******************************************************************************
* Empty appsys
******************************************************************************
PROCEDURE AppSys()
RETURN

PROCEDURE MAIN()

  LOCAL nEvent, mp1 := {}, mp2 := {}, oXbp, aEditControls := {}
  LOCAL oDlg, oDraw, oDisplay
  LOCAL cSmtpHost := "Please insert your SMTP Host"
  LOCAL cPop3Host := "Please insert your POP3 Host"
  LOCAL cFromAdr  := "Please insert your eMailAddress"
  LOCAL cSubject  := "ASINet Sample for sending/receiving eMails"
  LOCAL cBody     := "Dear Alaska Software Team," + CRLF +                             ;
                     "Today's " + DToC( date() ) + CRLF +                              ;
                     "I found this sample using the AsiNet.Lib and I am " + CRLF +     ;
                     "impressed by all the possibilities Xbase++ and " + CRLF +        ;
                     "the Alaska Software Technologies are offering to me !" + CRLF +  ;
                     CRLF +                                                            ;
                     "Be inspired !"
  LOCAL aPbSize        := { 60,40 }

  oDlg                 := XbpDialog():new()
  oDlg:title           := "Send and Receive an eMail"
  oDlg:taskList        := .T.
  oDlg:create( ,, { 50,50 }, { 500,300 },,.F. )

  SetAppWindow( oDlg )

  oDraw                := oDlg:drawingArea

  oXbp                 := XbpStatic():new( oDraw, , { 90,140 }, { 380,110 } )
  oXbp:type            := XBPSTATIC_TYPE_RAISEDRECT
  oXbp:create()

  oXbp                 := XbpStatic():new( oDraw, , { 100,225 } )
  oXbp:autoSize        := .T.
  oXbp:caption         := "SMTP Host : "
  oXbp:create()

  oXbp                 := XbpSle():new( oDraw, , { 180,225 }, { 280,20 } )
  oXbp:dataLink        := {|x| IIF( x==NIL, cSmtpHost, cSmtpHost := x ) }
  oXbp:bufferLength    := 250
  oXbp:TabStop         := .T.
  oXbp:create()
  oXbp:setData()
  AAdd( aEditControls, oXbp )
  SetAppFocus( oXbp )

  oXbp                 := XbpStatic():new( oDraw, , { 100,200 } )
  oXbp:autoSize        := .T.
  oXbp:caption         := "POP3 Host : "
  oXbp:create()

  oXbp                 := XbpSle():new( oDraw, , { 180,200 }, { 280,20 } )
  oXbp:dataLink        := {|x| IIF( x==NIL, cPop3Host, cPop3Host := x ) }
  oXbp:bufferLength    := 250
  oXbp:TabStop         := .T.
  oXbp:create()
  oXbp:setData()
  AAdd( aEditControls, oXbp )

  oXbp                 := XbpStatic():new( oDraw, , { 100,175 } )
  oXbp:autoSize        := .T.
  oXbp:caption         := "From Adr : "
  oXbp:create()

  oXbp                 := XbpSle():new( oDraw, , { 180,175 }, { 280,20 } )
  oXbp:dataLink        := {|x| IIF( x==NIL, cFromAdr, cFromAdr := x ) }
  oXbp:bufferLength    := 250
  oXbp:TabStop         := .T.
  oXbp:create()
  oXbp:setData()
  AAdd( aEditControls, oXbp )

  oXbp                 := XbpStatic():new( oDraw, , { 100,150 } )
  oXbp:autoSize        := .T.
  oXbp:caption         := "Subject  : "
  oXbp:create()

  oXbp                 := XbpSle():new( oDraw, , { 180,150 }, { 280,20 } )
  oXbp:dataLink        := {|x| IIF( x==NIL, cSubject, cSubject := x ) }
  oXbp:bufferLength    := 250
  oXbp:editable        := .F.
  oXbp:TabStop         := .T.
  oXbp:create()
  oXbp:setData()
  AAdd( aEditControls, oXbp )

  oXbp                 := XbpStatic():new( oDraw, , { 90,10 }, { 380,110 } )
  oXbp:type            := XBPSTATIC_TYPE_RAISEDRECT
  oXbp:create()

  oDisplay             := XbpMle():new( oDraw, , { 95,15 }, { 370,100 } )
  oDisplay:horizScroll := .F.
  oDisplay:editable    := .F.
  oDisplay:create()
  oDisplay:setData( cBody )

  oXbp                 := XbpPushButton():new( oDraw, , { 10,210 }, aPbSize )
  oXbp:caption         := "Send"
  oXbp:activate        := {|| sendIt( aEditControls, oDisplay ) }
  oXbp:TabStop         := .T.
  oXbp:create()

  oXbp                 := XbpPushButton():new( oDraw, , { 10,110 }, aPbSize )
  oXbp:caption         := "Receive"
  oXbp:activate        := {|| GetIt( oDisplay, oDlg, AllTrim( aEditControls[2]:GetData() ) ) }
  oXbp:TabStop         := .T.
  oXbp:create()

  oXbp                 := XbpPushButton():new( oDraw, , { 10,10 }, aPbSize )
  oXbp:caption         := "Exit"
  oXbp:activate        := {|| nEvent := xbeP_Close }
  oXbp:TabStop         := .T.
  oXbp:create()

  oDlg:show()

  nEvent := 0
  // Event loop
  DO WHILE nEvent <> xbeP_Close
     nEvent := AppEvent( @mp1, @mp2, @oXbp )
     oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

RETURN

FUNCTION SendIt( aEc, oDisp )

   LOCAL lRet     := .T.
   LOCAL cHost    := AllTrim( aEc[1]:GetData() )
   LOCAL cFrom    := AllTrim( aEc[3]:GetData() )
   LOCAL cSubject := AllTrim( aEc[4]:GetData() )
   LOCAL acToAddr := {"asinet.mail.sample@alaska-software.com"}
   LOCAL cMessage := "This eMail has been sent from the mail sample from AsiNet.Lib " + CRLF + ;
                     "on " + cDoW( Date() ) + " at " + Time()
   LOCAL cLogFile := "email.log"
   LOCAL cSent    := CRLF + ;
                     "The eMail has been sent to Alaska Software!" + CRLF +                 ;
                     "An automated eMail response will be sent to you immediately" + CRLF + ;
                     "Please press the RECEIVE button to get it !"

    IF ! Empty( cHost )
       IF File( cLogFile )
          DELETE FILE ( cLogFile )
       ENDIF
       IF SendMail( cHost,       ;  // name of mail server (defaults to "mail")
                    cFrom,       ;  // mail address of sender
                    acToAddr,    ;  // recipient ( "ac" means: string or array of strings )
                    cSubject,    ;  // subject line
                    cMessage,    ;  // message body
                    ,            ;  // CC: recipients
                    ,            ;  // BCC: recipients
                    ,            ;  // name(s) of attached file(s)
                    cLogFile )      // log file name
          oDisp:setData( cSent )
       ELSE
          oDisp:setData( memoread( cLogFile ) )
          lRet := .F.
       ENDIF
    ELSE
       lRet := .F.
    ENDIF

RETURN lRet

FUNCTION GetIt( oDisp, oParent , cPop3Host )

   LOCAL oDlg, oDraw2, oXbp
   LOCAL lRet    := .T.
   LOCAL aPP     := { { XBP_PP_COMPOUNDNAME, "9.Arial Bold" } }
   LOCAL aPos    := { 100, 100 }
   LOCAL cUser   := "insert your user name"
   LOCAL cPwd    := "insert your password"
   LOCAL aECs    := {}

   oDlg          := XbpDialog():new( oParent, , aPos, { 200,150 } )
   oDlg:title    := "Login to eMail Server"
   oDlg:taskList := .F.
   oDlg:titleBar := .F.
   oDlg:create()
   oDraw2        := oDlg:drawingArea

   oXbp          := XbpStatic():new( oDraw2, , { 10, 120 }, , aPP )
   oXbp:autosize := .T.
   oXbp:caption  := "Please login to the mail server"
   oXbp:create()

   oXbp          := XbpStatic():new( oDraw2, , { 10, 90 } )
   oXbp:autosize := .T.
   oXbp:caption  := "User : "
   oXbp:create()

   oXbp          := XbpSle():new( oDraw2, ,{ 50,90 }, { 120,20 } )
   oXbp:TabStop  := .T.
   oXbp:datalink := { | x | IIF( x == NIL, cUser, cUser := x ) }
   oXbp:create()
   oXbp:setData()
   AAdd( aECs, oXbp )

   oXbp          := XbpStatic():new( oDraw2, , { 10, 60 } )
   oXbp:autosize := .T.
   oXbp:caption  := "Pwd : "
   oXbp:create()

   oXbp          := XbpSle():new( oDraw2, ,{ 50,60 }, { 120,20 } )
   oXbp:TabStop  := .T.
   oXbp:datalink := { | x | IIF( x == NIL, cPwd, cPwd := x ) }
   oXbp:create()
   oXbp:setData()
   AAdd( aECs, oXbp )

   oXbp          := XbpPushButton():new( oDraw2, , { 10, 10 }, { 40, 20 } )
   oXbp:caption  := "OK"
   oXbp:TabStop  := .T.
   oXbp:activate := {|| RecvMail( aECs, oDisp, cPop3Host ), oDlg:destroy() }
   oXbp:create()

   oXbp          := XbpPushButton():new( oDraw2, , { 150, 10 }, { 40, 20 } )
   oXbp:caption  := "Cancel"
   oXbp:TabStop  := .T.
   oXbp:activate := {|| oDlg:destroy() }
   oXbp:create()

RETURN lRet

FUNCTION RecvMail( aEc, oDisp, cMailServer )

   LOCAL lRet := .T.
   LOCAL cUser       := AllTrim( aEc[1]:getData() )
   LOCAL cPwd        := AllTrim( aEc[2]:getData() )
   LOCAL cLogFile    := "email.log"
   LOCAL aMail       := {}

   IF Empty( cUser ) .OR. Empty( cPwd )
      lRet := .F.
   ELSE
      IF File( cLogFile )
         DELETE FILE ( cLogFile )
      ENDIF
      aMail := ReceiveMail( cMailServer , ;   // name of mail server (defaults to "mail")
                            cUser       , ;   // user name for mail account
                            cPwd        , ;   // password to access mail account
                            .F.         , ;   // delete mails after retrieval?
                            1           , ;   // max. number of messages to retrieve
                                        , ;   // additional header fields to include
                            cLogFile      )   // log file name
      IF aMail == NIL
         aMail := {}
      ENDIF

      IF Len( aMail ) > 0
         oDisp:setData( aMail[1][5][1] )
      ELSE
         oDisp:setData( MemoRead( cLogFile ) )
      ENDIF
   ENDIF

RETURN lRet
