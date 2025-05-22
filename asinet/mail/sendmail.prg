//////////////////////////////////////////////////////////////////////
//
//  SENDMAIL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      User defined function SendMail()
//   
//  Remarks:
//      The function encapsulates object creation required for sending
//      e-mails to a mail server. The parameter profile is simplified
//      so that 90% of outgoing e-mails are covered by this function.
//   
//  Syntax:
//      MAIL
//   
//  Return:
//      Returnvalue is always NIL
//   
//////////////////////////////////////////////////////////////////////



#include "EMail.ch"


#pragma library( "ASINet10.lib" )



FUNCTION SendMail( cMailServer , ;  // name of mail server (defaults to "mail")
                   cFromAddr   , ;  // mail address of sender
                   acToAddr    , ;  // recipient ( "ac" means: string or array of strings )
                   cSubject    , ;  // subject line
                   cMessage    , ;  // message body
                   acCCAddr    , ;  // CC: recipients
                   acBCCAddr   , ;  // BCC: recipients
                   acAttachFile, ;  // name(s) of attached file(s)
                   cLogFile      ;  // log file name
                                 )  // Return --> .T. or .F. on error
   LOCAL oMimeMsg
   LOCAL oSmtpClient
   LOCAL oLog := LogWriter():new( cLogFile )


   /*
    * parameter checking
    */
   IF Valtype( cMailServer ) <> "C"
      cMailServer := "mail"
   ENDIF

   IF Valtype( cFromAddr ) <> "C"
      oLog:logError( ERR_MSG_SENDER )
   ENDIF

   IF Valtype( cSubject ) <> "C"
      oLog:logError( ERR_MSG_SUBJECT )
   ENDIF

   IF Valtype( cMessage ) <> "C"
      oLog:logError( ERR_MSG_MESSAGE )
   ENDIF

   IF ! ( ( Valtype( acToAddr ) == "A" .AND. ;
        AScan( acToAddr, {|x| Valtype(x) <> "C" } ) == 0 ) .OR. ;
        Valtype( acToAddr ) == "C" )

      oLog:logError( ERR_MSG_RECIPIENT )
   ENDIF

   IF ! ( ( Valtype( acCCAddr ) == "A" .AND. ;
        AScan( acCCAddr, {|x| Valtype(x) <> "C" } ) == 0 ) .OR. ;
        Valtype( acCCAddr ) == "C"  ) .AND. ;
        acCCAddr <> NIL

      oLog:logError( ERR_MSG_CCHEADER )
   ENDIF

   IF ! ( ( Valtype( acBCCAddr ) == "A" .AND. ;
        AScan( acBCCAddr, {|x| Valtype(x) <> "C" } ) == 0 ) .OR. ;
        Valtype( acBCCAddr ) == "C"  ) .AND. ;
        acBCCAddr <> NIL

      oLog:logError( ERR_MSG_BCCHEADER )
   ENDIF

   IF ! ( ( Valtype( acAttachFile ) == "A" .AND. ;
        AScan( acAttachFile, {|x| Valtype(x) <> "C" .OR. .NOT. FExists(x) } ) == 0 ) .OR. ;
        Valtype( acAttachFile ) == "C"  ) .AND. ;
        acAttachFile <> NIL

      oLog:logError( ERR_MSG_ATTACHMENT )
   ENDIF

   IF oLog:isError()
      IF Valtype( cLogFile ) == "C"
         oLog:writeLogFile()
      ENDIF
      RETURN .F.
   ENDIF


   /*
    * We're using arrays of strings
    */
   IF Valtype( acToAddr ) == "C"
      acToAddr := { acToAddr }
   ENDIF

   IF Valtype( acCCAddr ) == "C"
      acCCAddr := { acCCAddr }
   ENDIF

   IF Valtype( acBCCAddr ) == "C"
      acBCCAddr := { acBCCAddr }
   ENDIF

   IF Valtype( acAttachFile ) == "C"
      acAttachFile := { acAttachFile }
   ENDIF


   /*
    * create a new mime message object and compose e-mail
    */
   oMimeMsg := MimeMessage():new()

   /*
    * add sender and recipient(s)
    */
   oMimeMsg:setFrom( MailAddress():new( cFromAddr ) )
   AEval( acToAddr, {|cTo| oMimeMsg:addRecipient( MailAddress():new(cTo) ) } )

   /*
    * add subject line and message body
    */
   oMimeMsg:setSubject( cSubject )
   oMimeMsg:setMessage( cMessage )

   /*
    * add CC headers and BCC headers
    */
   IF Valtype( acCCAddr ) == "A"
      AEval( acCCAddr, {|cCC| oMimeMsg:addHeader( "CC", cCC ) } )
   ENDIF

   IF Valtype( acBCCAddr ) == "A"
      AEval( acBCCAddr, {|cBCC| oMimeMsg:addHeader( "BCC", cBCC ) } )
   ENDIF

   /*
    * add file attachments
    */
   IF Valtype( acAttachFile ) == "A"
      AEval( acAttachFile, {|cFile| oMimeMsg:attachFile( cFile ) } )
   ENDIF

   /*
    * create an SMTP client object and log full information (=2)
    */
   oSmtpClient := SMTPClient():new( cMailServer,,, oLog, 2 )

   /*
    * connect to SMTP server and send message
    */
   IF .NOT. oSmtpClient:connect()
      oLog:logError( ERR_MSG_CONNECTION )

   ELSE
      IF .NOT. oSmtpClient:send( oMimeMsg )
         oLog:logError( ERR_MSG_TRANSMISSION )
      ENDIF
      /*
       * disconnect from server
       */
      oSmtpClient:disconnect()
   ENDIF

   IF .NOT. oLog:isError()
      oLog:write( ERR_MSG_NOERROR )
   ENDIF

   IF Valtype( cLogFile ) == "C"
      oLog:writeLogFile()
   ENDIF

RETURN ( .NOT. oLog:isError() )

// EOF
