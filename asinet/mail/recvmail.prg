//////////////////////////////////////////////////////////////////////
//
//  RECVMAIL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      User defined function ReceiveMail()
//   
//  Remarks:
//      The function encapsulates object creation required for retrieving
//      e-mails from a mail server. The parameter profile is simplified
//      so that 90% of incoming e-mails are covered by this function.
//   
//  Return:
//      ReceiveMail() returns a multi-dimensional array holding e-mail information
//      Elements of this array can be accessed using constants from EMAIL.CH
//   
//      aEmail[n, EMAIL_FROM      ] -> "C" sender address
//      aEmail[n, EMAIL_SUBJECT   ] -> "C" subject line
//      aEmail[n, EMAIL_CC        ] -> "C" CC: header
//      aEmail[n, EMAIL_BCC       ] -> "C" BCC: header"
//      aEmail[n, EMAIL_CONTENTS  ] -> "A" message body
//      aEmail[n, EMAIL_ATTACHMENT] -> "A" names of attached files
//   
//   
//////////////////////////////////////////////////////////////////////



#include "EMail.ch"
#include "FileIO.CH"

#pragma library( "ASINet10.lib" )


FUNCTION ReceiveMail( cMailServer , ;  // name of mail server (defaults to "mail")
                      cUsername   , ;  // user name for mail account
                      cPassword   , ;  // password to access mail account
                      lDeleteMail , ;  // delete mails after retrieval?
                      nHowmany    , ;  // max. number of messages to retrieve
                      acHeader    , ;  // additional header fields to include
                      cLogFile      ;  // log file name
                                    )  // Return --> Array or NIL on error
   LOCAL oPop3, oMimeMsg, xTemp
   LOCAL aAllMails, aEMail, i, imax
   LOCAL oLog := LogWriter():new( cLogFile )

   /*
    * parameter checking
    */
   IF Valtype( cMailServer ) <> "C"
      cMailServer := "mail"
   ENDIF

   IF Valtype( cUsername ) <> "C"
      oLog:logError( ERR_MSG_USERNAME )
   ENDIF

   IF Valtype( cPassword ) <> "C"
      oLog:logError( ERR_MSG_PASSWORD )
   ENDIF

   IF Valtype( lDeleteMail ) <> "L"
      lDeleteMail := .F.
   ENDIF


   /*
    * create a POP3 client and connect to mail server
    */
   oPop3 := POP3Client():new( cMailServer,, cUsername, cPassword, oLog, 2 )

   IF .NOT. oPop3:connect()
      oLog:logError( ERR_MSG_CONNECTION )
   ENDIF

   IF oLog:isError()
      oPop3:disconnect()

      IF Valtype( cLogFile ) == "C"
         oLog:writeLogFile()
      ENDIF
      RETURN NIL
   ENDIF


   /*
    * get number of mails to retrieve
    */
   imax := oPop3:getNumberOfNewMessages()
   IF Valtype( nHowmany ) <> "N"
      nHowMany := imax
   ENDIF

   imax := Max( 0, Min( nHowMany, imax ) )

   IF imax < 1
      /*
       * no mails to retrieve
       */
      oPop3:disconnect()
      oLog:write( ERR_MSG_NOERROR )

      IF Valtype( cLogFile ) == "C"
         oLog:writeLogFile()
      ENDIF
      RETURN {}
   ENDIF


   /*
    * retrieve individual mails
    */
   oLog:write( "Retrieving " + Var2Char( imax ) + " e-mails." )
   aAllMails := {}

   FOR i := 1 TO imax
      oMimeMsg := oPop3:getMessage( i )

      IF oMimeMsg != NIL
         aEMail := EMAIL_ARRAY
         AAdd( aAllMails, aEMail )

         // Get MailAddress object holding sender information
         xTemp := oMimeMsg:getFrom()
         IF xTemp != NIL
            aEMail[ EMAIL_FROM ] := xTemp:getString()
         ELSE
            aEMail[ EMAIL_FROM ] := "No FROM!"
         ENDIF

         // Get subject line
         xTemp := oMimeMsg:getSubject()
         IF xTemp != NIL
            aEMail[ EMAIL_SUBJECT ] := xTemp
         ELSE
            aEMail[ EMAIL_SUBJECT ] := "No SUBJECT!"
         ENDIF

         // Get CC header
         xTemp := oMimeMsg:getHeader( "CC" )
         IF xTemp != NIL
            aEMail[ EMAIL_CC ] := xTemp
         ELSE
            aEMail[ EMAIL_CC ] := ""
         ENDIF

         // Get BCC header
         xTemp := oMimeMsg:getHeader( "BCC" )
         IF xTemp != NIL
            aEMail[ EMAIL_BCC ] := xTemp
         ELSE
            aEMail[ EMAIL_BCC ] := ""
         ENDIF

         // Get mail content
         xTemp := oMimeMsg:getContent()
         IF xTemp != NIL
            GetMimeContent( aEMail, xTemp )
         ENDIF
      ENDIF

      /*
       * delete message from server if requested
       */
      IF lDeleteMail
         oPop3:deleteMessage( i )
      ENDIF
   NEXT

   /*
    * disconnect from server
    */
   oPop3:disconnect()

   IF .NOT. oLog:isError()
      oLog:write( ERR_MSG_NOERROR )
   ENDIF

   IF Valtype( cLogFile ) == "C"
      oLog:writeLogFile()
   ENDIF

RETURN aAllMails


/*
 * get content of mail recursively
 */
STATIC PROCEDURE GetMimeContent( aEMail, aContent )
   LOCAL i, imax, oMimeContent, xTemp, nHandle

   IF ValType( aContent ) != "A"
      RETURN
   ENDIF

   imax := Len( aContent )

   FOR i := 1 TO imax
      oMimeContent := aContent[i]

      IF oMimeContent:isMultiPart()
         /*
          * get every part of a multipart e-mail
          */
         GetMimeContent( aEMail, oMimeContent:getContent() )
         RETURN
      ENDIF

      /*
       * if part has a filename, create that file and
       * write its contents to disk
       */
      xTemp := oMimeContent:getFileName()

      IF xTemp != NIL
         nHandle := FCreate( xTemp, FC_NORMAL )
         IF nHandle != -1
            FWrite( nHandle, oMimeContent:getMessage() )
            FClose( nHandle )
            AAdd( aEMail[ EMAIL_ATTACHMENT ], xTemp )
         ENDIF
      ENDIF

      /*
       * we add only parts of content type "text/xxx"
       */
      IF "text/" $ Lower( oMimeContent:getContentType() )
         AAdd( aEMail[ EMAIL_CONTENTS ], oMimeContent:getMessage() )
      ENDIF
   NEXT

RETURN

// EOF
