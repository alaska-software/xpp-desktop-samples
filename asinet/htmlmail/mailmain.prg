//////////////////////////////////////////////////////////////////////
//
//  MAILMAIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//      Htmlmail sample
//   
//  Remarks:
//      Sample application that demonstrates how html e-mails can
//      be composed and sent via internet.
//      This sample uses the Html Reflection Framework (HRF), thus
//      HRF must be installed prior the sample can be build and
//      executed.
//   
//////////////////////////////////////////////////////////////////////


#include "appevent.ch"
#include "asinet.ch"
#include "common.ch"

#pragma Library( "asiutl10.lib" )
#pragma Library( "hrfclass.lib" )

#define TEXTSOURCE "txtsrc.txt"
#define HTMLSOURCE "htmlsrc.htm"
#define HTMLINTRO  "intro.htm"

/////////////////////////////////////////////////////////////////////
///
/// Overload AppSys and DbeSys
///
/////////////////////////////////////////////////////////////////////
PROCEDURE AppSys()
RETURN
PROCEDURE DbeSys()
RETURN


/////////////////////////////////////////////////////////////////////
///
/// Sample to demonstrate how to load a html page and send it as
/// html email.
///
/// Optionally all relevant data for sending e-mail can be passed
/// by parameters. In that case those values will be used to
/// initialize the Dialog.
///
/////////////////////////////////////////////////////////////////////
PROCEDURE main( cEmailAdr, cServer, cUsername, cPassword )
  LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL
  LOCAL oDlg

  //
  // Initialize all data
  //
  DEFAULT cEmailAdr    TO    ""
  DEFAULT cServer      TO    ""
  DEFAULT cUsername    TO    ""
  DEFAULT cPassword    TO    ""

  //
  // Set callback for dialog button.
  // Initilize the contained XbpSles
  // Create the dialog
  //
  oDlg := MailSample():new()
  oDlg:oButtonSendmail:activate := {|| SendMail( oDlg ) }
  oDlg:create()

  oDlg:oEditEmail:setData( cEmailAdr )
  oDlg:oEditServer:setData( cServer )
  oDlg:oEditUsername:setData( cUsername )
  oDlg:oEditPassword:setData( cPassword )

  oDlg:oHtmlViewer:setHtml( MemoRead( HTMLINTRO ) )

  SetAppWindow( oDlg )
  SetAppFocus( oDlg )

  nEvent := 0
  DO WHILE nEvent <> xbeP_Close
     nEvent := AppEvent( @mp1, @mp2, @oXbp )
     oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO


RETURN


/////////////////////////////////////////////////////////////////////
///
/// Send the MIMEMessage that is passed by second parameter.
/// oDlg serves as LogWriter
///
/////////////////////////////////////////////////////////////////////
FUNCTION CreateMessage( cEmailAdr )
  LOCAL cTmp, oTmp, oMailMsg, aImgMimeContent, oHrfDoc
  LOCAL oContHtml, oHtmlViewer


  //
  // prepare MimeMessage
  // The Html mail will consist of two parts. A text part (text/plain)
  // and a html part (text/html). This way also mailreaders that are not
  // able to display html content can visualize the mail.
  //
  cTmp     := MemoRead( TEXTSOURCE )
  cTmp     := StrTran( cTmp, "[MAILADDRESS]", cEmailAdr )
  oMailMsg := MIMEMessage():encodeQuotedPrintable( cTmp, "text/plain" )

  //
  // load Html file file and create MimeContent
  //
  oHrfDoc := HtmlDocument():loadFile( HTMLSOURCE )
  oTmp := oHrfDoc:getElementById( "MAILADDRESS" )
  oTmp:setContent( "To " + cEmailAdr)

  // images that are referenced from the html code will be
  // added as related content to the html part
  aImgMimeContent := CreateRelatedImage( oHrfDoc:images )

  // Create the html content with quoted printable encoding
  oContHtml := MimeContent():encodeQuotedPrintable( oHrfDoc:toHtml( 0 ), ;
                                                    "text/html" )

  // attach the images as related content
  AEval( aImgMimeContent, { |o| oContHtml:attachRelated( o ) } )

  // the html mail is alternative content of the text mail
  oMailMsg:attachAlternative( oContHtml )

  // Visualize the html mail
  oHtmlViewer := SetAppWindow():oHtmlViewer
  oHtmlViewer:setHtml( oHrfDoc:toHtml() )

RETURN oMailMsg

/////////////////////////////////////////////////////////////////////
///
/// Send the MIMEMessage that is passed by second parameter.
/// oDlg serves as LogWriter
///
/////////////////////////////////////////////////////////////////////
PROCEDURE SendMail()
  LOCAL oRecipient, oSender, oSmtp, oDlg, oMime
  LOCAL cEmail, cServer, cUsername, cPassword

  oDlg := SetAppWindow()

  cEmail    := oDlg:oEditEmail:getData()
  cServer   := oDlg:oEditServer:getData()
  cUsername := oDlg:oEditUsername:getData()
  cPassword := oDlg:oEditPassword:getData()

  //
  // Check for required field
  //
  IF Empty( cEmail )
    MsgBox( "Email must be set!", "Error" )
    RETURN
  ENDIF
  IF Empty( cServer )
    MsgBox( "Server must be set!", "Error" )
    RETURN
  ENDIF

  // Sender and recipient are identical
  oRecipient := MAILAddress():new( cEmail )
  oSender    := MAILAddress():new( cEmail )

  //
  // Initialize MIMEMessage
  //
  oMime := CreateMessage( cEmail )

  //
  // Prepare MIMEMessage for mail transmission
  //
  oMime:setFrom( oSender )
  oMime:addRecipient( oRecipient  )
  oMime:setSubject( "HTML Testmail" )

  //
  // Clear the area where the log is written to
  //
  oDlg:oListBoxOutput:clear()

  //
  // Prepare SMTPClient and send message
  //
  oSMTP := SMTPClient():new( cServer, , , oDlg, 3 )

  IF ! oSmtp:connect( cUsername, cPassword )
    MsgBox( "SmtpClient:connect() failed", "Error" )
    RETURN
  ENDIF

  oSmtp:send( oMime )
  oSmtp:disconnect()

RETURN


/////////////////////////////////////////////////////////////////////
///
///  Create one MimeContent object for each Html img link
///  Modify the img link in the html source accordingly
///
/////////////////////////////////////////////////////////////////////
FUNCTION CreateRelatedImage( aImgList )
  LOCAL cImgSource, nLen, cID, n, oImg
  LOCAL aImgContent := {}
  LOCAL oHtmlImage

  //
  // Repeat for all images.
  // After extracting the filepath:
  //   (A) Modify link in Html source
  //   (B) Create a new MimeContent object with the picture as
  //       base64 encoded string and add additional header with
  //       a reference
  //

  nLen := Len( aImgList )
  FOR n := 1 TO nLen

    // We need the html image object and an id to link the
    // html source to this image
    oHtmlImage := aImgList[n]

    // Extract Image Filepath
    cImgSource := oHtmlImage:src

    // Create a mime object that serves as a part in the
    // resulting html mail.
    oImg := MimeMessage():createFromFile( cImgSource )

    // The html image tag will refer to an image mime part by
    // a unique identifier
    cID := oImg:uniqueString

    // Replace source with reference to inline source:
    // <img src="cid:12345">
    oHtmlImage:setAttribute( "src", "cid:" + cID )

    // The mime part requires a header to be properly referenced:
    // Content-ID: <12345>
    oImg:addHeader( "Content-ID", "<" + cID + ">" )

    AAdd( aImgContent, oImg )

  NEXT

RETURN aImgContent


/////////////////////////////////////////////////////////////////////
///
///  Write mail to disk
///
/////////////////////////////////////////////////////////////////////
PROCEDURE MailToDisk( cFile, oMail )

  MemoWrit( cFile, oMail:toString() )

RETURN
