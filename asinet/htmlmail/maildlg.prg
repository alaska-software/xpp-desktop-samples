//////////////////////////////////////////////////////////////////////
//
//  MAILDLG.PRG
//
//  Copyright:
//      Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//      Dialog window for htmlmail sample
//   
//  Remarks:
//      When a mail is sent with an object of the class SmtpClient then
//      the activities can be dumped via an object that implements the
//      method :write(). For this we use the class MailSample that therefore
//      needs to implement such a method.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"


///////////////////////////////////////////////////////////////////////////////
// This class is derived from the implementation-level class of the form.
// Instance variables are declared in the _MailSample class.
///////////////////////////////////////////////////////////////////////////////
CLASS MailSample FROM _MailSample
   EXPORTED:
      METHOD init
      METHOD create

      METHOD write
ENDCLASS

///////////////////////////////////////////////////////////////////////////////
// Initialize form
///////////////////////////////////////////////////////////////////////////////
METHOD MailSample:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_MailSample:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

RETURN self



///////////////////////////////////////////////////////////////////////////////
// Request system resources
///////////////////////////////////////////////////////////////////////////////
METHOD MailSample:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   // Do not allow change of size
   ::maxSize := ::currentSize()
   ::minSize := ::currentSize()

   // Protect password
   ::oEditPassword:unreadable := .T.

   ::title := "Alaska Html Mail Demo"

   * Execute method of the super class
   ::_MailSample:create( oParent, oOwner, aPos, aSize, aPP, lVisible )


   * Display the form
   ::show()

RETURN self

///////////////////////////////////////////////////////////////////////////////
//
// This method servers also as log writer
// This method is called by SmtpClient.
//
///////////////////////////////////////////////////////////////////////////////
METHOD MailSample:write( cMsg )
  cMsg := StrTran( cMsg, Chr(13)+Chr(10), "" )
  ::oListBoxOutput:addItem( cMsg )
RETURN self

//EOF
/////
