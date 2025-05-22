//////////////////////////////////////////////////////////////////////
//
//  APPLICATION.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The main window
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"


//////////////////////////////////////////////////////////////////////
// This class is derived from the implementation-level class of the
// form. Instance variables are declared in the _Application class.
//////////////////////////////////////////////////////////////////////
CLASS Application FROM _Application
   PROTECTED:
      VAR _Url
      VAR _Port
      METHOD showHint

   EXPORTED:
      METHOD init
      METHOD create
      METHOD setTraceData
      METHOD setNumeric
      ACCESS ASSIGN METHOD setUrl VAR URL
      ACCESS ASSIGN METHOD setPort VAR Port

      METHOD startBrowserClient
      METHOD startXbaseClient
ENDCLASS

//////////////////////////////////////////////////////////////////////
// Initialize form
//////////////////////////////////////////////////////////////////////
METHOD Application:init()

   // Execute method of the super class
   SUPER

   ::_Url                  := ""
   ::_Port                 := 0
   ::_HandledEvent:caption := ""

   ::_StartBrowserClient:Activate := {|| ::startBrowserClient() }
   ::_StartXbaseClient:Activate   := {|| ::startXbaseClient()   }

RETURN self


//////////////////////////////////////////////////////////////////////
// Request system resources
//////////////////////////////////////////////////////////////////////
METHOD Application:create()

   // Execute method of the super class
   SUPER

   ::setTitle( "Trace Server [Port:"+AllTrim(Str(::_Port))+"]" )

   // Display the form
   ::show()

RETURN self

//
// The trace data is displayed in a static
// on the dialog
//
METHOD Application:setTraceData( cTraceData )
  ::_HandledEvent:setCaption( cTraceData )
RETURN self

//
// The number of connected clients is displayed in a
// static on the dialog
//
METHOD Application:setNumeric( nNumeric )
  ::_NumClients:setCaption(AllTrim(Str(nNumeric)))
RETURN self

//
// Display the connection string used to connect
// to the server.
//
METHOD Application:showHint()
   LOCAL cHint
   cHint := "Connect to following url: " + ::_Url
   ::_HintMessage:setCaption( cHint )
RETURN self

//
// Pass the URL for starting a web
// browser
//
METHOD Application:startBrowserClient()
  StartBrowser( ::_Url )
RETURN self

//
// Pass a port to start the Xbase++ client
//
METHOD Application:startXbaseClient()
  StartXbaseClient( ::_Port )
RETURN self

//
// ACCESS ASSIGN method for the URL
//
METHOD Application:setUrl( cUrl )
  IF PCount() > 0
    ::_Url := cUrl
    ::showHint()
  ENDIF
RETURN ::_Url

//
// ACCESS ASSIGN method for the port
//
METHOD Application:setPort( nPort )
  IF PCount() > 0
    ::_Port := nPort
  ENDIF
RETURN ::_Port

//EOF
/////
