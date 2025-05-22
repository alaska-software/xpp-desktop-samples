//////////////////////////////////////////////////////////////////////
//
//  WSTRACECLIENTDIALOG.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The class WSTraceClientDialog implements a user interface
//   suitable to connect and disconnect a websocket client with a
//   server. At the same time the dialog adapts its visual appearance
//   according the connection state.
//   
//  Remarks:
//   The class WSTraceClientDialog inherits from the interface
//   IWSTraceClientEndpoint and therefore implements all methods that
//   enables him to be assigned to a websocket client from the class
//   WSTraceClient. The dialog is then informed about all connection
//   state changes and about text messages that have been sent from
//   the socket server.
//   
//   Additionally, the dialog gets assigned a thread that is started
//   and stopped depending on whether the connection from the
//   websocket client to the server is established or disconnected.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#define BG_COLOR_CONNECTED    GRA_CLR_GREEN
#define BG_COLOR_DISCONNECTED GRA_CLR_RED
#define FG_COLOR_CONNECTED    GRA_CLR_WHITE
#define FG_COLOR_DISCONNECTED GRA_CLR_WHITE

//
// This class is derived from the implementation-level class of
// the form. Instance variables are declared in the _TraceClient
// class.
//
CLASS WSTraceClientDialog FROM _WSTraceClientDialog,       ;
                               IWSTraceClientEndpoint
   PROTECTED:
      VAR _PortNumber
      VAR _TraceHandler
      VAR _MessageThread
      VAR _Connected

      METHOD applyConnectionState

   EXPORTED:
      METHOD init
      METHOD create
      METHOD destroy

      METHOD connect
      METHOD disconnect

      ACCESS ASSIGN METHOD setPort          VAR Port
      ACCESS ASSIGN METHOD setTraceHandler  VAR TraceHandler
      ACCESS ASSIGN METHOD setMessageThread VAR MessageThread

      // Implements the Interface IWSTraceClientEndpoint
      METHOD setConnected
      METHOD setDisconnected
      METHOD setMessage
ENDCLASS

//
// Implement the class initialization here.
//
METHOD WSTraceClientDialog:init()
   SUPER
RETURN self


//
// Request system resources
//
METHOD WSTraceClientDialog:create()

   // Assign a default port if none was assigned
   // between the :init() and :create() call
   IF Empty( ::_PortNumber )
     ::_PortNumber    := 81
   ENDIF

   ::_Connected     := .F.

   // Ensure the baseclass implementation is called
   // This also hands over all parameters passed
   SUPER

   //
   // Setup the dialog
   // The connection state shall be reflected.
   // Also, the default port number shall be offered
   // to connect the client
   //
   ::applyConnectionState()
   ::_Port:setData( ::_PortNumber )

   ::show()
RETURN self

//
// The dialog goes out of scope. Ensure that the
// connecton is deestablished in any case.
//
METHOD WSTraceClientDialog:destroy()
  SUPER
  ::disconnect()
RETURN self

//
// Set up the dialog so that it visualizes the
// current connection state
//
METHOD WSTraceClientDialog:applyConnectionState()
  LOCAL cCaption, bActivate, nBGColor, nFGColor

  // Select the settings
  IF ::_Connected
    cCaption  := "Disconnect"
    bActivate := {|| ::disconnect() }
    nBGColor  := BG_COLOR_CONNECTED
    nFGColor  := FG_COLOR_CONNECTED
  ELSE
    cCaption  := "Connect"
    bActivate := {|| ::connect() }
    nBGColor  := BG_COLOR_DISCONNECTED
    nFGColor  := FG_COLOR_DISCONNECTED
  ENDIF

  // apply the settings
  ::_Button:setCaption( cCaption )
  ::_Button:activate := bActivate
  ::_TraceClientLabel:setColorBG(nBGColor)
  ::_TraceClientLabel:setColorFG(nFGColor)

RETURN self


//
// Connect the client to the server
// As the operation can be time consuming
// the button is disabled in the meantime
//
METHOD WSTraceClientDialog:connect()
  LOCAL lConnected

  ::_Button:disable()

  lConnected := ::_TraceHandler:connect()

  ::_Button:enable()

  IF .NOT. lConnected
    MsgBox( "Connection failed" )
  ENDIF

RETURN self

//
// Disconnect the client
//
METHOD WSTraceClientDialog:disconnect()
  ::_TraceHandler:disconnect()
RETURN self

//
// The client is connected. Now start the thread
// that is fetshing messages from the server
//
METHOD WSTraceClientDialog:setConnected()
  ::_Connected := .T.
  ::applyConnectionState()
  ::_MessageThread:start()
RETURN self

//
// The client is disconnected. Stop the thread
// that is fetching messages
//
METHOD WSTraceClientDialog:setDisconnected()
  ::_Connected := .F.
  ::applyConnectionState()
  ::_MessageThread:stop()
RETURN self

//
// A text message has been received. Display the
// message in the static.
//
METHOD WSTraceClientDialog:setMessage( cText )
  ::_Text:setCaption( cText )
RETURN self

//
// Set the port. The port passed can be either a
// character string or a numeric value
//
METHOD WSTraceClientDialog:setPort( xPort )
  IF (PCount() > 0) .AND. .NOT. (NIL == xPort)
    IF "C" == Valtype( xPort )
      xPort := Val( xPort )
    ENDIF
    ::_PortNumber := xPort
  ENDIF
RETURN ::_PortNumber


METHOD WSTraceClientDialog:setTraceHandler( oTraceHandler )
  // ACCESS case
  IF PCount()==0
    RETURN ::_TraceHandler
  ENDIF

  // Ensure that only objects of class WebSocketClient
  // get assigned.
  IF .NOT. ((ValType(oTraceHandler)=="O")    .AND.  ;
             oTraceHandler:isDerivedFrom("WebSocketClient"))
    // Todo: Error handling not implemented yet.
    RETURN NIL
  ENDIF

  // ASSIGN case
  ::_TraceHandler := oTraceHandler

  // The trace handler needs to know the endpoint
  // that serves as communication point.
  ::_TraceHandler:WSTraceClientEndpoint := self

RETURN self

METHOD WSTraceClientDialog:setMessageThread( oMessageThread )
  // ACCESS case
  IF PCount()==0
    RETURN ::_MessageThread
  ENDIF

  // Only allow objects of class MessageThread
  IF .NOT. ((ValType(oMessageThread)=="O")    .AND.  ;
             oMessageThread:isDerivedFrom("MessageThread"))
    // Todo: Error handling not implemented yet.
    RETURN NIL
  ENDIF

  // ASSIGN case
  ::_MessageThread := oMessageThread
RETURN self
