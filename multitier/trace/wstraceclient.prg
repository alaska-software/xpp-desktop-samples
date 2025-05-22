//////////////////////////////////////////////////////////////////////
//
//  WSTRACECLIENT.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The class WSTraceClient inherits from WebSocketClient and therefore
//   implements a websocket client that can be connected with a server.
//   
//   Method calls for event notification are forwarded to an object that
//   was assigned to the member variable WSTraceClientEndpoint. Any
//   object can serve as endpoint as long as it has inheritted from the
//   interface IWSTraceClientEndpoint. This interface guarantees the
//   existance of all methods that are required by the class
//   WSTraceClient.
//   
//   
//  Remarks:
//////////////////////////////////////////////////////////////////////


#include "appevent.ch"

#define URL         GetEnv("Computername" )
#define HOST        "www.nowhere.com"
#define APPLICATION "wstrace"

//
// The WebSocket client works on the interface
// IWSTraceClientEndpoint.
// The interface has methods to be triggered on
// connection and disconnection events. Another
// method can be triggered if a textmessage
// arrived.
//
CLASS IWSTraceClientEndpoint
  EXPORTED:
    DEFERRED METHOD setDisconnected
    DEFERRED METHOD setConnected
    DEFERRED METHOD setMessage
ENDCLASS

//
// WSTraceClient is a WebSocketClient
//
CLASS WSTraceClient FROM WebSocketClient
  PROTECTED:
    VAR _WSTraceClientEndpoint
  EXPORTED
    METHOD init

    // Methods that can be implemented when deriving
    // from a WebSocketClient
    METHOD onText
    //METHOD onBinary      // not implemented. The baseclass behaviour applies
    METHOD onConnect
    METHOD onDisconnect

    ACCESS ASSIGN METHOD setWSTraceClientEndpoint VAR WSTraceClientEndpoint
ENDCLASS

//
// Init does not do anything special here.
// Just ensure that the baseclass' init is
// called with the parameters passed
//
METHOD WSTraceClient:init()
  SUPER
RETURN self

//
// :onText, :onConnect() and :onDisconnect() get
// automatically called from the framework. The class
// implements all neccessary methods.
//
METHOD WSTraceClient:onText( cText )
  IF (NIL==::_WSTraceClientEndpoint)
    RETURN self
  ENDIF
  ::_WSTraceClientEndpoint:setMessage( cText )
RETURN self

METHOD WSTraceClient:onConnect()
  IF (NIL==::_WSTraceClientEndpoint)
    RETURN self
  ENDIF
  ::_WSTraceClientEndpoint:setConnected()
RETURN self

METHOD WSTraceClient:onDisconnect()
  IF (NIL==::_WSTraceClientEndpoint)
    RETURN self
  ENDIF
  ::_WSTraceClientEndpoint:setDisconnected()
RETURN self

//
// ACCESS ASSIGN of the member variable _IWSTraceClientEndpoint.
// The implementation ensures that only objects can be assignd
// that implement the interface IWSCTraceHandlerUI
//
METHOD WSTraceClient:setWSTraceClientEndpoint( oWSCTHUI )
  IF PCount()>0
    IF oWSCTHUI==NIL
      ::_WSCTraceHandlerEndpoint := NIL
      RETURN(NIL)
    ENDIF
    IF (ValType(oWSCTHUI)=="O") .AND.                    ;
        oWSCTHUI:isDerivedFrom("IWSTraceClientEndpoint")
      ::_WSTraceClientEndpoint := oWSCTHUI
    ENDIF
  ENDIF
RETURN ::_WSTraceClientEndpoint
