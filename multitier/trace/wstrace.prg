//////////////////////////////////////////////////////////////////////
//
//  WSTRACE.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The class WSTrace is derived from WebSocketHandler. An incoming
//   web socket request for the URL ws://<server>:<port>/WSTrace will
//   be associated with an instance of the class WSTrace. The class
//   Trace is derived from WebHandler and implements the method
//   :start().
//   
//   The URL http://<server>:<port>/Trace/start returns an html page
//   suitable to connect to the websocket server.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


//
// The class WebHandler depends on web.ch
//
#include "web.ch"

//
// All connections to ws://<server>:<port>/WSTrace will
// be associated with an instance of the class WSTrace
//
CLASS WSTrace FROM WebSocketHandler
  PROTECTED:
    CLASS VAR _Instances
    CLASS VAR _InstanceCount

    SYNC METHOD       _syncedOnConnect
    SYNC METHOD       _syncedOnDisConnect
  EXPORTED:
    ACCESS ASSIGN CLASS METHOD setInstanceCount VAR InstanceCount

    SYNC CLASS METHOD _syncedBroadcast
         CLASS METHOD broadcast
    SYNC CLASS METHOD closeAll

    METHOD onConnect
    METHOD onDisconnect
    METHOD onText
    METHOD onBinary
ENDCLASS

CLASS METHOD WSTrace:setInstanceCount( oInstanceCount )
  IF .NOT. IsMethod( oInstanceCount, "setNumeric" )
    RETURN self
  ENDIF
  ::_InstanceCount := oInstanceCount
RETURN self

CLASS METHOD WSTrace:_syncedBroadcast( cText )
  IF NIL == ::_Instances
    RETURN self
  ENDIF
  AEval( ::_Instances, {|o| o:sendText( cText ) } )
RETURN self

//
// Send a message to all connected clients
//
CLASS METHOD WSTrace:broadcast( cText )
  LOCAL cTmp
  cTmp := ""
  cTmp += Time() + " " + AllTrim(Var2Char(Milliseconds())) + ": "
  ::_syncedBroadcast( cTmp + cText )
RETURN self

//
// Close all connected clients
//
CLASS METHOD WSTrace:closeAll()
  IF( ::_Instances == NIL ); RETURN; ENDIF
  AEval( ::_Instances, {|o| o:close() } )
  ::_Instances := NIL
RETURN self

//
// Helper method for :onConnect()
//
METHOD WSTrace:_syncedOnConnect()
  IF NIL == ::_Instances
    ::_Instances := Array( 0 )
  ENDIF

  AAdd( ::_Instances, self )

  IF NIL == ::_InstanceCount
    RETURN self
  ENDIF

  // Set the updated number of instances
  ::_InstanceCount:setNumeric( Len(::_Instances) )
RETURN self

//
// A connection is established
//
METHOD WSTrace:onConnect()
  ::_syncedOnConnect()
RETURN self

//
// Remove the instance from the instance
// collection
//
METHOD WSTrace:_syncedOnDisConnect
  LOCAL nPos

  nPos := AScan( ::_Instances, self )

  IF nPos == 0
    RETURN self
  ENDIF

  ARemove( ::_Instances, nPos )

  IF NIL == ::_InstanceCount
    RETURN self
  ENDIF

  // Set the updated number of instances
  ::_InstanceCount:setNumeric( Len(::_Instances) )
RETURN self

//
// The instance got disconnected
//
METHOD WSTrace:onDisconnect()
  ::_syncedOnDisconnect()
RETURN self

//
// When a connected client sends a text message
// then nothing is done
//
METHOD WSTrace:onText( cText )
  Unused( cText )
RETURN self

//
// When a connected client sends a binary message
// then nothing is done
//
METHOD WSTrace:onBinary( cBinary )
  Unused( cBinary )
RETURN self

//
// The class Trace implements the REST path /Trace/start.
// The start page can be fetched with the URI
// http://<server>:<port>/Trace/start
//
CLASS Trace FROM WebHandler
  PROTECTED:
    CLASS VAR _Port
  EXPORTED:
    CLASS METHOD initClass
    METHOD start
ENDCLASS

CLASS METHOD Trace:initClass( nPort )
  ::_Port := nPort
RETURN self

#define STARTPAGE    "trace.htm"
#define COMPUTERNAME "<!--[COMPUTERNAME]-->"
#define SERVER_PORT  "<!--[PORT]-->"
#define CLASSNAME    "<!--[CLASS]-->"
METHOD Trace:start()
  LOCAL cHtml

  // Load the template of the startpage
  IF .NOT. File( STARTPAGE )
    RETURN("<html><body><h1>Can not load start page ("+STARTPAGE+")</h1></body></html>")
  ENDIF
  cHtml := MemoRead( STARTPAGE )

  // Replace information on the templates so that the client
  // is able to connect
  cHtml := StrTran( cHtml, COMPUTERNAME, GetEnv("COMPUTERNAME") )
  cHTML := StrTran( cHtml, SERVER_PORT, AllTrim(Str(::_Port)) )
  cHtml := StrTran( cHtml, CLASSNAME, "WSTrace" )
RETURN cHtml
