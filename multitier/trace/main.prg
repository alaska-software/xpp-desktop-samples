//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The first dialog is shown on which configuration settings can be
//   done. After the main application window is started a HTTP
//   endpoint is created on which incoming connections can be
//   established. Within the eventloop all events are sent to the main
//   application and also to all connected clients.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "xbp.ch"
#include "appevent.ch"

PROCEDURE Main()
  LOCAL oDataEntry, oHttpEndpoint
  LOCAL nEvent, oXbp, mp1, mp2, cTraceData
  LOCAL oApplication, oConfiguration

  //
  // Show the configuration dialog where the
  // user can configure the port. After the
  // dialog is closed it will be destroyed.
  //
  oDataEntry := DataEntry():New():Create()
  oDataEntry:showModal()
  // Get the configuration
  oConfiguration := oDataEntry:Configuration
  oDataEntry:destroy()

  //
  // Create the main dialog that is the
  // user interface for the application
  //
  oApplication := Application():new()
  oApplication:Url  := oConfiguration:Url
  oApplication:Port := oConfiguration:Port
  oApplication:create()

  //
  // The application implements a http endpoint
  // Use a Web browser to connect to that endpoint.
  //
  oHttpEndpoint := HttpEndpoint():new( oConfiguration:Port )
  oHttpEndpoint:start()

  //
  // Initialize the class Trace. Trace will offer an HTML
  // start page suitable to connect to the application
  //
  Trace( oConfiguration:Port )

  //
  // Associate an instance to which the WebSocketHandler
  // sends notifications about the number of connected
  // clients
  //
  WSTrace():InstanceCount := oApplication

  //
  // If the user decided on the configuration step
  // then start a web browser now that can connect
  // with the application
  //
  IF oConfiguration:StartBrowser
    StartBrowser( oConfiguration:Url )
  ENDIF

  //
  // The event loop
  //
  nEvent := xbe_None
  mp1 := mp2 := oXbp := NIL
  WHILE nEvent != xbeP_Close

    nEvent := AppEvent ( @mp1, @mp2, @oXbp )
    oXbp:HandleEvent ( nEvent, mp1, mp2 )

    //
    // Any connected client shall be informed about
    // all events.
    //
    cTraceData := AppName()                                + ;
                  " nEvent("+AllTrim(Var2Char(nEvent))+")" + ;
                  " mp1("+AllTrim(Var2Char(mp1))+")"       + ;
                  " mp2("+AllTrim(Var2Char(mp2))+")"

    // Broadcast the trace to all connected client
    WSTrace():broadcast( cTraceData )

    // and to the UI also
    oApplication:setTraceData( cTraceData )

  ENDDO

  // Stop the HttpEndpoint. No more connections are possible
  oHttpEndpoint:stop()
  // Close all currently active connections
  WSTrace():closeAll()

RETURN

PROCEDURE StartBrowser( cUrl )
  RunShell( "/C explorer.exe "+cUrl,, .T., .T. )
RETURN

PROCEDURE StartXbaseClient( nPort )
  LOCAL cPort
  cPort := Str( nPort )
  RunShell( "/C traceclient.exe "+cPort,, .T., .T. )
RETURN

PROCEDURE AppSys(); RETURN
