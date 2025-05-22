//////////////////////////////////////////////////////////////////////
//
//  TRACECLIENTMAIN.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   In the procedure MAIN all objects of the application are created
//   and connected. Before the application terminates the main window
//   gets destroyed.
//   
//  Remarks:
//   Instanzes of the application are evenly distributed between all
//   available CPU cores.
//   
//////////////////////////////////////////////////////////////////////


#include "appevent.ch"

#define SERVERNAME  GetEnv("Computername" )
#define TARGET      "wstrace"
#define PORT        81

PROCEDURE Main( cPort )

   LOCAL oWSTraceClient, oWSTraceClientDialog, nPort, oMessageThread

   //
   // The application may get started several times from the
   // server UI. Ensure that the instances of this application are
   // distributed evenly on the available CPUs
   //

   SetLogicalProcessor( RandomInt(GetLogicalProcessorCount()) )

   // The default port may be passed from the command line.
   nPort := PORT
   IF PCount() > 0
     nPort := Val( cPort )
   ENDIF

   // Instantiate the WebSocket client.
   oWSTraceClient := WSTraceClient():new( SERVERNAME, TARGET, nPort )

   // A thread responsible for fetching messages from the
   // WebSocket client
   oMessageThread := MessageThread():new(oWSTraceClient)

   //
   // Before creating the main window pass the port, the WebSocket
   // client and the message handling thread.
   //

   oWSTraceClientDialog := WSTraceClientDialog():new()

   oWSTraceClientDialog:Port          := nPort
   oWSTraceClientDialog:TraceHandler  := oWSTraceClient
   oWSTraceClientDialog:MessageThread := oMessageThread

   oWSTraceClientDialog:create()

   //
   // Run the application
   //
   SetAppWindow( oWSTraceClientDialog )
   EventHandler()

   // Cleaning up. Destroy the dialog.
   oWSTraceClientDialog:destroy()

RETURN

//
// The event handler returns as soon as Close or Quit
// is fetched from the event queue
//
STATIC PROCEDURE EventHandler()
   LOCAL nEvent, oXbp, mp1, mp2
   mp1 := mp2 := oXbp := NIL
   nEvent := xbe_None
   WHILE (.NOT. nEvent==xbeP_Close) .AND. ;
         (.NOT. nEvent==xbeP_Quit )
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN

//
// No default window is created
//
PROCEDURE AppSys()
RETURN
