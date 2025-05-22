//////////////////////////////////////////////////////////////////////
//
//  MESSAGETHREAD.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The class MessageThread implements a thread for processing messages
//   sent from the connected websocket server.
//   
//   The message processing is done in the method :execute() in blocking
//   mode. This ensures that the thread is only executes when incoming
//   messages are being pending.
//   
//   Furthermore the class MessageThread implements the method :stop()
//   that safely terminates the execution of the thread.
//   
//  Remarks:
//   Disconnecting the client from the server will implicitly terminate
//   the message processing.
//   
//////////////////////////////////////////////////////////////////////


// Constants for the method :interval()
#define INTERVAL_IDLE 1   // 1/100th of a second
#define INTERVAL_OFF  NIL // no execution

CLASS MessageThread FROM Thread
  PROTECTED:
    VAR _WSTraceClient
  EXPORTED:
    METHOD init
    METHOD atStart
    METHOD execute
    METHOD stop
ENDCLASS

//
// Set the websocket client
//
METHOD MessageThread:init( oWSTraceClient )
  ::_WSTraceClient := oWSTraceClient
  SUPER:init( , "Message thread" )
RETURN

//
// By default we will have an idle interval
//
METHOD MessageThread:atStart()
  ::setInterval(INTERVAL_IDLE)
RETURN self

//
// Handle the messages in blocking mode.
// As soon as the client gets disconnected
// :handleMessage() will return false.
//
METHOD MessageThread:execute()
  DO WHILE ::_WSTraceClient:handleMessage( .T. )
  ENDDO
RETURN self

//
// Stop the thread
//
METHOD MessageThread:stop()
  // Ignore this method call when the
  // thread is not running.
  IF .NOT. ::Active
    RETURN self
  ENDIF

  // Ensure that the websocket client is
  // disconnected. This will also make
  // :handleMessage() .F., hence ending
  // processing in method :execute()
  ::_WSTraceClient:disconnect()

  //
  // Thread shutdown sequence
  //

  // First ensure that the method :execute()
  // will not be called again
  ::setInterval( INTERVAL_OFF )

  // Wait until thread execution is finalized
  IF ThreadId() <> ::ThreadId
    ::synchronize( 0 )
  ENDIF
RETURN self
