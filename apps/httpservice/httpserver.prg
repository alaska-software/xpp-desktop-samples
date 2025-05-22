//////////////////////////////////////////////////////////////////////
//
//  HTTPSERVER.PRG
//
//  Copyright:
//     Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//     Implementation of a http server class
//   
//  Remarks:
//    We are going to listen on port 81.
//    Since this is not standard, Urls have to be entered in
//    the following way:
//   
//    http://your-url:81
//   
//    Debugging: When the application is running in the debugger,
//               the method :main() of the class HttpServer is called
//               directly. Nevertheless the service must be installed since
//               the service status is read.
//   
//////////////////////////////////////////////////////////////////////

#include "socket.ch"

#include "service.ch"

#define SERVERPORT     80

#define RUNNING        1
#define NOTRUNNING     2
#define PAUSING        3

CLASS HttpServer FROM ServiceApp

  HIDDEN:
    CLASS VAR    nStatus;

  PROTECTED:
    CLASS VAR    nServerSocket
    CLASS VAR    nFDSet
    CLASS VAR    cRootDirectory
    CLASS METHOD connected

  EXPORTED:

    CLASS METHOD initClass
    CLASS METHOD setRootDirectory
    CLASS METHOD serverMain

    CLASS METHOD main
    CLASS METHOD pause
    CLASS METHOD continue
    CLASS METHOD stop


ENDCLASS

//
// When this class is created, we are creating
// a socket in non-blocking mode.
//
CLASS METHOD HttpServer:initClass()

  LOCAL nSocket

  ::nStatus := RUNNING

  Logger():log( "HttpServer begin startup" )

  // We need a collection for the server socket
  IF 0 == ( ::nFDSet := SocketFDSetNew( ) )
    RETURN SELF
  ENDIF

  IF ( nSocket := SocketCreate(SOCK_STREAM, SERVERPORT)) == INVALID_SOCKET
    RETURN SELF
  ENDIF

  // We set our socket to listening mode
  IF !SocketListen( nSocket )
    RETURN SELF
  ENDIF

  // Furthermore it will be in non-blocking mode.
  IF ! SocketSetBlockingMode( nSocket, .F. )
    RETURN SELF
  ENDIF

  ::nServerSocket := nSocket

  Logger():log( "HttpServer initialized" )

RETURN SELF

//
// Tell the Http server what directory to use as Root
//
CLASS METHOD HttpServer:setRootDirectory( cDir )
  ::cRootDirectory := cDir
RETURN SELF

//
// The main task of a httpserver is to listen to its accept
// socket. When someone is "knocking on the door", a thread
// is started to handle the job.
//
CLASS METHOD HttpServer:serverMain( )

  LOCAL nSocket, oJob

  IF INVALID_SOCKET != ( nSocket := ::connected() )
    oJob := HttpJob():new( nSocket, ::cRootDirectory )
    Logger():log( "Executing job with socket " + Alltrim( Str( nSocket ) ) )
    oJob:start()
  ENDIF

RETURN SELF

//
// Return as soon as a client tries to connect at the socket or
// after one second.
//
CLASS METHOD HttpServer:connected( )

  LOCAL nNumber, nConnectionSocket

  // ALLWAYS invalidate the socket collection
  IF ! SocketFD_ZERO( ::nFDSet )
    RETURN INVALID_SOCKET
  ENDIF

  // Set the socket to the collection
  IF ! SocketFD_SET( ::nServerSocket, ::nFDSet )
    RETURN INVALID_SOCKET
  ENDIF

  IF SOCKET_ERROR == ( nNumber := SocketSelect( ::nFDSet, , , { 1, 0} ) )
    RETURN INVALID_SOCKET
  ENDIF

  // If the result is 0, there is a timeout condition, oterwise
  // there is a connect request at the socket.
  IF nNumber == 0
    RETURN INVALID_SOCKET
  ENDIF

  IF ( nConnectionSocket := SocketAccept( ::nServerSocket ) ) == 0
    RETURN INVALID_SOCKET
  ENDIF

  Logger():log( "Socket successfully accepted" )


RETURN nConnectionSocket

CLASS METHOD HttpServer:main( aParam )

  //
  // As long as our status class is in running mode, we
  //
  DO WHILE ::nStatus != NOTRUNNING

    //
    // check if not in pause mode
    //
    IF ::nStatus == PAUSING
      Sleep( 10 )
      Loop
    ENDIF

    //
    // and call the main method of our server.
    //
    HttpServer():serverMain( )
  ENDDO

RETURN


//
// Here are the callback methods for the service
// They are setting the status accordingly
//
CLASS METHOD HttpServer:pause()
  ::nStatus := PAUSING
RETURN

CLASS METHOD HttpServer:continue()
  ::nStatus := RUNNING
RETURN

CLASS METHOD HttpServer:stop()
  ::nStatus := NOTRUNNING
RETURN
