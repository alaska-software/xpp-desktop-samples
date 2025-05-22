//////////////////////////////////////////////////////////////////////
//
//  SERVER.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2012. All rights reserved.
//
//  Contents:
//      Server side of the sockets demo program. It receives an array of
//      code blocks via socket, evaluates them and and returns the result
//      to the client process.
//
//  Remarks:
//      Two classes are implemented in this program:
//
//      ServerThread()
//         This thread accepts client connections and spawns a new thread
//         when a connection is established.
//      ClientThread()
//         Objects of this class handle communication between individual
//         client processes and the server.
//
//////////////////////////////////////////////////////////////////////

#include "Appevent.ch"
#include "socket.ch"

#define MAX_READLEN 4096

REQUEST Scatter  // from \SOURCE\SYS\BLOCKS.PRG


/*
 * Main() handles user input for server termination
 */
PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, oThread

   CLS

   oThread := ServerThread():new( 1042 )
   oThread:start()

   IF .NOT. oThread:active
      ? "Server start-up failed"
      QUIT
   ELSE
      ? "Server up and running ..."
      ? "Press ESC to QUIT"
   ENDIF

   DO WHILE nEvent<> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )

      IF nEvent == xbeK_ESC
         EXIT
      ENDIF
   ENDDO

   oThread:stop()
RETURN




/* ***************************************************************************
 *
 * This class listens on a socket for client connections
 */
CLASS ServerThread FROM Thread
   PROTECTED:
   VAR nPort
   VAR nSocket

   EXPORTED:
   METHOD init, atStart, execute, atEnd, stop
   METHOD logClient
ENDCLASS


METHOD ServerThread:init( nPort )
   ::Thread:init()
   ::nSocket := 0
   ::nPort   := nPort
RETURN self


/*
 * Thread starts!
 *
 * Create socket, put it into listening state and set time interval
 * to zero, so that :execute() is repeated automatically
 */
METHOD ServerThread:atStart
   ::nSocket := SocketCreate( SOCK_STREAM, ::nPort )

   IF ::nSocket <> 0
      SocketListen( ::nSocket )
      ::setInterval(0)
   ENDIF
RETURN self


/*
 * Thread is running!
 *
 * Accept a client connection and spawn new thread for data transmission
 */
METHOD ServerThread:execute
   LOCAL oClient, nClientSocket

   IF ::nSocket <> 0
      nClientSocket := SocketAccept( ::nSocket )

      IF nClientSocket <> 0
         ::logClient( nClientSocket )
         oClient := ClientThread():new( nClientSocket )
         oClient:start()
      ENDIF
   ENDIF
RETURN self


/*
 * Thread is ending!
 */
METHOD ServerThread:atEnd
   ::nSocket := 0
RETURN self


/*
 * Stop listening thread from outside!
 *
 * :setInterval( NIL ) results in :execute() not being called again.
 * SocketClose() causes SocketAccept() -which is blocking- to return.
 * :synchronize(0) causes external function to wait until this thread
 * has ended.
 */
METHOD ServerThread:stop
   IF ::nSocket <> 0
      ::setInterval( NIL )

      SocketClose( ::nSocket )

      ::synchronize(0)
   ENDIF
RETURN self


/*
 * Display log information
 */
METHOD ServerThread:logClient( nClientSocket )
   LOCAL aIPAddress := SocketGetPeerName( nClientSocket )

   ? "Servicing:", Var2Char( aIPAddress )
RETURN self




/* ***************************************************************************
 * This class communicates with a client process using a simple communication
 * protocol. The first 4 bytes of received data contain the length of the
 * subsequent message that must be read. The message consists of binary
 * data that is converted with Bin2Var()/Var2Bin().
 */
CLASS ClientThread FROM Thread
   PROTECTED:
   VAR nSocket
   METHOD evalData

   EXPORTED:
   METHOD init, execute
ENDCLASS


METHOD ClientThread:init( nSocket )
   ::Thread:init()
   ::nSocket := nSocket
RETURN self


/*
 * Establish a simple communication with a client process
 *
 *  1. Read incoming message
 *  2. Bin2Var() -> array of code blocks
 *  3. Evaluate the code blocks
 *  4. Var2Bin() -> result is binary
 *  5. Send binary result to client process
 */

METHOD ClientThread:execute
   LOCAL nError  := 0
   LOCAL cLength := Space(4)
   LOCAL cBuffer
   LOCAL nBytes
   LOCAL xData
   LOCAL bError
   LOCAL oError  := NIL

   /*
    * Obtain length of message to read from socket
    */
   nBytes := SocketRecv( ::nSocket, @cLength, 4, ,@nError )

   IF nError <> 0 .OR. nBytes <> 4
      cBuffer := "ERROR:" + LTrim(Str(nError)) + " BYTES RECEIVED:" + LTrim(Str(nBytes))
      cLength := U2Bin( Len( cBuffer ) )
      SocketSend( ::nSocket, cLength + cBuffer )
      RETURN self
   ENDIF

   /*
    * Read message from socket
    */
   nBytes  := Bin2U( cLength )
   cBuffer := ReadFromSocketLen( ::nSocket, nBytes, @nError )

   IF nError == 0
      /*
       * CLIENT.EXE sends an array of code bocks!
       * Convert array, evaluate the code blocks
       * and return result as binary data.
       */
      bError := ErrorBlock( {|e| BREAK(e) } )
      BEGIN SEQUENCE
         xData   := Bin2Var( cBuffer )
         xData   := ::evalData( xData )
         cBuffer := Var2Bin( xData )
      RECOVER USING oError
         cBuffer := "ERROR:" + oError:DESCRIPTION
      END SEQUENCE
      ErrorBlock( bError )
   ELSE
      cBuffer := "ERROR:" + LTrim(Str(nError))
   ENDIF

   /*
    * Add length of result to buffer
    */
   cLength := U2Bin( Len( cBuffer ) )
   SocketSend( ::nSocket, cLength + cBuffer )

   /*
    * Release the socket recources
    */
   SocketShutdown( ::nSocket, SD_BOTH )
   SocketClose( ::nSocket )

RETURN self

/*
  This function ensures that <nLen> bytes are read from the
  socket.
*/
FUNCTION ReadFromSocketLen( nSocket, nLen, nError )
  LOCAL cRet, cTmp, nRead, nTmp
  cRet  := ""
  nRead := 0
  DO WHILE Len( cRet ) < nLen
    nTmp := Min( MAX_READLEN, nLen - nRead )
    cTmp := Space( nTmp )
    nTmp :=  SocketRecv( nSocket, @cTmp, , ,@nError )
    IF nError <> 0
      RETURN cRet
    ENDIF
    cRet += SubStr( cTmp, 1, nTmp )
    nRead += nTmp
  ENDDO
RETURN cRet

/*
 * aData is a 2-column array -> { { lIsResult, bCodeblock } }
 */
METHOD ClientThread:evalData( aData )
   LOCAL xResult
   LOCAL i, imax := Len( aData )

   FOR i:=1 TO imax
      IF aData[i,1]
         xResult := Eval( aData[i,2] )
      ELSE
         Eval( aData[i,2] )
      ENDIF
   NEXT
RETURN xResult