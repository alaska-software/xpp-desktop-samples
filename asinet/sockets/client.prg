//////////////////////////////////////////////////////////////////////
//
//  CLIENT.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2011. All rights reserved.
//
//  Contents:
//      Client side of the sockets demo program. It sends an array of
//      code blocks via socket to the server process. The server
//      evaluates the code blocks and returns the result.
//
//  Remarks:
//      The client process does not use a Database Engine at all, but it
//      displays data stored in CUSTOMER.DBF
//
//////////////////////////////////////////////////////////////////////

#include "socket.ch"
#include "Inkey.ch"

MEMVAR getList

PROCEDURE DbeSys
   // Don't load a Database Engine
   // Data stored in a database is retrieved from the server process
RETURN



PROCEDURE Main( cServerName )
   LOCAL nPortNumber := 1042
   LOCAL nCustomerID := 1
   LOCAL xCustData

   IF Empty( cServerName )
      CLS
      ?
      ? "you must provide the name of the station that runs SERVER.EXE"
      ?
      ? "Try: CLIENT.EXE localhost"
      QUIT
   ENDIF

   DO WHILE Lastkey() <> K_ESC
      CLS
      ? "Press ESC to QUIT"

      @ 3,0 SAY "Enter Customer ID (1-22)" GET nCustomerID
      READ

      IF Lastkey() == K_ESC
         EXIT
      ENDIF

      xCustData := GetCustomerData( nCustomerID, ;
                                    cServerName, ;
                                    nPortNumber  )
      CLS
      ? "  Customer ID:", nCustomerID
      ? "Customer data:"

      IF Valtype( xCustData ) == "A"
         AEval( xCustData, {|x| Qout( ">", x ) } )
      ELSE
         ? xCustData
      ENDIF

      WAIT
   ENDDO
RETURN



/*
 * Retrieve customer data from a database managed by a server process
 */
FUNCTION GetCustomerData( nCustomerID, cServer, nPort )
   LOCAL nSocket
   LOCAL aInstruct
   LOCAL cBuffer
   LOCAL cLength
   LOCAL nBytes

   LOCAL nError := NIL

   /*
    * Create a bound socket to communicate with the server process
    */
   nSocket := SocketOpen( SOCK_STREAM, cServer, nPort, @nError )

   IF nError <> 0
      RETURN "ERROR:" + LTrim(Str(nError))
   ENDIF

   /*
    * This is an array of code blocks.
    * The code blocks are user-defined and evaluated in the server process!
    */
   aInstruct := InstructServer( PadL( nCustomerID, 6 ) )

   /*
    * Convert the array of code blocks to binary data and
    * obtain length of binary string
    */
   cBuffer   := Var2Bin( aInstruct )
   cLength   := U2Bin( Len( cBuffer ) )

   /*
    * Transmit binary data to the server process.
    * The first four bytes tell the length of the binary string.
    */
   SocketSend( nSocket, cLength + cBuffer )

   /*
    * The server process creates a result from the code blocks.
    * Obtain length of binary data created in the server process.
    * NOTE: SocketRecv() blocks the client process until the server
    *       has created the result.
    */
   nBytes    := SocketRecv( nSocket, @cLength, 4, ,@nError )

   IF nError <> 0 .OR. nBytes <> 4
      SocketClose( nSocket )
      cBuffer := "ERROR:" + LTrim(Str(nError)) + " BYTES RECEIVED:" + LTrim(Str(nBytes))
      RETURN cBuffer
   ENDIF

   /*
    * Retrieve binary data created in the server process from socket
    */
   nBytes  := Bin2U( cLength )
   cBuffer := Space( nBytes )
   nBytes  := SocketRecv( nSocket, @cBuffer, nBytes, ,@nError )

   SocketClose( nSocket )

   IF nError <> 0
      cBuffer := "ERROR:" + LTrim(Str(nError)) + " BYTES RECEIVED:" + LTrim(Str(nBytes))
      RETURN cBuffer
   ELSEIF  "ERROR" $ cBuffer
      RETURN cBuffer
   ENDIF

   /*
    * Return the result created in the server process to calling function
    */
RETURN Bin2Var( cBuffer )



/*
 * This function simply creates a 2-column array of logical values and
 * code blocks:  {{ lIsReturnValue, bCodeblock }}
 *
 * The array is transferred to a server process (Xbase++ at its best!)
 *
 * 1. The code blocks in the second column are evaluated in the server process
 * 2. When the element in the first column contains .T., the return value
 *    of the associated code block is sent back to the client process.
 *
 * In the code below, the client process defines function Scatter() in
 * a code block and wants to receive its return value from the server
 * process. The value .T. in the first column causes the server process
 * to return the value of function Scatter() to the client process.
 */
FUNCTION InstructServer( cCustID )
RETURN { ;
  { .F., {|| DbUseArea( .T., "DBFNTX", "Customer" ) } }, ;
  { .F., {|| OrdListAdd( "CUSTA.NTX" )              } }, ;
  { .F., {|| DbSeek( cCustID )                      } }, ;
  { .T., {|| Scatter()                              } }, ;
  { .F., {|| DbCloseArea()                          } }  ;
}
