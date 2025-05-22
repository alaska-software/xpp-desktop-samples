//////////////////////////////////////////////////////////////////////
//
//  SENDFILE.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       File send functions.
//   
//  Remarks:
//       For a 'real world' application the original X-Y-or Z-modem protocols
//       should be used. Here we only want to demonstrate the use of the
//       XbTools COM functions.
//   
//////////////////////////////////////////////////////////////////////


#include "fileio.ch"
#include "xbtcom.ch"
#include "transmit.ch"

******************************************************************************
* Send a file via serial communication.
*
* Returns:
*     - SENDER_OK, if file has been sent completely
*     - SENDER_CANNOT_CONNECT, if no receiver is waiting on the other end
*     - SENDER_TRANSMIT, if a block could not be sent.
******************************************************************************
FUNCTION sendFile( cFileName, ;
                   nComPort , ;        // COM port to send data to
                   nBaudRate, ;        // Transmission speed
                   cParity  , ;        // Parity
                   nDataBits, ;        // Number of data bits
                   nStopBits, ;        // Number of stop bits
                   lUseCrc  , ;        // Use CRC instead of 8 bit checksum
                   bProgress  )        // Evaluated whenever a block is sent

   LOCAL nFileHandle, nFileLen, nErrorCode, nBytesRead, nErrorCount, nBlockNum
   LOCAL cBuffer   := Replicate( S_EOF, 1024 )
   LOCAL nBytesSent:= 0
   LOCAL cLine     := ""

   nFileHandle := FOpen( cFileName, FO_READ )
   IF nFileHandle == -1                // Unable to open file
      RETURN SENDER_CANNOT_OPEN_FILE   // ** RETURN **
   ENDIF
                                       // Open COM port, UART assumed.
   IF ! COM_OPEN( nComPort,,,,COM_OPEN_FASTUART )
      FClose( nFileHandle )            // Error opening COM port
      RETURN SENDER_CANNOT_OPEN_PORT   // ** RETURN **
   ENDIF                               // Initialize COM port with
                                       // transmission options
   IF ! COM_INIT( nComPort, nBaudRate, cParity, nDataBits, nStopBits )
      FClose(nFileHandle)
      RETURN SENDER_CANNOT_INIT_PORT
   ENDIF

   COM_HARD( nComPort, .T. )           // Use hardware handshake.
   COM_SOFT( nComPort, .F. )

   FSeek( nFileHandle, 0, FS_SET )     // Get file size
   nFileLen := FSeek( nFileHandle, 0, FS_END)
   FSeek( nFileHandle, 0, FS_SET )
                                       // Set one second wait-for-something
                                       // read timeout...
   COM_READMODE( nComPort, WAIT_READ_TIMEOUT, 100 )
                                       // ...and 20 seconds write timeout.
   COM_SENDMODE( nComPort, WRITE_TIMEOUT, 2000 )

   COM_FLUSH( nComPort )               // Flush send buffer.

   Message( "Waiting for receiver ..." )
   COM_SEND( nComPort, S_ACK )         // Send an ACK = I am ready to send

   nErrorCount := 0                    // Wait for a ACK from receiver.
   DO WHILE ( cLine := COM_READ( nComPort, 1) ) != S_ACK .AND. ;
            nErrorCount++ < 10
   ENDDO

   IF cLine != S_ACK                   // Receiver did not acknowlege
      Message( "Timeout while waiting for receiver" )
      FClose( nFileHandle )
      COM_CLOSE( nComPort )
      RETURN SENDER_CANNOT_CONNECT
   ENDIF

   Message( "Connected to receiver" )  // Something is at the other end.
                                       // Prepare first block:
   cFileName := FileName( cFileName )  // - Do not send path
   cFileName += Chr(0)                 // - Delimiter for file name
   cFileName += L2Bin( nFileLen )      // - Include file size
   nBlockNum := 1

   IF ! SendBlock( nComPort , cFileName, 1, lUseCrc )
      FClose( nFileHandle )            // Sending first block failed
      COM_CLOSE( nComPort )            // Close everything and
      RETURN SENDER_TRANSMIT           // ** RETURN **
   ENDIF

   nBlockNum ++                        // Read file in blocks of 1024 bytes.
   DO WHILE ( nBytesRead := Fread(nFileHandle, @cBuffer, 1024) ) != 0

      Message( "Bytes sent: " + LTrim( Str(nBytesSent) ) + " ("+ ;
                       LTrim( Str(nFileLen-nBytesSent) ) +  ")"  )

                                       // Build a XMODEM block and send it.
      IF ! SendBlock( nComPort, cBuffer, nBlockNum++, lUseCrc )
         FClose( nFileHandle )         // Sending block failed
         COM_CLOSE( nComPort )         // Close everything and
         RETURN SENDER_TRANSMIT        // ** RETURN **
      ENDIF

      IF bProgress != NIL              // Show progress, if there is a codeblock.
         Eval( bProgress, nFileLen, nBytesSent += Len(cBuffer) )
      ENDIF
      cBuffer := Replicate( S_EOF, 1024 )
   ENDDO

   COM_SEND( nComPort, S_EOT )         // We're done. Send EOT...
   Message( "File sent successfully" )

   FClose( nFileHandle )               // ...and do the cleanup
   COM_CLOSE( nComPort )

RETURN SENDER_OK

******************************************************************************
* Send one block.
*
* <nComPort>  - COM port; must be open and initialized
* <cBlock>    - the data to be sent
* <nBlockNum> - the number of the block
*
* Returns .T. if block could be sent and .f. otherwise.
******************************************************************************
FUNCTION SendBlock( nComPort, cBlock, nBlockNum, lUseCRC )
   LOCAL nBytesSend, cLine
   LOCAL nError :=  0
   LOCAL lError := .T.

   IF Len( cBlock ) < 128              // Pad block with EOFs to correct size
      cBlock += Replicate( S_EOF, 128-Len(cBlock) )
   ELSEIF Len( cBlock ) < 1024
      cBlock += Replicate( S_EOF, 1024-Len(cBlock) )
   ENDIF
                                       // Build an XMODEM block.
   cBlock := XMOBLOCK( cBlock, nBlockNum, lUseCRC, IIf(Len(cBlock)==128, 1, 2) )

   DO WHILE lError .AND. nError < 10   // Try max. 10 times to send block
      IF COM_SEND( nComPort, cBlock ) != 0
         lError := .T.
         nError++
         LOOP                          // Sending failed
      ENDIF

      cLine := COM_READ( nComPort, 1 ) // Wait for response of receiver.

      COM_FLUSH( nComPort )            // Flush receive buffer

      IF Empty( cLine )                // No response from receiver
         COM_SFLUSH( nComPort )        // Flush send buffer
         lError := .T.
         nError ++                     // Increment error and retry.
      ELSEIF Left( cLine, 1 )== S_NAK  // Receiver did not understand
         COM_SFLUSH( nComPort )        // Flush send buffer.
         lError := .T.
         nError ++                     // Increment error and retry.
         Tone( 1000, 2 )               // Note: there was a transmission error
         Message( "Receiver has a problem with block "+AllTrim(Str(nBlockNum)))
      ELSEIF Left( cLine, 1 )== S_ACK  // Receiver is ready to receive
         lError := .F.
      ENDIF
   ENDDO

RETURN nError < 10


******************************************************************************
* Get filename from a path\filename string.
******************************************************************************
FUNCTION fileName(cFileName)
   LOCAL  n := RAT("\", cFileName)

   IF n != 0
      RETURN SubStr( cFileName, n+1 )
   ENDIF
RETURN cFileName
