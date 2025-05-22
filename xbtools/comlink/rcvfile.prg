//////////////////////////////////////////////////////////////////////
//
//  RCVFILE.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       File receive functions.
//   
//  Remarks:
//       For a 'real world' application the original X-Y-or Z-modem protocols
//       should be used. Here we only want to demonstrate the use of the
//       XbTools COM functions.
//   
//////////////////////////////////////////////////////////////////////


#include "fileio.ch"
#include "common.ch"
#include "xbtcom.ch"
#include "transmit.ch"

******************************************************************************
* Initialize COM port and wait for an ACKnowledge of a sender to start 
* receiving a file. Exit if <cSyncVar> contains .F.
*
* Note: This function is executed in a separate thread
******************************************************************************
FUNCTION autoReceiveFile( cSyncVar , ; // Name of PUBLIC synchronization variable
                          nComPort , ; // COM port to monitor
                          nBaudRate, ; // Transmission speed
                          cParity  , ; // Parity
                          nDataBits, ; // Number of data bits
                          nStopBits, ; // Number of stop bits
                          lUseCrc  , ; // Use CRC instead of 8 bit checksum
                          bProgress, ; // Evaluated whenever data arrived
                          bFileExists) // Evaluated when file exists locally.
                                       // It must return a file name
   LOCAL nError

                                       // Open COM port, UART assumed.
   IF ! COM_OPEN(nComPort,,,,COM_OPEN_FASTUART)
      RETURN RECEIVER_CANNOT_OPEN_PORT
   ENDIF                               // Initialize COM port with
                                       // transmission options
   IF ! COM_INIT( nComPort, nBaudRate, cParity, nDataBits, nStopBits )
      COM_CLOSE( nComPort )            // Inititialization failed
      RETURN RECEIVER_CANNOT_INIT_PORT // ** RETURN **
   ENDIF

   COM_HARD( nComPort, .T. )           // Use hardware handshake.
   COM_SOFT( nComPort, .F. )
                                       // Set one second normal read timeout
                                       // and 20 seconds write timeout.
   COM_READMODE( nComPort, READ_TIMEOUT , 100  )
   COM_SENDMODE( nComPort, WRITE_TIMEOUT, 2000 )

   COM_FLUSH ( nComPort )              // Flush send and receive buffer.
   COM_SFLUSH( nComPort )

   DO WHILE &cSyncVar                  // Until sync variable contains .T. ...
                                       // Test, if an ACK arrived.
      IF COM_READ( nComPort, 1) == S_ACK
         COM_SEND( nComPort, S_ACK )   // Send an ACK to let sender
                                       // start transmission.
         nError := ReceiveFile( nComPort, lUseCrc, bProgress, bFileExists )

         IF nError != RECEIVER_OK      // Something went wrong
            MsgBox("Error "+Str(nError,2)+" while receiving file")
         ENDIF

         Message( "Auto-Receive-Mode" , 1 )
         Message( "Receiver started" )
         ResetProgress()
      ENDIF
      Sleep(100)                       // No data arrived, sleep a second
   ENDDO

   COM_CLOSE( nComPort )

RETURN RECEIVER_OK

******************************************************************************
* Receive one file 
******************************************************************************
FUNCTION receiveFile( nComPort, lUseCrc, bProgress, bFileExists )
   LOCAL cBlock, cFileName, nFileHandle, nErrorCount
   LOCAL nCurBlockNum, nBytesToWrite, nFileLen, n
   LOCAL nBlockNum     := 2
   LOCAL nBytesWritten := 0

   Message( "Waiting for filename ..." )

   nErrorCount := 0
   cFileName   := ""

   DO WHILE Len( cFileName ) == 0 .AND. nErrorCount < 10
      // Try to read block #1 that contains the filename.
      cBlock    := readBlock( nComPort, lUseCrc)
      cFileName := GetFileName( cBlock , @nFileLen, lUseCrc)

      IF Len( cFileName ) == 0
         COM_SEND( nComPort, S_NAK )  // Send a NAK if filename 
         nErrorCount++                // is not received correctly.
      ENDIF
   ENDDO

   IF Len(cFileName) == 0 .OR. ;      // Return if Len(filename) == 0
      nErrorCount    >= 10            // or too many errors. 
      RETURN RECEIVER_ERROR_READING_FILENAME
   ENDIF

   IF File( cFileName ) .AND. bFileExists != NIL
      // If file to receive exists locally and there is a
      // codeblock to inform user, execute this codeblock and use
      // its return value for the file name
      cFileName := Eval( bFileExists, cFileName )
   ENDIF

   
   nFileHandle := ;                    // Create a file to write
     FCreate( cFileName, FC_NORMAL )   // received data to

   IF nFileHandle == -1
      RETURN RECEIVER_CANNOT_OPEN_FILE
   ENDIF

   COM_SEND( nComPort, S_ACK )         // Acknowledge filename to sender.

   Message( "Receiving file "+cFileName, 1 )

   nErrorCount := 0
   DO WHILE nErrorCount < 10
      cBlock := ;                      // Try to get a block.
         readBlock( nComPort, lUseCrc )

      IF Left( cBlock, 1 ) == S_EOT    // This is the end of the file.
         FClose(nFileHandle)       
         RETURN RECEIVER_OK            // Got the file ** RETURN **
      ENDIF

      nCurBlockNum := ;                // Check block and block number
         XMOCHECK( cBlock, lUseCRC )

      DO CASE
      CASE nCurBlockNum == -1          // Block is invalid; send a NAK
         COM_SEND( nComPort, S_NAK )
         Sleep( 100 )                  // Sleep, so sender can receive NAK
         COM_FLUSH( nComPort )         // Clear buffer
         nErrorCount++
         Tone( 500, 2 )

      CASE nCurBlockNum == nBlockNum   // Block is okay; send an ACK.
         COM_SEND( nComPort, S_ACK )
         nBytesToWrite := IIf( Asc(cBlock) == 1, 128, 1024 )

         IF nBytesWritten < nFileLen   // Write received data to file             
            n := Min( nBytesToWrite, nFileLen-nBytesWritten )
            FWrite( nFileHandle, SubStr(cBlock, 4), n )
            nBytesWritten += n
         ENDIF

         IF bProgress <> NIL           // Evaluate progress codeblock
            Eval( bProgress, nFileLen, nBytesWritten )
         ENDIF

         Message( "Bytes received: "+ LTrim(Str(nBytesWritten))+ ;
                  " ("+LTrim(Str(nFileLen-nBytesWritten))+")")

         nBlockNum++
         nBlockNum   %= 256            // XMODEM block number range: 0 - 255 !
         nErrorCount := 0

      CASE nCurBlockNum+1 == nBlockNum
         COM_SEND(nComPort, S_ACK)     // Last block has been received again.
         nErrorCount := 0              // This is not considered to be an
                                       // error, just send an ACK.
      OTHERWISE                        
         COM_SEND(nComPort, S_NAK)     // Block number out of range, Send a NAK.
         Sleep( 100 )                  // Sleep, so sender can receive NAK
         COM_FLUSH( nComPort )         // Clear buffer
         nErrorCount++
         Tone(500, 2)
      ENDCASE
   ENDDO

   FClose(nFileHandle)
RETURN RECEIVER_TOO_MANY_ERRORS


******************************************************************************
* Get filename from first block received.
******************************************************************************
STATIC FUNCTION GetFileName( cBlock, nFileLen, lUseCRC )
   LOCAL nBlockNum, nPos, cFileName

   nBlockNum := ;                      // Get block number. 
      XMOCHECK( cBlock, lUseCRC )

   IF nBlockNum != 1
      RETURN ""                        // Not the first block ** RETURN **
   ENDIF

   cBlock    := SubStr(cBlock, 4)      // Strip XMODEM header.
   nPos      := AT( Chr(0), cBlock )   // Get filename. 
   cFileName := Left(cBlock, nPos-1)
   nFileLen  := ;                      //  Get length of file.
      Bin2L( SubStr(cBlock,nPos+1,4) )

RETURN AllTrim( cFileName )


******************************************************************************
* Read a block from COM port.
*
* The function returns a zero-length string if a timeout or error
* condition occurs, a XMODEM block if one is available, or S_EOT 
* when a single EOT arrives.
******************************************************************************
STATIC FUNCTION readBlock(nComPort, lUseCrc)
   LOCAL cFirst, cBlock, nLen

   cFirst := COM_READ( nComPort, 1 )   // Read first byte

   IF cFirst == S_SOH                  // XMODEM block, EOT or garbage ?
      nLen := 131+IIf( lUseCrc, 1, 0 )
   ELSEIF cFirst == S_STX
      nLen := 1027+IF(lUseCrc, 1, 0)
   ELSEIF cFirst == S_EOT
      RETURN S_EOT
   ELSE
      COM_READ( nComPort )
      RETURN ""                        // Garbage arrived ** RETURN **
   ENDIF

   cBlock := ;                         // Get rest of XMODEM block.
      COM_READ( nComPort, nLen )

RETURN cFirst+cBlock
