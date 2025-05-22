//////////////////////////////////////////////////////////////////////
//
//  TRANSMIT.CH
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Constants for serial communication
//////////////////////////////////////////////////////////////////////


#ifndef  _TRANSMIT_CH_
#define  _TRANSMIT_CH_

#define  ID_APPNAME   1

#define  S_SOH   CHR(1)                    // 
#define  S_STX   CHR(2)                    // 
#define  S_EOT   CHR(4)                    // End of transmission
#define  S_ACK   CHR(6)                    // Acknowledged
#define  S_NAK   CHR(21)                   // Not acknowleged
#define  S_CAN   CHR(24)                   // Cancel
#define  S_EOF   CHR(26)                   // End of file
#define  S_C      "C"                      // Letter C

#define  SENDER_OK                         0
#define  SENDER_CANNOT_OPEN_FILE           1
#define  SENDER_CANNOT_OPEN_PORT           2
#define  SENDER_CANNOT_INIT_PORT           3
#define  SENDER_CANNOT_CONNECT             4
#define  SENDER_TRANSMIT                   5

#define  RECEIVER_OK                       0
#define  RECEIVER_CANNOT_OPEN_PORT         1
#define  RECEIVER_CANNOT_INIT_PORT         2
#define  RECEIVER_ERROR_READING_FILENAME   3
#define  RECEIVER_CANNOT_OPEN_FILE         4
#define  RECEIVER_TOO_MANY_ERRORS          5


#endif
