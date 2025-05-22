//////////////////////////////////////////////////////////////////////
//
//  EMAIL.CH
//
//  Copyright:
//       Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//       #define constants for MAIL.EXE
//   
//   
//////////////////////////////////////////////////////////////////////


#ifndef  _EMAIL_CH

#define  _EMAIL_CH

#define  CRLF  Chr(13)+Chr(10)


#define  ERR_MSG_NOERROR            "Transmission complete"
#define  ERR_MSG_USERNAME           "User name missing or illegal"
#define  ERR_MSG_PASSWORD           "Password missing or illegal"
#define  ERR_MSG_SENDER             "Sender address missing or illegal"
#define  ERR_MSG_SUBJECT            "Subject line missing or illegal"
#define  ERR_MSG_MESSAGE            "Message body missing or illegal"
#define  ERR_MSG_RECIPIENT          "Recipient address missing or illegal"
#define  ERR_MSG_CCHEADER           "CC header missing or illegal"
#define  ERR_MSG_BCCHEADER          "BCC header missing or illegal"
#define  ERR_MSG_ATTACHMENT         "File attachment missing or illegal"
#define  ERR_MSG_CONNECTION         "Connection to server failed"
#define  ERR_MSG_TRANSMISSION       "Message transmission failed"


// Constants for accessing elements of the array returned by ReceiveMail()
#define  EMAIL_FROM                 1
#define  EMAIL_SUBJECT              2
#define  EMAIL_CC                   3
#define  EMAIL_BCC                  4
#define  EMAIL_CONTENTS             5
#define  EMAIL_ATTACHMENT           6

#define  EMAIL_ARRAY                {,,,,{},{}}

#endif
