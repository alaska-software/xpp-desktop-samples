//////////////////////////////////////////////////////////////////////
//
//  LOGMAIL.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      Userdefined class LogWriter()
//   
//  Remarks:
//      This simple class is used to log transmission information while
//      sending or retrieving e-mails via SendMail() or ReceiveMail().
//   
//////////////////////////////////////////////////////////////////////


#include "Fileio.ch"

#define CRLF CHR(13)+CHR(10)

CLASS LogWriter
   VAR lIsError
   VAR aMessage
   VAR cLogFile

 EXPORTED:

   INLINE METHOD init( cLogFile )
      ::cLogFile := cLogFile
      ::aMessage := {}
      ::lIsError := .F.
   RETURN


   INLINE METHOD write( cMsg )
      AAdd( ::aMessage, cMsg )
   RETURN self


   INLINE METHOD logError( cMsg )
      ::lIsError := .T.
      ::write( cMsg )
   RETURN self


   INLINE METHOD isError
   RETURN ::lIsError


   INLINE METHOD writeLogFile
      LOCAL nHandle

      IF .NOT. FExists( ::cLogFile )
         nHandle := FCreate( ::cLogFile )
      ELSE
         nHandle := FOpen( ::cLogFile )
         FSeek( nHandle, 0, FS_END )
      ENDIF

      AEval( ::aMessage, {|cMsg| FWrite( nHandle, cMsg + CRLF ) } )
      FClose( nHandle )
   RETURN self

ENDCLASS

// EOF
