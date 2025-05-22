//////////////////////////////////////////////////////////////////////
//
//  LOGGER.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  

//  Contents:
//     This class implements a simple logging mechanism.
//     Please note that the :log() method is a class method.
//     This means that it can be called from anywhere in the
//     server code just by Logger():log( "your log message" )
//   
//   
//  Remarks:
//     Logger implements a helper class to perform output from a service.
//   
//   
//////////////////////////////////////////////////////////////////////


#include "fileio.ch"

#define CRLF Chr( 13 ) + Chr( 10 )

CLASS Logger
  PROTECTED:
    CLASS VAR cFileName
  EXPORTED:
    CLASS METHOD initClass
    CLASS METHOD log
ENDCLASS

CLASS METHOD Logger:initClass( cFile )

  ::cFileName   := cFile
  DbCreate( ::cFileName, {{"MESSAGE", "C", 100, 0}} )

RETURN

CLASS METHOD Logger:log( cMsgLine )

  LOCAL nPos

  DbUseArea( .T., , ::cFileName, "logdb", .T., .F. )

  IF ! Used()
    RETURN SELF
  ENDIF

  logdb->( DbAppend( ) )
  nPos := logdb->( FieldPos( "MESSAGE" ) )
  logdb->( FieldPut( nPos, cMsgLine ) )
  CLOSE logdb

RETURN SELF
