//////////////////////////////////////////////////////////////////////
//
//  HTTPSERVICE.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  

//  Contents:
//     This is the implementation of a service.
//   
//   
//  Remarks:
//     Before the service can actually be started, we query the location
//     of the executable of the service. We need this information
//     for writing the logfile to a proper place and as information
//     from where files may be returned which are requested by a
//     http request.
//   
//     Debugging: When the application is running in the debugger, then
//                the method :main() of the class HttpServer will be called
//                directly. Nevertheless the service must be installed since
//                the service status is read.
//   
//  Syntax:
//   
//  Return:
//   
//   
//////////////////////////////////////////////////////////////////////


#include"service.ch"
#include"os.ch"

#define CRLF Chr( 13 ) + Chr( 10 )

PROCEDURE Main( cParam )

  LOCAL cRootDir, nPos, bError

  UNUSED( cParam )

  //
  // Usage of own error handling
  //
  bError := Errorblock( { |oError| ErrorLog(oError) } )

  //
  // The server root directory is the directory where the
  // executable of the server is.
  //
  cRootDir := AppName( .T. )
  nPos := Rat( "\", cRootDir )
  cRootDir := SubStr( cRootDir, 1, nPos - 1 )

  //
  // This calls the :initClass() method of our logger.
  //
  Logger( cRootDir + "\HttpAlaskaLog" )

  //
  // The Http server returns only files relative to the root directory.
  //
  HttpServer():setRootDirectory( cRootDir )

  //
  // Start the Http server.
  //
  HttpServer():start()

  Errorblock( bError )

RETURN

//
// When a runtime error occurs, error information is written to 
// system log with WriteCToLog() function.
//
FUNCTION ErrorLog( oError ) 
  LOCAL cString
  LOCAL i:=0 

  cString := ""
  i       := 0

  // Aufrufstack festhalten
  DO WHILE ! Empty( ProcName(++i) )
    cString += ProcName(i) + "(" + LTrim(Str(ProcLine(i))) +")" + CRLF
  ENDDO 
 
  // Collect information of error object
  IF Valtype( oError ) =="O"
    cString += CRLF

    IF Valtype( oError:Args )=="A"
       cString += "e:args         :" + CRLF
       AEval( oError:Args, ; 
              {|x| cString += " ->" + "VALTYPE:" + Valtype(x) + ", value:" + ;
               Var2Char(x) + CRLF } )
    ELSE
       cString += "e:args         : NIL" + CRLF
    ENDIF 

    cString += "e:canDefault   :" + ;
               IIF( oError:canDefault   , ".T.",  ".F." ) + CRLF

    cString += "e:canRetry     :" + ;
               IIF( oError:canRetry     , ".T.",  ".F." ) + CRLF

    cString += "e:canSubstitute:" + ;
               IIF( oError:canSubstitute, ".T.",  ".F." ) + CRLF


    cString += "e:description  :" + oError:description                 + CRLF
    cString += "e:filename     :" + oError:filename                    + CRLF
    cString += "e:genCode      :" + Alltrim( Str( oError:genCode ) )   + CRLF
    cString += "e:operation    :" + oError:operation                   + CRLF
    cString += "e:osCode       :" + Alltrim( Str(  oError:osCode ) )   + CRLF
    cString += "e:severity     :" + Alltrim( Str(  oError:severity ) ) + CRLF
    cString += "e:subCode      :" + Alltrim( Str( oError:subCode ) )   + CRLF
    cString += "e:subSystem    :" + oError:subSystem                   + CRLF
    cString += "e:thread       :" + Alltrim( Str( oError:thread ) )    + CRLF
    cString += "e:tries        :" + Alltrim( Str( oError:tries ) )     + CRLF
  ENDIF 
 
 
  ErrorLevel(1) 

  EventLogWriteStr( cString, "TestLog", , LOG_ERROR_TYPE )

  QUIT 

RETURN .F. 




