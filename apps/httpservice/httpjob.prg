//////////////////////////////////////////////////////////////////////
//
//  HTTPJOB.PRG
//
//  Copyright:
//     Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//     This program implements a thread which performs the communication with a
//     http client ( web browser ).
//     A detailed description of the Http protocol can be found at
//     http://www.w3c.org.
//   
//  Remarks:
//     Please note that HttpJob is derived from the Thread class.
//     This means that as soon as the :start() method is called,
//     the method :execute() is running in a new thread.
//   
//   
//////////////////////////////////////////////////////////////////////


#include "socket.ch"

#define BUFLEN          1024
#define CRLF            Chr( 13 ) + Chr( 10 )
#define SP              Chr( 32 )
#define TAB             Chr( 9 )
#define HTTPGET         "GET"
#define DEFAULTFILE     "index.html"
#define SLASH           "/"
#define HTTPOK          "HTTP/1.1 200 OK" + CRLF
#define HTTPERROR       "HTTP/1.1 404 Not Found" + CRLF


CLASS HttpJob FROM Thread

  PROTECTED:
    VAR nJobSocket
    VAR cHttpRequest
    VAR cHttpResponse
    VAR cHttpMethod
    VAR cResponseFile
    VAR cRootDirectory

    METHOD getHttpRequest
    METHOD getHttpResponse
    METHOD setHttpMethod
    METHOD setResponseFile
    METHOD sendHttpResponse
    METHOD sendError

  EXPORTED:
    METHOD INIT
    METHOD execute
    METHOD destroy

ENDCLASS

//
// When a httpjob is created, it needs the socket and
// the location where to search for Html files.
//
METHOD HttpJob:INIT( nSocket, cRoot )

  ::cRootDirectory := cRoot

  ::nJobSocket := nSocket

  ::Thread:INIT()
  ::setInterval( NIL )

RETURN SELF

//
// This methd destroyes the socket resource
//
METHOD HttpJob:destroy()
  SocketClose( ::nJobSocket )
  ::nJobSocket := INVALID_SOCKET
RETURN SELF

//
// This method performs the entire communication with the
// Http client. It extracts the Http request, loads the
// response and returns it to the client.
//
METHOD HttpJob:execute(  )

  IF ! ::getHttpRequest( )
    RETURN ::destroy()
  ENDIF

  ::setHttpMethod( )

  //
  // Handle Http GET requests only!!
  //
  IF HTTPGET != ::cHttpMethod
    ::sendError( "HttpMethod invalid" )
    RETURN ::destroy()
  ENDIF

  IF ! ::getHttpResponse( )
    ::sendError( "HttpResponse not found" )
    RETURN ::destroy()
  ENDIF

  IF ! ::sendHttpResponse( )
    ::sendError( "Could not send http response" )
    RETURN ::destroy()
  ENDIF

RETURN ::destroy()

//
// Read the http request from the client and store
// it in ::cHttpRequest.
//
METHOD HttpJob:getHttpRequest( )

  LOCAL cBuffer

  ::cHttpRequest := ""
  cBuffer := Space( BUFLEN )

  DO WHILE 0 != SocketRecv( ::nJobSocket, @cBuffer )
    ::cHttpRequest += cBuffer
    cBuffer := Space( BUFLEN )
  ENDDO

  ::cHttpRequest := Alltrim( ::cHttpRequest )

RETURN ! Empty( ::cHttpRequest )

//
// Lod Http response according to the request.
//
METHOD HttpJob:getHttpResponse(  )

  ::cHttpResponse := ""

  IF ! ::setResponseFile(  )
    RETURN .F.
  ENDIF

  ::cHttpResponse := MemoRead( ::cResponseFile )

RETURN ! Empty( ::cHttpResponse )

//
// As soon as an error occurs, an error message is returned
// to the client.
//
METHOD HttpJob:sendError( cString )
  LOCAL cError
  cError := MemoRead( ::cRootDirectory + SLASH + "error.html" )
  cError := HTTPERROR + CRLF + cError + CRLF + cString
RETURN 0 != SocketSend( ::nJobSocket, cError )

//
// It may be assuemed that the response is stored in ::cHttpResponse.
// So send this one to the client.
//
METHOD HttpJob:sendHttpResponse(  )
  ::cHttpResponse := HTTPOK + CRLF + ::cHttpResponse + CRLF
RETURN 0 != SocketSend( ::nJobSocket, ::cHttpResponse )

//
// We assume that the http request starts with
// "GET /resourcetoload.html HTTP/1.1"
// The filename is extracted and stored in
// ::cResponseFile
//
METHOD HttpJob:setResponseFile(  )

  LOCAL cFile, nStart, nEnd

  ::cResponseFile := ""

  IF 0 == ( nStart := At( SP, ::cHttpRequest ) )
    RETURN .F.
  ENDIF
  nStart++

  IF 0 == ( nEnd := At( SP, ::cHttpRequest, nStart ) )
    RETURN .F.
  ENDIF

  cFile := SubStr( ::cHttpRequest, nStart, nEnd - nStart )

  IF Empty( cFile ) .OR. cFile == SLASH
    cFile := SLASH + DEFAULTFILE
  ENDIF

  cFile := ::cRootDirectory + cFile

  cFile := StrTran( cFile, "\", SLASH )

  // Assure that no file is requested which is not below
  // the root directory
  cFile := StrTran( cFile, "/..", "" )
  cFile := StrTran( cFile, "../", "" )

  IF ! FILE( cFile )
    Logger():log( ::cResponseFile + " is requested but cannot be found." )
    RETURN .F.
  ENDIF

  ::cResponseFile := cFile

  Logger():log( "HttpResponse is " + ::cResponseFile )

RETURN .T.

//
// Extract the http method from the http request and store it
// in ::cHttpMethod.
//
METHOD HttpJob:setHttpMethod( )

  LOCAL nPos

  ::cHttpMethod := ""
  IF 0 == ( nPos := At( SP, ::cHttpRequest ) )
    RETURN SELF
  ENDIF
  ::cHttpMethod := Upper( SubStr( ::cHttpRequest, 0, nPos - 1 ) )

RETURN SELF
