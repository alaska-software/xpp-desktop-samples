//////////////////////////////////////////////////////////////////////
//
//  HTTPCTR.PRG
//
//  Copyright:
//     Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//     This class implements a service control application for the
//     AlaskaHttp sample service.
//     For a description just enter httpctr on the commandline.
//   
//  Remarks:
//   
//  Syntax:
//     httpctr cMessage [cUserName, cPassword]
//   
//     cMessage  -> the control request to be sent to the service.
//     cUserName -> When the service is installed, this parameter
//                  contains the username under which the service is
//                  started, i.e: "machinename\accountname"
//                  When the service is installed on the local machine
//                  following notation may be used: ".\accountname"
//     cPassword -> When cUserName is not empty this must include the
//                  login password of the user.
//  Return:
//     This application returns messages which are catched by the class
//     Logger().
//   
//   
//////////////////////////////////////////////////////////////////////


#include"service.ch"
#include"os.ch"
#include"simpleio.ch"

#define CRLF Chr( 13 ) + Chr( 10 )

PROCEDURE AppSys()
RETURN

//
// Test whether the required Asinet1c.dll is present
//
FUNCTION ChkForDllAndVer()
  LOCAL lRet, cFamily, cFullName

  lRet := .T.

  IF 0 != DllLoad( "Asinet1c" )
    DllUnload( "Asinet1c" )
  ELSE
    OutStd( "Required Asinet1c.dll not found" + CRLF )
    OutStd( "Asinet is part of the Professional Subscription" )
    lRet := .F.
  ENDIF

  cFamily := Os( OS_FAMILY )
  IF "WIN9X" == cFamily
    cFullName := OS( OS_FULLNAME )
    OutStd( cFullName + " does not support services" )
    lRet := .F.
  ENDIF

RETURN lRet


PROCEDURE PrintUsage()

  local cTxt

TEXT INTO cTxt WRAP
  This Applikation installs and starts the service sample
  
  Usage:
  
    httpctr <iusxcpoa> [".\AccountName" "Password"]
  
  Parameter:

    i installs the service httpservice.exe
      When the service shall be installed, parameter
      AccountName and Password become relevant. If
      one of these parameters are not passed, the service
      will be installed under the account LOCAL ACCOUNT.
      ( Note: Point and Backslash (".\") must prefix
        the Account Name )
    u uninstall service
    s start service
    x stop service
    c continue service
    p pause service
    o print status
    a print names of all installed sevices

  Debugging:

    For debugging the service, start it with the debugger, eg:

      XppDbg HttpService.exe
  
ENDTEXT

  OutStd( cTxt )

RETURN


CLASS Logger
  EXPORTED:
    INLINE METHOD write( cMsg )
      OutStd( cMsg )
    RETURN SELF
ENDCLASS

//
// Subclass a Controller class, so we are able
// to overwrite the :logError() method
//
CLASS HttpCtr from ServiceController
  EXPORTED:
    METHOD logError
ENDCLASS

//
// Whenever an error occurs, it can be handled in
// the method :logError()
//
METHOD HttpCtr:logError( nMsg )

  LOCAL cLastErr

  IF ::oLog == NIL
    RETURN self
  ENDIF

  IF ! IsMethod( ::oLog, "write" )
    RETURN self
  ENDIF

  cLastErr := DosErrorMessage( ::getLastError() )

  // Here the behaviour of the error condition
  // is implemented
  DO CASE
    CASE nMsg == SERVICE_MSG_CREATE_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_DELETE_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_START_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_PAUSE_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_STOP_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_CONTINUE_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_INTERROGATE_ERROR
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_WIN9X
      OutStd( cLastErr )
    CASE nMsg == SERVICE_MSG_ALLREADYINSTALLED_ERROR
      OutStd( cLastErr )
  ENDCASE

RETURN self


//
// This program is able to control and query a service.
// It is assumed that this control utility HTTPCTR.EXE
// is located in the same location as the service itself.
//
PROCEDURE Main( cControl, cUserName, cPassword )

  LOCAL cServiceName, cDisplayName
  LOCAL cServicePathName
  LOCAL oLog, oControl, aNames, lOk, cStatus, cStatusTest

  lOk := .T.

  IF ! ChkForDllAndVer()
    RETURN
  ENDIF

  IF "U" == Valtype( cControl )
    PrintUsage()
    RETURN
  ENDIF

  IIf( Valtype( cUserName ) != "C", cUserName := "", )
  IIf( Valtype( cPassword ) != "C", cPassword := "", )

  cServiceName     := "AlaskaHttp"
  cDisplayName     := "Alaska Software HTTP Service"

  cServicePathName := CurDrive() + ":\" + CurDir()+"\HttpService.exe"

  oLog := Logger():new()

  //
  // This defines the behaviour of the service.
  //
  HttpCtr():addController( cServiceName,     ;
                           cDisplayName,     ;
                           cServicePathName, ;
                           cUserName,        ;
                           cPassword,        ;
                           ,                 ;
                           oLog  )


  //
  // Control the service according to the passed parameters.
  //
  DO CASE
    // Install the service
    CASE cControl == "i"
      IF Empty( cUserName ) .OR. Empty( cPassword )
        OutStd( "You have not passed an account name and/or password." + CRLF )
        OutStd( "The service will be installed with LOCAL ACCOUNT rights." + CRLF )
      ENDIF
      OutStd( "installing..." + CRLF )
      lOk := HttpCtr():install( cServiceName )
    // Uninstall service
    CASE cControl == "u"
      OutStd( "uninstalling..." + CRLF )
      lOk := HttpCtr():uninstall( cServiceName )
    // Start the service
    CASE cControl == "s"
      OutStd( "starting..." + CRLF )
      lOk := HttpCtr():start( cServiceName )
    // Stop service
    CASE cControl == "x"
      OutStd( "stopping..." + CRLF )
      lOk := HttpCtr():stop( cServiceName )
    // Continue service
    CASE cControl == "c"
      OutStd( "continue..." + CRLF )
      lOk := HttpCtr():Continue( cServiceName )
    // Pause service
    CASE cControl == "p"
      OutStd( "pausing..." + CRLF )
      lOk := HttpCtr():pause( cServiceName )
    // Print service status
    CASE cControl == "o"
      oControl := HttpCtr():getUpdatedControl( cServiceName ) 
      cStatus := Var2Char( oControl:serviceType ) + CRLF
      cStatus += Var2Char( oControl:currentState ) + CRLF
      cStatus += Var2Char( oControl:controlsAccepted ) + CRLF
      cStatus += Var2Char( oControl:startType ) + CRLF
      cStatus += Var2Char( oControl:errorControl ) + CRLF
      cStatus += Var2Char( oControl:pathName ) + CRLF
      cStatus += Var2Char( oControl:startName ) + CRLF
      cStatus += Var2Char( oControl:displayName )

      cStatusTest := cStatus

      cStatusTest := StrTran( cStatusTest, "NIL" )
      cStatusTest := StrTran( cStatusTest, CRLF )

      IF Empty( cStatusTest )
        OutStd( "The service seems not to be installed" + CRLF )
      ELSE
        OutStd( cStatus )
      ENDIF
    // Print names of all installed services
    CASE cControl == "a"
      aNames := Array( 0 )
      ServiceQueryNames( aNames )
      OutStd( aNames )
    OTHERWISE
      PrintUsage()
  END CASE

  IIF( lOk, OutStd( "OK" ), )

RETURN

