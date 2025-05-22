//////////////////////////////////////////////////////////////////////
//
//  SIMPLECTR.PRG
//
//  Copyright:
//     Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//     Application for controlling the service simpleservice.exe
//   
//  Syntax:
//     simplectr <isxu> [".\AccountName" "Passwort"]
//   
//     Flags:
//   
//        i installs the service simpleservice.exe
//          When installing the service the account name and the
//          password of this account become mandatory parameters.
//          ( Note: Point and Backslash (".\") must prefix the
//                  account name )
//        s starts the service
//        x stopps the service
//        u deinstalls the service
//   
//     Debugging:
//   
//        To debug the service, start it with:
//   
//          XppDbg simpleservice.exe
//   
//////////////////////////////////////////////////////////////////////


#include "service.ch"
#include "os.ch"
#include "simpleio.ch"

#define CRLF Chr( 13 ) + Chr( 10 )

CLASS Logger 
  EXPORTED: 
    INLINE METHOD write( cMsg ) 
      OutStd( cMsg )
  RETURN SELF 
ENDCLASS 
 
PROCEDURE AppSys()
RETURN
 
FUNCTION ChkOsVersion()
  LOCAL cFamily, cFullName

  cFamily := Os( OS_FAMILY )
  IF "WIN9X" == cFamily
    cFullName := OS( OS_FULLNAME )
    OutStd( cFullName + " does not support services" )
    RETURN .F.
  ENDIF

RETURN .T.

PROCEDURE Main( cFlag, cUser, cPass ) 
  LOCAL cLocation, oLog, oCtrl, cServiceName, lOk

  lOk := .F.

  IF ! CheckParam( cFlag, cUser, cPass )
    Quit
  ENDIF

  IF ! ChkOsVersion()
    Quit
  ENDIF

  cServiceName := "SimpleService" 
  cLocation    := CurDrive() + ":\" + CurDir() + "\"  
  oLog         := Logger():new() 
  oCtrl        := ServiceController() 
 
  oCtrl:addController( cServiceName ,                       ; 
                       "Alaska Software Simple Service"   , ; 
                        cLocation + cServiceName + ".exe" , ; 
                       cUser, cPass,  /*parameter*/ , ; 
                       oLog ) 
  DO CASE 
  CASE cFlag == "i" 
    OutStd( "installing..." + CRLF )
    lOk := ServiceController():install( cServiceName ) 
  CASE cFlag == "s" 
    OutStd( "starting..." + CRLF )
    lOk := ServiceController():start( cServiceName ) 
  CASE cFlag == "x" 
    OutStd( "stopping..." + CRLF )
    lOk := ServiceController():stop( cServiceName ) 
  CASE cFlag == "u" 
    OutStd( "uninstalling..." + CRLF )
    lOk := ServiceController():uninstall( cServiceName ) 
  OTHERWISE
    Usage()
    Quit
  ENDCASE 

  IF lOk
    OutStd( "OK" )
  ENDIF

RETURN 

FUNCTION CheckParam( cFlag, cUser, cPass )

  IF ! "C" == Valtype( cFlag )
    Usage()
    RETURN .F.
  ENDIF

  IF ! cFlag $ "isxu"
    Usage()
    RETURN .F.
  ENDIF

  IF cFlag == "i"
    IF "U" == Valtype( cUser ) .OR. ;
       "U" == Valtype( cPass )
      Usage()
      RETURN .F.
    ENDIF
  ENDIF

RETURN .T.

PROCEDURE Usage()

  local cTxt

TEXT INTO cTxt WRAP
  Usage:

     simplectr <isxu> [".\AccountName" "Passwort"]

  Flags:

     i installs the service simpleservice.exe
       When installing the service the account name and the
       password of this account become mandatory parameters.
       ( Note: Point and Backslash (".\") must prefix the
               account name )
     s starts the service
     x stopps the service
     u deinstalls the service

  Debugging:

     To debug the service, start it with:

       XppDbg simpleservice.exe

ENDTEXT

  OutStd( cTxt )

RETURN
