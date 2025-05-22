//////////////////////////////////////////////////////////////////////
//
//  SIMPLESERVICE.PRG
//
//  Copyright:
//     Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//     Implementation of the service simpleservice
//   
//  Remarks:
//     This service may not be executed directly.
//     For controlling, the application simplectr.exe may be used.
//     Use xppdbg.exe for debugging the service.
//   
//////////////////////////////////////////////////////////////////////

 
#include "service.ch" 

// Entry point of application
PROCEDURE Main() 
  MyService():start() 
RETURN 
 
CLASS MyService From ServiceApp 
  EXPORTED: 
    CLASS METHOD main 
    CLASS METHOD stop 
  HIDDEN: 
    CLASS VAR lRunning 
ENDCLASS  
 
// Entry point of service
CLASS METHOD MyService:main() 
  ::lRunning := .T. 
 
  DO WHILE ::lRunning 
    Tone( 400, 9 ) 
    Sleep( 500 ) 
  ENDDO 
RETURN self 
 
// Entry point for stop request
CLASS METHOD MyService:stop() 
  ::lRunning := .F. 
RETURN self 

