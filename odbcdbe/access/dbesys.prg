//////////////////////////////////////////////////////////////////////
//
//  DBESYS.PRG
//
//  Copyright:
//      Alaska Software, (c) 1998-2025. All rights reserved.         
//  
//  Contents:
//      Xbase++ DatabaseEngine startup/preloader for ODBC
//   
//  Syntax:
//      DbeSys() is called automatically at program start before the 
//      function MAIN.
//   
//////////////////////////////////////////////////////////////////////



#define MSG_DBE_NOT_LOADED   " database engine not loaded"
#define MSG_DBE_NOT_CREATED  " database engine could not be created"

*******************************************************************************
* DbeSys() is always executed at program startup
*******************************************************************************
PROCEDURE dbeSys()
/* 
 *   The lHidden parameter is set to .T. for all database engines 
 *   which will be combined to a new abstract database engine.
 */
LOCAL aDbes := { { "DBFDBE", .T.},;
                 { "NTXDBE", .T.},;
                 { "DELDBE", .F.},;
                 { "SDFDBE", .F.},;
                 { "ODBCDBE", .F.} }
LOCAL aBuild :={ { "DBFNTX", 1, 2 } } 
LOCAL i

  /*
   *   Set the sorting order and the date format
   */
  SET COLLATION TO AMERICAN
  SET DATE TO AMERICAN

  /* 
   *   load all database engines 
   */
  FOR i:= 1 TO len(aDbes)
      IF ! DbeLoad( aDbes[i][1], aDbes[i][2])
         Alert( aDbes[i][1] + MSG_DBE_NOT_LOADED , {"OK"} )
      ENDIF
  NEXT i

  /* 
   *   create database engines 
   */
  FOR i:= 1 TO len(aBuild)
      IF ! DbeBuild( aBuild[i][1], aDbes[aBuild[i][2]][1], aDbes[aBuild[i][3]][1])
         Alert( aBuild[i][1] + MSG_DBE_NOT_CREATED , {"OK"} )
      ENDIF
  NEXT i
          
  DbeSetDefault("ODBCDBE")
       
RETURN

//
// EOF
