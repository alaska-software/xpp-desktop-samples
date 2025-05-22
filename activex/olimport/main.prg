//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//  Remarks:
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////


#pragma library( "ascom10.lib" )

#include "Appevent.ch"
#include "outlook.ch"

/// <summary>
/// <para>
/// Overloaded AppSys
/// </para>
/// </summary>
PROCEDURE AppSys()
  SET CHARSET TO ANSI
RETURN

/// <summary>
/// <para>
/// Initialize application
/// </para>
/// </summary>
INIT PROCEDURE AppInit()
  LOCAL oDlg

  oDlg := CreateContactDialog()
  SetAppWindow( oDlg )
RETURN

/// <summary>
/// <para>
/// Destroy allocated resources
/// </para>
/// </summary>
EXIT PROCEDURE AppExit()

  LOCAL oApp := SetAppWindow()
  oApp:destroy()

RETURN

/// <summary>
/// <para>
/// Overloaded DbeSys
/// </para>
/// </summary>
PROCEDURE DBESys()
  IF ! DbeLoad( "FOXDBE", .T.)
    MsgBox( "Database Engine FOXDBE not loaded" , "Error" )
  ENDIF

  IF ! DbeLoad( "CDXDBE" , .T.)
    MsgBox( "Database Engine CDXDBE not loaded" , "Error" )
  ENDIF

  IF ! DbeBuild( "FOXCDX", "FOXDBE", "CDXDBE" )
    MsgBox( "Database Engine DBFCDX not created" , "Error" )
  ENDIF
RETURN

/// <summary>
/// <para>
/// Entry point
/// </para>
/// </summary>
PROCEDURE main()

  LOCAL oApp

  // Access the application window
  oApp := SetAppWindow()

  // handle all events
  oApp:showModal()

RETURN
