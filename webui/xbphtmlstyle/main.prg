//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 2015-2025. All rights reserved.         
//  
//  Contents:
//      This file contains the main entry point (main procedure) of the
//      respective example program. In this procedure, the application 
//      window is created, the table with the data for the combo box
//      is opened and the function for creating the combo box is called.
//   
//////////////////////////////////////////////////////////////////////

#include "Common.ch"
#include "xbp.ch"
#include "appevent.ch"

#pragma library( "xppwui.lib" )
#pragma library( "asiutl10.lib" )

/// <summary>Main entry point</summary>
PROCEDURE Main()
 LOCAL oDlg

   SET DEFAULT TO ..\..\data\misc

   /*
    * Create the application's main window
    */
   oDlg := XbpDialog():New( AppDesktop() )
   oDlg:TaskList := .T.
   oDlg:Title    := "XbpHTMLStyle Class Demo"
   oDlg:DrawingArea:ClipChildren := .T.
   oDlg:Create( ,,, {640,480},, .F. )
   CenterControl( oDlg )

   /*
    * Open the database with the information
    * to display and create the combo box 
    * object
    */
   OpenParts()
   CreateComboBox( oDlg )

   oDlg:ShowModal()
RETURN



/// <summary>Open the parts table</summary>
PROCEDURE OpenParts()
   USE parts SHARED NEW
RETURN


/// <summary>Set general display properties of the 
/// application</summary>
PROCEDURE AppSys()
   /*
    * The application uses the ANSI character set 
    * and does not use a console window
    */
   SET CHARSET TO ANSI
RETURN


/// <summary>Set general properties for database 
/// access</summary>
PROCEDURE DbeSys()
   /*
    * Build the FOXCDX compound DBE and make 
    * it the default DBE of the application
    */
   DbeLoad( "FOXDBE" )
   DbeLoad( "CDXDBE" )
   DbeBuild( "FOXCDX", "FOXDBE", "CDXDBE" )
   DbeSetDefault( "FOXCDX" )
RETURN

// EOF
