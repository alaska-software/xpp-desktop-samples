//////////////////////////////////////////////////////////////////////
//
//  CHART.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   This sample demonstrates the usage of a chart object via the
//   Xbase++ ActiveX layer.
//   
//  Remarks:
//   This sample needs the Microsoft Office Web Components (OWC).
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////



#pragma Library( "ascom10.lib" )

#include "owcchart.ch"
#include "activex.ch"
#include "appevent.ch"


/// <summary>
/// <para>
/// Open Database and generate Dialog
/// </para>
/// </summary>
PROCEDURE main()

  LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL
  LOCAL cOld, oDlg

  // Open the database
  cOld := Set( _SET_DEFAULT, "..\..\data\misc\" )

  use cars

  Set( _SET_DEFAULT, cOld )

  // Create the dialog and enter the the eventloop.
  oDlg := OpenDlg()

  nEvent := 0
  DO WHILE nEvent <> xbeP_Close
     nEvent := AppEvent( @mp1, @mp2, @oXbp )
     oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

  oDlg:destroy()

RETURN

/// <summary>
/// <para>
/// Create Dialog and Show first chart
/// </para>
/// </summary>
/// <returns>
/// oDialog
/// </returns>
FUNCTION OpenDlg()

  LOCAL oDlg

  // Create the dialog that is derived from XbpDialog and
  // set it as Application Window.
  oDlg := ChartViewer():new( AppDesktop(), , {100, 100} )
  oDlg:tasklist := .T.
  oDlg:title    := "Xbase++ Chart Sample"
  oDlg:create()
  SetAppWindow( oDlg )

  // Show the chart. Hereby CARS.DBF is used and a temporary
  // Gif picture is generated. This picture is loaded and
  // displayed.
  oDlg:showChart()

RETURN oDlg

/// <summary>
/// <para>
/// Overloaded AppSys
/// </para>
/// </summary>
PROCEDURE AppSys()
RETURN


// EOF
//////
