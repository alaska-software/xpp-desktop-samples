//////////////////////////////////////////////////////////////////////
//
//  SDIDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Main routine and AppSys() for a SDI application
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "AppEvent.ch"
#include "Sdidemo.ch"

#pragma Library( "Xppui2.lib" )

/*
 * Main procedure and event loop
 */
PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   FIELD CUSTNO, LASTNAME, FIRSTNAME, PARTNO, PARTNAME

   SET DEFAULT TO "..\..\data\misc\"

   IF ! AllFilesExist( { "CUSTA.NTX", "CUSTB.NTX" } )
      USE Customer EXCLUSIVE
      INDEX ON CustNo                    TO CustA
      INDEX ON Upper(LastName+Firstname) TO CustB
      CLOSE DATABASE
   ENDIF

   IF ! AllFilesExist( { "PARTS.CDX" } )
      USE Parts EXCLUSIVE VIA FOXCDX
      INDEX ON Upper(PartNo)   TAG PartNo   TO Parts
      INDEX ON Upper(PartName) TAG PartName TO Parts
      CLOSE DATABASE
   ENDIF

   SET DELETED ON

   USE Customer  NEW
   SET INDEX TO CustA, CustB
   SET ORDER TO 2
   GO TOP

   USE Parts NEW VIA FOXCDX
   SET INDEX TO Parts
   SET ORDER TO 2
   GO TOP

   SET DELETED ON

   SetAppWindow():close := ;
      {|mp1,mp2,obj| DlgWriteData( obj ), AppQuit() }

   SetAppWindow():cargo := { -1, -1 }    //  { This record, previous record }
   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Example for AppSys() in an SDI application
 * The function is executed prior to Main()
 */
PROCEDURE AppSys
   LOCAL oDlg, oXbp, aPos[2], aSize, nHeight:=400, nWidth := 615

   // Get size of desktop window
   // to center the application window
   aSize    := SetAppWindow():currentSize()
   aPos[1]  := Int( (aSize[1]-nWidth ) / 2 )
   aPos[2]  := Int( (aSize[2]-nHeight) / 2 )

   // Create application window
   oDlg := XbpDialog():new()
   oDlg:title    := "Toys & Fun Inc. [Xbase++ - SDI Demo]"
   oDlg:border   := XBPDLG_DLGBORDER
   oDlg:taskList := .T.
   oDlg:maxButton:= .F.
   oDlg:create( ,, aPos, {nWidth, nHeight},, .F. )

   // Select font
   oDlg:drawingArea:SetFontCompoundName( "8.Helv" )

   // Create menu system (UDF)
   MenuCreate( oDlg:menuBar() )

  // Provide online help via UDF
   oXbp := XbpHelpLabel():new():create()
   oXbp:helpObject := ;
        HelpObject( "SDIDEMOE.HLP", "Help for SDI demo" )
   oDlg:helpLink := oXbp

   // Display application window and set focus to it
   oDlg:show()
   SetAppWindow( oDlg )
   SetAppFocus ( oDlg )

RETURN



/*
 * Routine to terminate the programm
 */
PROCEDURE AppQuit()
   LOCAL nButton, oXbp := SetAppFocus()

   nButton := ConfirmBox( , ;
                 "Do you really want to quit ?", ;
                 "Quit", ;
                  XBPMB_YESNO , ;
                  XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE )

   IF nButton == XBPMB_RET_YES
      CLOSE ALL
      QUIT
   ENDIF

   SetAppFocus( oXbp )
RETURN



/*
 * Routine to retrieve the help object. It manages the online help
 */
FUNCTION HelpObject( cHelpFile, cTitle )
   STATIC soHelp

   IF soHelp == NIL
      soHelp := XbpHelp():new()
      soHelp:resGeneralHelp := ID_HELP_GENERAL
      soHelp:resKeysHelp    := ID_HELP_KEYS
      soHelp:create( SetAppWindow(), cHelpFile, cTitle )
   ENDIF
RETURN soHelp



/*
 * Check if all files of the array 'aFiles' exist
 */
FUNCTION AllFilesExist( aFiles )
   LOCAL lExist := .T., i:=0, imax := Len(aFiles)

   DO WHILE ++i <= imax .AND. lExist
      lExist := File( aFiles[i] )
   ENDDO
RETURN lExist



/*
 * Check if an index value does not exist in an index file
 */
FUNCTION IsIndexUnique( nOrder, xIndexValue )
   LOCAL nOldOrder := OrdNumber()
   LOCAL nRecno    := Recno()
   LOCAL lUnique   := .F.

   OrdSetFocus( nOrder )

   lUnique := ! DbSeek( xIndexValue )

   OrdSetFocus( nOldOrder )
   DbGoTo( nRecno )

RETURN lUnique
