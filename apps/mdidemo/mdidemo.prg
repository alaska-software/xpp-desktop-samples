//////////////////////////////////////////////////////////////////////
//
//  MDIDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Main routine and AppSys() for an MDI application
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "AppEvent.ch"
#include "Mdidemo.ch"


/*
 * Add the define USE_POSTGRES to your project-settings -> compiler-tab -> defines
 * to use the PostgreSQL server instead of the dbf tables and index files.
 */
#ifdef USE_POSTGRES
#include "pgdbe.ch"
#endif


/*
 * Main procedure and event loop
 */
PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   FIELD CUSTNO, LASTNAME, FIRSTNAME, PARTNO, PARTNAME

   /* Load the PostgreSQL DatabaseEngine and connect to the server
    * NOTE:
    * Please change server, uid and pwd according to your server setup
    */
#ifdef USE_POSTGRES
   LOCAL oSession
   LOCAL cConnStr

   DbeLoad("pgdbe")
   DbeSetDefault("pgdbe")

   cConnStr := "DBE=pgdbe;server=localhost;"
   cConnStr += "db=mdidemo;uid=postgres;pwd=postgres"

   oSession := DacSession():New(cConnStr)
   IF(!oSession:IsConnected())
     MsgBox("Connection failed ("+Var2Char(oSession:GetLastMessage())+")")
     QUIT
   ENDIF
#endif

   /* We can not use File() to verify if a table exists. Use Table()
    * instead. Table() works with dbf tables, sql tables, local or
    * remote.
    */
#ifdef USE_POSTGRES
   MsgBox("Connected to PostgreSQL server")
   IF(!Table("Customer"))
     MsgBox("No customer table")
   ENDIF
   IF(!Table("Parts"))
     MsgBox("No parts table")
   ENDIF
#else
   // Check index files and create them if not existing
   SET DEFAULT TO ..\..\data\misc

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
#endif

   SET DELETED ON

   // Infinite loop. The programm is terminated in AppQuit()
   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN



/*
 * Check if all files of the array 'aFiles' exist
 */
FUNCTION AllFilesExist( aFiles )
   LOCAL lExist := .T., i:=0, imax := Len(aFiles), cPath := Set( _SET_DEFAULT )

   IF(!Empty(cPath) .AND. cPath[-1]!='\')
     cPath += "\"
   ENDIF

   DO WHILE ++i <= imax .AND. lExist
      lExist := File( cPath + aFiles[i] )
   ENDDO
RETURN lExist



/*
 * Routine to terminate the programm
 */
PROCEDURE AppQuit()
   LOCAL nButton

   nButton := ConfirmBox( , ;
                 "Do you really want to quit ?", ;
                 "Quit", ;
                  XBPMB_YESNO , ;
                  XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE )

   IF nButton == XBPMB_RET_YES
      COMMIT
      CLOSE ALL
#ifdef USE_POSTGRES
      // We always should disconnect.
      AEval( DacSession():sessionList(), {|o|o:disconnect()})
#endif

      QUIT
   ENDIF

RETURN



/*
 * Example for AppSys() in an MDI application
 * This function is executed prior to Main()
 */
PROCEDURE AppSys
   LOCAL oDlg, oXbp, aPos[2], aSize

   // Get size of desktop window and
   // dimension the application window accordingly
   aSize    := SetAppWindow():currentSize()
   aPos[1]  := 0.1 * aSize[1]
   aPos[2]  := 0.1 * aSize[2]
   aSize[1] *= 0.8
   aSize[2] *= 0.8

   // Create application window
   oDlg       := XbpDialog():new( ,, aPos, aSize,, .F. )
   oDlg:title := "Toys & Fun Inc. [Xbase++ - MDI Demo]"
   oDlg:icon  := ICON_APPLICATION
   oDlg:close := {|| AppQuit() }
   oDlg:taskList := .T.
   oDlg:drawingArea:ClipChildren := .T.
   oDlg:create()
   oDlg:drawingArea:SetColorBG( XBPSYSCLR_APPWORKSPACE )
   SetAppWindow( oDlg )

   // Create menu system (UDF). The main menu is globally
   // accessible via GetApplication():MainForm:MenuBar()
   MenuCreate( oDlg:menuBar() )

   // Provide online help via UDF
   oXbp := XbpHelpLabel():new():create()
   oXbp:helpObject := ;
        HelpObject( "MDIDEMOE.HLP", "Help for MDI demo" )
   oDlg:helpLink := oXbp

   // Display application window and set focus to it
   oDlg:show()
   SetAppFocus( oDlg )

   // Application window is globally accessible via
   // GetApplication():MainForm
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
