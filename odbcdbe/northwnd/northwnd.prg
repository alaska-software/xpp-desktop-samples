//////////////////////////////////////////////////////////////////////
//
//  NORTHWND.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      Show the Nortwind-Sample Database of the MS-SQL Server via ODBC
//   
//  Remarks:
//      Please install the ODBC data source before and change the source 
//      code accordingly.
//      
//////////////////////////////////////////////////////////////////////


#include "dmlb.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "Xbp.ch"
#include "gra.ch"
#include "Dbstruct.ch"
#pragma library ("Xppui2.lib")
#pragma library( "ADAC20B.LIB" )
/*
 * Including odbcdbe.ch includes requests to ODBCUTIL.LIB.
 * Please see odbcdbe.ch for details.
 */
#include "odbcdbe.ch"
#define CRLF chr(13)+chr(10)

PROCEDURE MAIN()
  LOCAL nEvent:=0, mp1:=0, mp2:=0, oParent
  LOCAL oXbp, oListBox, oTxt, oDlg
  LOCAL aTables, n
  LOCAL oSession
  LOCAL cDbsName
  LOCAL bErr
  LOCAL lExit

  oDlg := GuiStdDialog("MS-SQL Server Northwind browser", ;
                       {300,300}, {10,50})
  oDlg:close := {|u1,u2,self| lExit := .T.}

  oParent := oDlg:drawingArea

  DbeInfo(COMPONENT_DICTIONARY, ODBCDBE_PROMPT_MODE, ODBC_PROMPT_COMPLETE)
  DbeInfo(COMPONENT_DICTIONARY, ODBCDBE_WIN_HANDLE, oDlg:getHwnd())

  oTxt := XbpStatic():new(oParent,,{10, 215}, {260,45},;
                         { { XBP_PP_FGCLR       , GRA_CLR_RED         }, ; 
                           { XBP_PP_COMPOUNDNAME, FONT_TIMES_SMALL }  } )
  oTxt:type := XBPSTATIC_TYPE_TEXT
  oTxt:options := XBPSTATIC_TEXT_WORDBREAK
  oTxt:caption := "You need the MS-SQL Server and a valid ODBC-Driver"+CRLF+;
                  "installed if you want to browse the "+CRLF+;
                  "Northwind sample database."
  oTxt:create()
  
  oSession := dacSession():New("DBE=ODBCDBE") 
  IF (!oSession:isConnected())
    MsgBox( "Cannot connect to server !")
    QUIT
  ENDIF

  /*
   * Print out the name of the database we are connected to now
   */
  SetAppFocus(oDlg)
  cDbsName := oSession:setProperty(ODBCSSN_CURRENT_DATABASE)
  oTxt:setColorFG( GRA_CLR_DARKGREEN ) 
  oTxt:setCaption("Connected to database: " + CRLF + cDbsName)
  /*
   * Get all table names belonging to this database
   */
  IF !empty(cDbsName)
     cDbsName += "."
  ENDIF

  aTables := {}
  bErr := ErrorBlock({|e| Break(e)})  
  BEGIN SEQUENCE
      USE ("@"+cDbsName+"TABLES")
      DbEval({|| AAdd( aTables, Field->Table_name)})
  RECOVER
      ErrorBlock(bErr)  
      USE ("SELECT * from sysobjects WHERE type = 'U';")
      DbEval({|| AAdd( aTables, Field->name)})
  END SEQUENCE
  ErrorBlock(bErr)  
  USE

 
  /*
   * Show timestamps as dates
   */ 
  oSession:setProperty(ODBCSSN_TIMESTAMP_AS_DATE, .T.)

  /*
   * Add some queries to the table list
   */ 
  IF "Orders" $ aTables .and. "Order Details" $ aTables
     AAdd(aTables, "SELECT Orders.OrderId, CustomerId, Discount "+;
                   "FROM Orders, [Order Details] WHERE "+;   
                   "Orders.OrderId = [Order Details].OrderId AND "+;
                   "Discount > 0;")
  ENDIF
  /*
   * Fill listbox with table names
   */
  oListBox := XbpListbox():new(oParent)  
  oListBox:horizScroll := .T.
  oListBox:create( ,, {10, 70}, {260, 150} ) 
  AEval( aTables, {|e,i| oListBox:addItem(e)})
  oListBox:ItemSelected := {|mp1, mp2, obj| DbBrowse(oDlg, ;
                                              aTables[oListBox:getData()[1]])}
  oXbp := XbpPushButton():new(oParent,,{100,30}, {80,25})
  oXbp:caption := "Close"
  oXbp:create()
  oXbp:activate := {|| lExit := .T.}
  lExit := .F.
  SetAppFocus(oListBox)

  /*
   * Use cargo slot for focus management
   */
  oDlg:cargo := oListBox
  DO WHILE !lExit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

  DbCloseAll()
  oSession:disconnect()
RETURN


PROCEDURE DbBrowse(oParent, cTable)
   LOCAL oBrowse, oDlg, aFields

   USE (cTable) NEW
   aFields := {}
  /*
   * Add fields
   */
   AEval(DbStruct(), {|e,i| AAdd( aFields, e[DBS_NAME]) })

   oDlg := GuiStdDialog(Alias(), {600,400})
   oBrowse            := XbpQuickBrowse():new(oDlg:drawingArea,,,{550,370})
   oBrowse:style      := XBP_STYLE_SYSTEMDEFAULT
   oBrowse:cursorMode := XBPBRW_CURSOR_CELL
   oBrowse:dataLink   := DacPagedDataStore():new( Alias(), aFields )
   oBrowse:create()
   oDlg:close := {|u1,u2,self| self:hide(), self:destroy(),;
                               SetAppFocus(oParent:cargo)}
   oDlg:drawingArea:resize := ;
       {|mp1,mp2,obj| oBrowse:setSize(mp2), ;
                      oBrowse:dataLink:setAbsolutePageSize( oBrowse:rowCount ) }

   oBrowse:show()

   SetAppFocus( oBrowse )
RETURN

/*
 * Create invisible standard dialog
 */
FUNCTION GuiStdDialog( cTitle , aSize, aPos)
   LOCAL oDlg, aDeskSize
   STATIC aLastPos 

   aDeskSize := AppDesktop():currentSize() 
   IF aPos = NIL 
      IF aLastPos = NIL .OR. aLastPos[1]+aSize[1] > aDeskSize[1]
          aLastPos := CenterPos( aSize, aDeskSize)
      ENDIF
      aPos := Array(2)
      aPos[1] := aLastPos[1] + 20
      aPos[2] := aLastPos[2] + 10
      aLastPos := aPos
   ENDIF

   oDlg          := XbpDialog():new(AppDesktop() ,, aPos, aSize,, .F. )
   oDlg:icon     := 1
   oDlg:taskList := .T.
   oDlg:title    := cTitle
   oDlg:drawingArea:ClipChildren := .T.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_DEFPROP_SMALL )
   oDlg:show() 
RETURN oDlg
 

FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }


PROCEDURE AppSys
RETURN

/*
 * Load ODBCDBE as default dbe
 */
PROCEDURE DbeSys
   DbeLoad( "ODBCDBE" )
   DbeSetDefault( "ODBCDBE" )
RETURN
