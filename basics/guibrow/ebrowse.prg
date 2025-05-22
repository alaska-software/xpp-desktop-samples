//////////////////////////////////////////////////////////////////////
//
//  EBROWSE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample for XbpBrowse
//   
//  Remarks:
//      Sample code for a value set in an array which is displayed
//      with the XbpBrowse class and changed with entryfields.
//      Keyboard interaction on XbpBrowse and XbpSle is also shown.
//   
//  Syntax:
//      EBROWSE
//   
//  Return:
//      returns 0 always
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"


PROCEDURE AppSys
 * no XbpCrt instance is created
RETURN

******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _EditForm class.
******************************************************************************
CLASS EditForm FROM _EditForm

   EXPORTED:

      VAR aData, nLastRec, nRecno
      VAR oValues

      METHOD init
      METHOD create

      METHOD Skip, ShowTotal
      METHOD EndEditIncome, BeginEditIncome

ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD EditForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT lVisible TO .T.

   * Execute method of the super class
   ::_EditForm:init( oParent, oOwner, aPos, aSize, aPP, .F. )
   ::_EditForm:Border := XBPDLG_DLGBORDER
   ::_EditForm:TaskList := .T.
   ::_EditForm:MaxButton := .F.
   ::_EditForm:Title  := "Sample Edit Browser"

   * initialize data
   ::aData    := { { "January",    1200 }, ;
                   { "February",   1000 }, ;
                   { "March",     -1000 }, ;
                   { "April",     13000 }, ;
                   { "Mai",         342 }, ;
                   { "June",       7120 }, ;
                   { "July",        523 }, ;
                   { "August",     1540 }, ;
                   { "September",  2001 }, ;
                   { "October",   13000 }, ;
                   { "November",    534 }, ;
                   { "December",    -48 } }
   ::nLastRec := LEN(::aData)
   ::nRecno   := 1

   * init browser, skip and value blocks for browser
   ::oValueGroup:Type := XBPSTATIC_TYPE_RECESSEDRECT
   ::oValues := XbpBrowse():new( ::oValueGroup )
   ::oValues:SkipBlock     := {|nSkip,oBrowse| ::Skip(nSkip,oBrowse) }
   ::oValues:GoBottomBlock := {|| ::nRecno := LEN(::aData) }
   ::oValues:GoTopBlock    := {|| ::nRecno := 1 }
   ::oValues:PosBlock      := {|| ::nRecno }
   ::oValues:PhyPosBlock   := {|| ::nRecno }
   ::oValues:GoPhyPosBlock := {|n| ::nRecno := n}
   ::oValues:LastPosBlock  := {|| LEN(::aData) }
   ::oValues:FirstPosBlock := {|| 1 }
   ::oValues:HScroll := .F.
   ::oValues:VScroll := .F.
   ::oValues:SizeCols := .F.
   ::oValues:CursorMode := XBPBRW_CURSOR_ROW

   * init captions for static and buttons
   ::oButtonOK:Caption     := "Ok"
   ::oButtonCancel:Caption := "Cancel"
   ::oGroup:Caption        := ""
   ::oGroup:Type           := XBPSTATIC_TYPE_RAISEDBOX

   * init other flags
   ::oTotal:Editable := .F.
   ::oIncome:DataLink := { | cValue | IIF ( cValue != NIL, ;
            ::aData[::nRecno][2] := VAL ( cValue ), ALLTRIM ( STR ( ::aData[::nRecno][2] ) ) ) }

   * Init callback slots
   ::oIncome:Keyboard     := { |mp1| ::EndEditIncome( mp1 ) }
   ::oValues:StableBlock  := { || ::ShowTotal() }
   ::oValues:ItemSelected := { || ::BeginEditIncome() }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD EditForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
  LOCAL nSizeX, nSizeY, nSize2, nSize1

   * Execute method of the super class
   ::_EditForm:create( oParent, oOwner, aPos, aSize, aPP, .F. )

   * Add columns to browser
   ::oValues:Create ( ,, { 1, 1 }, ;
            { ::oValueGroup:CurrentSize()[1] - 1, ::oValueGroup:CurrentSize()[2] - 1 } )
   ::oValues:AddColumn ( {|| ::aData[::nRecno,1] }, , "Month" )
   ::oValues:AddColumn ( {|| ::aData[::nRecno,2] }, ,  "Income" )

   * Adjust the size of the columns taking into account the size of the
   * parent Xbp
   nSizeX := ::oValues:currentSize()[1]
   nSizeY := ::oValues:currentSize()[2]
   nSize2 := int( nSizeX / 3 )
   nSize1 := nSizeX - nSize2

   oColumn := ::oValues:getColumn(1)
   oColumn:setsize( { nSize1, nSizeY } )

   oColumn := ::oValues:getColumn(2)
   oColumn:setsize( { nSize2, nSizeY } )

   * Values less then zero are red
   ::oValues:GetColumn(2):ColorBlock := ;
            { | nData | IIF( nData < 0, {GRA_CLR_RED,}, ) }
   ::oValues:Show()

   * income SLE is disabled per default
   ::oIncome:Disable()

   * Display the form centered
   CenterControl( self )
   ::show()

   * now set the focus on the browser so that the user may edit the data
   SetAppFocus ( ::oValues )

RETURN self


******************************************************************************
* Edit Income value
******************************************************************************
METHOD EditForm:BeginEditIncome()

   ::oIncome:SetData ()
   ::oIncome:Enable ()
   SetAppFocus ( ::oIncome )

RETURN self


******************************************************************************
* End Edit Income value
******************************************************************************
METHOD EditForm:EndEditIncome( nKey )

   IF nKey == xbeK_RETURN .OR. nKey == xbeK_ENTER
      * get the changed value and continue browsing
      ::oIncome:GetData ()
      ::oIncome:Disable ()
      SetAppFocus ( ::oValues )
      ::oValues:RefreshAll()
   ELSEIF nKey == xbeK_ESC
      * ignore value and continue browsing
      ::oIncome:Disable ()
      SetAppFocus ( ::oValues )
   ENDIF

RETURN self


******************************************************************************
* Display Total income
******************************************************************************
METHOD EditForm:ShowTotal()

   LOCAL nTotal := 0

   * calculate the total value
   AEval ( ::aData, { | aValue | nTotal += aValue[2] } )

   * oTotal and oIncome display always the current value
   ::oTotal:SetData ( STR ( nTotal ) )
   ::oIncome:SetData ()

RETURN self


******************************************************************************
* Skip Array
******************************************************************************
METHOD EditForm:Skip( nSkip )
   LOCAL nCanSkip

   IF ::nRecno + nSkip < 1               // "BoF"
      nCanSkip := 1 - ::nRecno
      TONE ( 1000 )
   ELSEIF ::nRecno + nSkip > ::nLastRec  // "EoF"
      nCanSkip := ::nLastRec - ::nRecno
      TONE ( 500 )
   ELSE
      nCanSkip := nSkip
   ENDIF

   ::nRecno += nCanSkip

RETURN nCanSkip


******************************************************************************
* Main procedure
******************************************************************************
PROCEDURE Main

   LOCAL nEvent, oXbp, mp1, mp2

   EditForm():New():Create()

   nEvent := xbe_None
   WHILE nEvent != xbeP_Close
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent ( nEvent, mp1, mp2 )
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO

RETURN

//EOF
/////
