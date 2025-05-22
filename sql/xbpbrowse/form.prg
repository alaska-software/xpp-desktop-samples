//////////////////////////////////////////////////////////////////////
//
//  FORM.PRG
//
//  Copyright:
//   Alaska Software, (c) 2013-2025. All rights reserved.         
//  
//  Contents:
//   
//////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#pragma library("xppui2.lib")
#pragma library("xppwui.lib")

/// <summary>
/// The SqlForm consists of a combo box used to select SQL queries and a
/// browse to show the SQL result set. The class also implements a row edit
/// feature for the browse cursor. Simply hit Enter or double-click on a row
/// and an edit window for the current row is opened.
/// </summary>
CLASS SqlForm FROM XbpDialog
  PROTECTED:
  VAR _BrwView
  VAR _CboQuery
  VAR _Queries

  METHOD getQueryById()
  EXPORTED:
  METHOD init()
  METHOD create()
  METHOD showModal()
  METHOD createView()
  METHOD editRow()
  METHOD querySelected()
  METHOD queryData()
ENDCLASS


/// <summary>
/// Return query meta data by selection id of combo box
/// </summary>
/// <returns>{ cDescription, cSQLSelect, lEditable, cTablename }</returns>
METHOD SqlForm:getQueryById( nId )
  // Off-by-one: first entry in query array holds hint "Please select..." and no SQL query.
  nId--
  IF(nId<1 .OR. nId>Len(::_Queries))
    RETURN(NIL)
  ENDIF
RETURN(::_Queries[nId])


/// <remarks>
/// Instead of just setting up the form controls we perform a :create() and
/// make the form the application's main form. So a simple :new() will create
/// the entire form and give it focus.
/// </remarks>
/// <returns>oForm</returns>
METHOD SqlForm:init( oParent, aQueries )

   ::_Queries := AClone(aQueries)

   SUPER:init( oParent, , , {642,400}, {{ XBP_PP_COMPOUNDNAME, "9.Tahoma" }} , .F. )
   ::taskList := .T.
   ::title    := "SqlBrowse"

   ::_CboQuery := XbpCombobox():new( ::drawingArea, , {24,155}, {576,192}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::_CboQuery:tabstop := .T.
   ::_CboQuery:type := XBPCOMBO_DROPDOWNLIST
   ::_CboQuery:setEditable(.f.)

   ::Create()
   XbpApplication():SetMainform(SELF)
RETURN self


METHOD SqlForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
  LOCAL n

  UNUSED(oParent)
  UNUSED(oOwner)
  UNUSED(aPos)
  UNUSED(aSize)
  UNUSED(aPP)
  UNUSED(lVisible)
  SUPER

  // Create and fill combo box with query definitions
  ::_CboQuery:create()
  ::_CboQuery:addItem("Please wait, connecting to server...")
  FOR n:=1 TO Len(::_Queries)
    IF( ::_Queries[n][3] )
      ::_CboQuery:addItem( ::_Queries[n][1] + " (double-click/press Enter to edit)" )
    ELSE
      ::_CboQuery:addItem( ::_Queries[n][1] )
    ENDIF
  NEXT n

  // Bind itemSelected event and ensure first item is selected
  ::_CboQuery:setData( {1} )
  ::_CboQuery:itemSelected := {|mp1,mp2,o|::querySelected()}

  CenterControl(SELF)
  ::show()

RETURN self


/// <summary>
/// Display form modal and return saved/cancelled state
/// </summary>
/// <returns>nResult (XBP_MRESULT_OK or XBP_MRESULT_CANCEL)</returns>
METHOD SqlForm:showModal()
  // Set and select first entry of combo box to display help to the user
  ::_CboQuery:setItem(1 , "Please select query...")
  ::_CboQuery:setData( {1} )

// Set focus to combo box to ensure user can use keyboard to navigate
RETURN( SUPER:showModal( ::_CboQuery ) )


/// <summary>
/// Handle combo box selection and perform query as well as view creation
/// </summary>
METHOD SqlForm:querySelected()
  LOCAL aSelection

  IF(ValType(::_BrwView)=="O")
    ::_BrwView:Hide()
    ::_BrwView:Destroy()
    ::_BrwView := NIL
  ENDIF

  aSelection := ::_CboQuery:XbpListBox:GetData()
  IF(!Empty(aSelection) .AND. aSelection[1]!=1 )
    ::queryData( aSelection[1] )
    ::createView( aSelection[1] )
  ENDIF
RETURN(SELF)


/// <summary>
/// Queries the data according to the selection using pass-through SQL.
/// </summary>
METHOD SqlForm:queryData( nId )
  LOCAL aQuery
  LOCAL cStmt, oStmt

  aQuery := ::getQueryById( nId )
  IF(!Empty(aQuery))
    cStmt := aQuery[2]
    oStmt := DacSqlStatement():FromChar(cStmt)
    oStmt:Build():Query()
  ENDIF

RETURN(SELF)


METHOD SqlForm:createView( nId )
  LOCAL i,cField
  LOCAL oParent := SELF:drawingArea
  LOCAL aPos := {24,10}
  LOCAL aSize := oParent:currentSize()
  LOCAL lEditable

  aSize[1] := 576
  aSize[2] -= 65

  lEditable := ::getQueryById( nId )[3]

 ::_BrwView := XbpBrowse():new( oParent,, aPos, aSize, ,.f. )
 ::_BrwView:cursorMode := XBPBRW_CURSOR_ROW
 ::_BrwView:softTrack := .F.
 ::_BrwView:Create()

 // Bind itemSelected event (double-click or Enter on row) to enable row editing
 IF(lEditable)
   ::_BrwView:itemSelected := {| uNIL1, uNIL2, oBrw | SELF:editRow( nId, Alias() ) }
 ENDIF

 // Navigation code blocks for the browser
 ::_BrwView:skipBlock     := {|n| DbSkipper(n) }
 ::_BrwView:goTopBlock    := {| | DbGoTop()    }
 ::_BrwView:goBottomBlock := {| | DbGoBottom() }
 ::_BrwView:phyPosBlock   := {| | Recno()      }

 // Navigation code blocks for the vertical scroll bar
 ::_BrwView:posBlock      := {| | DbPosition()    }
 ::_BrwView:goPosBlock    := {|n| DbGoPosition(n) }
 ::_BrwView:lastPosBlock  := {| | 100             }
 ::_BrwView:firstPosBlock := {| | 0               }

 // Add columns for the first six fields of the table
 FOR i:=1 TO Min(6,FCount())
    cField := FieldName( i )
    ::_BrwView:addColumn( FieldBlock(cField), , cField )
 NEXT

 ::_BrwView:show()
 SetAppFocus(::_BrwView)
RETURN(SELF)


/// <summary>
/// Edit the current record under the row cursor. Optionally store the data changes
/// in the table using ISAM code and update the browse to reflect data changes.
/// </summary>
///
/// <remarks>
/// STRATEGY:<para/>
/// Retrieve ISAM record# for current row selected in the browse<para/>
/// Open ISAM table, handle editing of that record<para/>
/// Close ISAM table and in case of updates re-query the result set<para/>
/// </remarks>
METHOD SqlForm:editRow( nId, cAlias )
  LOCAL nRecordId, nCurrRecord
  LOCAL cTable, oSession
  LOCAL oData, oEditView
  LOCAL nResult
  FIELD __record

  // - Determine record# of selected item of browse in result set
  // - Retrieve ISAM record# (fieldname: __record) from result set
  // - Use that record# as our primary key for the ISAM update
  //
  nRecordId   := ::_BrwView:rowPhyPos
  nCurrRecord := (cAlias)->(RecNo())
  (cAlias)->(DbGoto( nRecordId ))
  nRecordId   := (cAlias)->(__record)
  (cAlias)->(DbGoto( nCurrRecord ))

  // Get ISAM table name from query definition, retrieve current session
  //
  cTable   := ::getQueryById( nId )[4]
  oSession := DbSession()
  USE (cTable) NEW VIA (oSession)
  DbGoto( nRecordId )
  DbRLock()

  // Create edit view
  oEditView := EditView():new( SELF )

  // Used to disable the XBT0120 warning: not initialized var
#pragma info(noinit)
  // Scatter data from table to a DataObject (our model)
  SCATTER NAME oData
  oEditView:setModel( oData )

  // Process editing and store data from model back to record
  nResult := oEditView:showModal()
  IF(XBP_MRESULT_OK == nResult)
    GATHER NAME oEditView:setModel()
  ENDIF
  DbRUnlock()
  USE
  DbSelectArea( cAlias )

  // Destroy edit view and enforce a refresh of the browsers selected row, if required 
  oEditView:destroy()
  IF(XBP_MRESULT_OK == nResult)

    // Since we changed the data in the ISAM table, we need to re-query
    // the SQL result set. This is done automatically by DbRefresh(). Alternatively,
    // a SKIP 0 would do the same. However, DbRefresh() is the preferred way as it also
    // posts a DBO_ROWSET_CHANGED event.
    DbRefresh()

    ::_BrwView:refreshCurrent()
    ::_BrwView:forceStable()
  ENDIF
RETURN(SELF)



/// <summary>
/// The EditView is in fact a simple HTML form which uses the layout definition as
/// defined in the assets\form-template.htm file introduced by the XbpSmartForm base 
/// class. Consequently, there is no need to position any element on the form. Everything 
/// is placed automatically. If the form is too large, scrollbars will appear and the 
/// form can be scrolled.
/// </summary>
CLASS EditView FROM XbpSmartForm
  EXPORTED:
  METHOD init()
ENDCLASS

METHOD EditView:init( oParent )

  SUPER:init( oParent:SetParent() , {600,470}  )
  ::create()

  ::startDefinition( "CustomerId: "+field->customerid )

  /* Note:
   * A heading is like a group, the following fields are floated under the heading
   * A field or entry field has a label, an id which is used to bind the model, a hint
   * message and the length of the entry field in characters.
   * :addFields() is used to have multiple entry fields in the same row with one label.
   */
  ::addHeading( "Address Information" )

  ::addField( "Company", "companyname", "Enter name of legal entity", 40 )
  ::addField( "Address", "address", "Enter full postal address", 60 )

  ::addFields( "Zip/City/Region", {"postalcode","city","region"}, {"Zip code","City name","Enter region"},{10,15,15} )
  ::addField( "Country", "country", "Countryname", 15 )

  ::addHeading( "Contact Details" )
  ::addField( "Contact", "contactname", "Enter full contactname", 30 )
  ::addField( "Title", "contacttitle", "Enter titel/role", 30 )

  ::addField( "Phone", "phone", "Enter business phone#", 24 )
  ::addField( "Fax", "fax", "Enter order fax#", 24 )
  ::endDefinition()

RETURN SELF

//EOF
/////
