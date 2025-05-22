//////////////////////////////////////////////////////////////////////
//
//  QBROWSE2.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      The sample demonstrates how to use the XbpQuickBrowse Object
//      for an incremental search within a database.
//////////////////////////////////////////////////////////////////////


#pragma Library( "XppUI2.LIB" )
#pragma Library( "ADAC20B.LIB" )

#include "Appevent.ch"
#include "Directry.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Font.ch"
#include "Xbp.ch"


PROCEDURE AppSys()
RETURN


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, i, oBrowse, oSLE, oDialog

   LOCAL aFields :=            { ;
         "CUSTOMER->CUSTNO"    , ;
         "CUSTOMER->MR_MRS"    , ;
         "CUSTOMER->FIRSTNAME" , ;
         "CUSTOMER->LASTNAME"  , ;
         "CUSTOMER->STREET"    , ;
         "CUSTOMER->ZIP"       , ;
         "CUSTOMER->CITY"        }

   LOCAL aHeader  := { ;
         "Cust No."  , ;
         "Mr/Mrs"    , ;
         "Firstname" , ;
         "Lastname"  , ;
         "Street"    , ;
         "Zip"       , ;
         "City"        }


   SET DEFAULT TO ..\..\data\misc\

   USE CUSTOMER

   /*
    * Create application window, browser and SLE
    */
   oDialog            := GuiStdDialog( "Address Data" )
   SetAppWindow( oDialog )


   /*
    * The SLE is required as entry field for the incremental
    * search. It is related to the column of the browser in
    * which the cursor is positioned.
    * Whenever a key is pressed, the procedure IncrementalSearch()
    * is called.
    */
   oSLE               := XbpSLE():new( oDialog:drawingArea,,,, , .F. )
   oSLE:keyboard      := {|nKey,mp2,obj| IncrementalSearch( nKey, obj, ;
                                                            oBrowse, aFields ) }
   oSLE:create()


   oBrowse            := XbpColoredQuickBrowse():new( oDialog:drawingArea,,,, , .F. )
   oBrowse:style      := XBP_STYLE_SYSTEMDEFAULT
   oBrowse:cursorMode := XBPBRW_CURSOR_CELL
   oBrowse:dataLink   := DacPagedDataStore():new( Alias(), aFields )
   oBrowse:create()

   Resize( oSLE, oBrowse, oDialog:drawingArea:currentSize() )

   /*
    * Copy strings to header of columns.
    */
   oBrowse:setHeader( aHeader )

   /*
    * Respecting the SLE, the browser fills the entire window. The cache
    * size of DacPagedDataStore must be set according to the rows of the
    * browser.
    */
   oDialog:drawingArea:resize := {|mp1,mp2,obj| Resize( oSLE, oBrowse, mp2 ) }

   oSLE:show()
   oBrowse:show()
   oDialog:show()
   SetAppFocus( oSLE )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * The procedure uses DbLocate()/DbContinue() and the
 * edit buffer of the SLE to perform an incremental search
 * in the database.
 * Pressing the RETURN key continues the search.
 */
PROCEDURE IncrementalSearch( nKey, oSle, oBrowse, aFields )
   LOCAL cValue  := Upper( AllTrim( oSle:editBuffer() ) )
   LOCAL cField  := aFields[oBrowse:colPos]
   LOCAL cSearch := '{||Upper(Left(%1,%2))=="%3"}'
   LOCAL nRecno, bGoto

   IF nKey == xbeK_RETURN
      DbContinue()
   ELSE
      // The first column contains the right adjustet customer number.
      IF oBrowse:colPos == 1
         cValue := Right(Space(6) + cValue, 6)
      ENDIF

      cSearch := StrTran( cSearch, "%1", cField )
      cSearch := StrTran( cSearch, "%2", LTrim( Str(Len(cValue)) ) )
      cSearch := StrTran( cSearch, "%3", cValue )
      DbLocate( &(cSearch) )
   ENDIF

   IF Found()
      // Position the browser cursor on the found
      // record.
      oBrowse:gotoRecord( Recno() )
   ELSE
      Tone(1000)
   ENDIF
RETURN



/*
 * Resize of the SLE and the Browser to fit the window size.
 */
PROCEDURE Resize( oSLE, oBrowse, aSize )
   oSLE:setPos(  { 0, aSize[2]-24 } )
   oSLE:setSize( { aSize[1], 24} )

   oBrowse:setSize( {aSize[1], aSize[2]-24} )
   oBrowse:dataLink:setAbsolutePageSize( oBrowse:rowCount )
RETURN



/*
 * Create invisible standard dialog
 */
FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg
   LOCAL aSize := {600,400}
   LOCAL aPos  := CenterPos( aSize, AppDesktop():currentSize() )

   DEFAULT cTitle TO "Standard Dialog Window"

   oDlg          := XbpDialog():new( ,, aPos, aSize,, .F. )
   oDlg:icon     := 1
   oDlg:taskList := .T.
   oDlg:title    := cTitle
   oDlg:drawingArea:ClipChildren := .T.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_DEFPROP_SMALL )

RETURN oDlg


FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }


/*
 * This derived class paints the even records in a different
 * color than the odd rows.
 */
CLASS XbpColoredQuickBrowse FROM XbpQuickBrowse
EXPORTED:
   VAR EvenRow
   INLINE METHOD Init( p1, p2, p3, p4, p5, p6 )
      ::XbpQuickbrowse:Init( p1, p2, p3, p4, p5, p6 )
      ::EvenRow := .T.
   RETURN self

   INLINE METHOD DrawRow( nRow, nCol, aValues, nCols, lRedraw )
      LOCAL aValue, j
      LOCAL lRGBLightBlue := {201,222,245}, lHilite := .F.

      /*
       * Draw the row
       */
      ::XbpQuickBrowse:DrawRow( nRow, nCol, aValues, nCols, lRedraw )
      aValue := ::dataLink:GetRowData( nRow )

      /*
       * Give the row a color in case it has a valid record id,
       * and depending whether we colour odd or even rows.
       */
      IF ::EvenRow
         IF nRow % 2 == 0
            lHilite := .T.
         ENDIF
      ELSE
         IF nRow % 2 == 1
            lHilite := .T.
         ENDIF
      ENDIF
      IF lHilite .AND. ValType(aValues[1]) != "U"
         FOR j := 1 TO ::ColCount
            ::dataArea:SetCellColor( nRow, j, NIL, GraMakeRGBColor( lRGBLightBlue ), lRedraw )
         NEXT
      ENDIF
   RETURN .T.

   INLINE METHOD ScrollDown( nScroll )
      IF nScroll % 2 == 1
         ::EvenRow := !::EvenRow
      ENDIF
   RETURN ::XbpQuickbrowse:ScrollDown( nScroll )

   INLINE METHOD ScrollUp( nScroll )
      IF nScroll % 2 == 1
         ::EvenRow := !::EvenRow
      ENDIF
   RETURN ::XbpQuickbrowse:ScrollUp( nScroll )

   INLINE METHOD RefreshAll( p1, p2 )
      ::EvenRow := .T.
   RETURN ::XbpQuickbrowse:RefreshAll( p1, p2 )

   INLINE METHOD GoTop()
      ::EvenRow := .T.
      ::XbpQuickbrowse:GoTop()
   RETURN ::ForceStable()

   INLINE METHOD GoBottom()
      ::EvenRow := .T.
      ::XbpQuickbrowse:GoBottom()
   RETURN ::ForceStable()

ENDCLASS
