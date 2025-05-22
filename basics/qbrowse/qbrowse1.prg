//////////////////////////////////////////////////////////////////////
//
//  QBROWSE1.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      The example shows how to use an XbpQuickBrowse object for
//      browsing a two dimensional array.
//   
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
   LOCAL nEvent, mp1, mp2, oXbp, i, aArray, oBrowse, oDialog
   LOCAL aPP      := {{ XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL}}

   LOCAL aColumns :=     { ;
         F_NAME          , ; // Display files as icons
         F_NAME          , ; // Display file names as text
         F_SIZE          , ;
         F_WRITE_DATE    , ;
         F_WRITE_TIME    , ;
         F_CREATION_DATE , ;
         F_CREATION_TIME   }
   LOCAL aHeader  :=     { ;
         " "             , ;
         "File name    " , ;
         "    File size" , ;
         "  Access date" , ;
         "  Access time" , ;
         "Creation date" , ;
         "Creation time"   }


   aArray := Directory( "*.*", "D" )

   /*
    * Create an application window and the browser
    */
   oDialog          := GuiStdDialog( CurDrive() + "\" + CurDir() + "\" )
   SetAppWindow( oDialog )

   oBrowse          := XbpQuickBrowse():new( oDialog:drawingArea,,, oDialog:drawingArea:currentSize(), aPP, .F. )
   oBrowse:style    := XBP_STYLE_SYSTEMDEFAULT
   oBrowse:dataLink := DacPagedDataStore():new( aArray  , ;
                                                aColumns  )
   /*
    * The width of the browser columns is calculated from the
    * reference array.
    */
   aHeader[2]    := PadR( aHeader[2], 40 )
   oBrowse:heading:referenceArray  := ;
   oBrowse:dataArea:referenceArray := aHeader
   oBrowse:create()

   /*
    * Transfer the column headers and set height of browser rows
    * and width of the first column according to the icon size.
    */
   oBrowse:setHeader   ( aHeader )
   oBrowse:SetColType  ( 1, "C", XBPCOL_TYPE_FILEMINIICON )
   oBrowse:SetColWidth ( 32, 1 )

   /*
    * Set the alignment of data.
    */
   oBrowse:dataArea:setAlignment( 1, XBPALIGN_VCENTER + XBPALIGN_HCENTER )

   FOR i:=3 TO 7
      oBrowse:heading :setAlignment( i, XBPALIGN_RIGHT + XBPALIGN_VCENTER )
      oBrowse:dataArea:setAlignment( i, XBPALIGN_RIGHT + XBPALIGN_VCENTER )
   NEXT

   /*
    * Overload resize so that the browser fills the window
    * and adjust the cache size of DacPagedDataStore
    * according to the visible browser rows
    */
   oDialog:drawingArea:resize := ;
       {|mp1,mp2,obj| oBrowse:setSize(mp2), ;
                      oBrowse:dataLink:setAbsolutePageSize( oBrowse:rowCount ) }

   oBrowse:show()
   oBrowse:refreshAll()
   oBrowse:itemSelected := {|mp1,mp2,obj| ChangeDir( obj, @aArray, aColumns ) }
   oDialog:show()
   SetAppFocus( oBrowse )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Change directory and display new data
 */
PROCEDURE ChangeDir( oBrowse, aDirectory, aColumns )
   LOCAL i        := oBrowse:getData()
   LOCAL cCurDir  := StrTran( CurDrive()+":\"+CurDir()+"\", "\\", "\" )
   LOCAL lRefresh := .F.

   IF aDirectory[i,F_NAME] == "."
      /*
       * Current directory - do nothing
       */

   ELSEIF aDirectory[i,F_NAME] == ".."
      IF Len( cCurDir ) > 3
         /*
          * A parent directory exists
          */
         cCurDir  := SubStr( cCurDir, 1, RAt( "\", cCurDir, Len(cCurDir)-1 ) )
         lRefresh := .T.
      ENDIF

   ELSEIF "D" $ aDirectory[i,F_ATTR]
      /*
       * This is a subdirectory
       */
      cCurDir  += aDirectory[i,F_NAME] + "\"
      lRefresh := .T.

   ENDIF

   IF lRefresh
      SetAppWindow():setTitle( cCurDir )

      oBrowse:setPointer( NIL, XBPSTATIC_SYSICON_WAIT, XBPWINDOW_POINTERTYPE_SYSPOINTER )

      /*
       * Create a new DacPagedDataStore and
       * adjust its cache size according to the visible rows in the browser
       */
      CurDir( cCurDir )
      aDirectory := Directory( "*.*", "D" )
      oBrowse:dataLink := DacPagedDataStore():new( aDirectory, ;
                                                   aColumns    )
      oBrowse:dataLink:SetAbsolutePagesize( oBrowse:RowCount )
      oBrowse:refreshAll()

      oBrowse:setPointer( NIL, XBPSTATIC_SYSICON_ARROW, XBPWINDOW_POINTERTYPE_SYSPOINTER )
   ENDIF
RETURN


/*
 * Create std dialog window hidden
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
