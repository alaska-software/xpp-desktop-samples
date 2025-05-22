//////////////////////////////////////////////////////////////////////
//
//  COLTEST.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      Example for an editable browser.
//   
//  Remarks:
//      Different Xbp???Column objects are used for editing.
//   
//////////////////////////////////////////////////////////////////////



#pragma Library( "XppUI2.LIB" )

#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"
 
 
PROCEDURE AppSys
   // no Crt instance is created
RETURN
 
PROCEDURE DbeSys
   // This program does not use index files.
   // Therefore, we do not load DBE ORDER components.
   IF ! DbeLoad( "DBFDBE" )
      MsgBox( "Unable to load DBFDBE" )
   ENDIF

   DbeSetDefault( "DBFDBE" )
RETURN

PROCEDURE AppQuit
   QUIT
RETURN
 
 
PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, oBrowse, bError

   bError := ErrorBlock( {|oErr| Break( oErr ) } )
   BEGIN SEQUENCE
      USE ..\..\data\misc\customer.dbf
   RECOVER
      ErrorBlock( bError )
      MsgBox( "Unable to open file" + CHR(10) + CHR(10)+ "..\..\data\misc\customer.dbf" )
      QUIT
   ENDSEQUENCE
   ErrorBlock( bError )

   // create std dialog
   oXbp := GuiStdDialog( "Testing editable columns in browser" )
   SetAppWindow( oXbp )
   oXbp:close := {|| AppQuit() }


   // create browser in window
   oBrowse := GuiBrowseDb( oXbp:drawingArea )
   oBrowse:softTrack := .F.

   // Add the columns to browser
   AddColumns( oBrowse )

   // Overload resize so that the browser fills the window
   oXbp:drawingArea:resize := ;
       {|mp1,mp2,obj| obj:childList()[1]:setSize(mp2) }
 
   // Delegate keys for editing to the Xbp???Column object
   oBrowse:keyboard := ;
      {|nKey, x, obj| IIf( nKey > 31 .OR. nKey < 256, ;
                           obj:getColumn( obj:colPos ):keyboard( nKey ), ;
                           NIL ) }

   // show browser and set focus on it
   oXbp:show()
   oBrowse:show()
   SetAppFocus( oBrowse )

   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN
 
 
******************************************************************
* Create GUI Browser with navigation codeblocks
******************************************************************
FUNCTION GuiBrowseDB( oParent, aPos, aSize )
   LOCAL oBrowse
 
   oBrowse := XbpBrowse():new( oParent,, aPos, aSize,, .F. )
   oBrowse:cursorMode := XBPBRW_CURSOR_CELL
   oBrowse:create()

   // navigation codeblocks for the browser
   oBrowse:skipBlock     := {|n| DbSkipper(n) }
   oBrowse:goTopBlock    := {| | DbGoTop()    }
   oBrowse:goBottomBlock := {| | DbGoBottom() }
   oBrowse:phyPosBlock   := {| | Recno()      }

   // Code blocks for the vertical scrollbar.
   // Note: DbPosition() returns values in the range from 0 to 100.
   oBrowse:posBlock      := {| | DbPosition()    }
   oBrowse:goPosBlock    := {|n| DbGoPosition(n) } 
   oBrowse:lastPosBlock  := {| | 100             }
   oBrowse:firstPosBlock := {| | 0               }
 
RETURN oBrowse
  

******************************************************************
* Create std dialog window hidden
******************************************************************
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


******************************************************************
* Write value to field when record lock can be obtained
******************************************************************
FUNCTION WriteField( cField, xValue )
   IF RLock()
      &cField := xValue
      DbUnlock()
   ENDIF
RETURN xValue



******************************************************************
* Define the columns for the browser
******************************************************************
PROCEDURE AddColumns( oBrowse )
   LOCAL aPP, oColumn, bBlock
   LOCAL aMrMrs := {   ; 
              "Dr."  , ;
              "Frau" , ;
              "Herr" , ;
              "Miss" , ;
              "Mr."  , ;
              "Mrs." , ;
              "Sir"  , ;
              "Sirs"   }

   LOCAL aState := {   ;
              "AK"   , ;
              "CA"   , ;
              "CO"   , ;
              "FL"   , ;
              "IL"   , ;
              "MD"   , ;
              "MI"   , ;
              "MN"   , ;
              "ND"   , ;
              "NJ"   , ;
              "NY"   , ;
              "TN"   , ;
              "TX"   , ;
              "WY"     }

   aPP := { { XBP_PP_COL_HA_CAPTION          , ""               }, ;
            { XBP_PP_COL_FA_CAPTION          , ""               }, ;
            { XBP_PP_COL_DA_FGCLR            , GRA_CLR_BLACK    }, ; 
            { XBP_PP_COL_DA_BGCLR            , GRA_CLR_WHITE    }, ; 
            { XBP_PP_COL_DA_HILITE_FGCLR     , GRA_CLR_WHITE    }, ; 
            { XBP_PP_COL_DA_HILITE_BGCLR     , GRA_CLR_DARKGRAY }, ; 
            { XBP_PP_COL_DA_HILITEFRAMELAYOUT, XBPFRAME_BOX + XBPFRAME_RECT + XBPFRAME_DOTTED }, ;
            { XBP_PP_COL_DA_CELLFRAMELAYOUT  , XBPFRAME_BOX               }, ;
            { XBP_PP_COL_DA_COLSEPARATOR     , XBPCOL_SEP_NONE            }, ;
            { XBP_PP_COL_DA_FRAMELAYOUT      , XBPFRAME_BOX               }, ;
            { XBP_PP_COL_DA_HILITE_BGCLR     , XBPSYSCLR_HILITEBACKGROUND }  }

   // Spinbutton: FIELD->CUSTNO
   bBlock               := {|x| IIf(x==NIL, Val(FIELD->CUSTNO), WriteField( "FIELD->CUSTNO", LTrim(Str(x)) ) ) }
   aPP[1,2]             := "CustNo"
   aPP[2,2]             := "SpinButton"
   oColumn              := XbpSpinbuttonColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:setNumLimits( 0, 999999 )
   oBrowse:addColumn( oColumn )
 
   // Read-only combobox: FIELD->MR_MRS
   bBlock               := {|x| IIf(x==NIL, Trim(FIELD->MR_MRS), WriteField( "FIELD->MR_MRS", Trim(x) ) ) }
   aPP[1,2]             := "Mr_Mrs"
   aPP[2,2]             := "RO Combobox"
   oColumn              := XbpComboboxColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:comboType    := XBPCOMBO_DROPDOWNLIST
   oColumn:setItems ( aMrMrs )
   oBrowse:addColumn( oColumn )
 
   // XbpGet: FIELD->LASTNAME
   bBlock               := {|x| IIf(x==NIL, Trim(FIELD->LASTNAME), WriteField( "FIELD->LASTNAME", x) ) }
   aPP[1,2]             := "LastName"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Eval( bBlock ) )
   oBrowse:addColumn( oColumn )
 
   // XbpGet: FIELD->FIRSTNAME
   bBlock               := {|x| IIf(x==NIL, FIELD->FIRSTNAME, WriteField( "FIELD->FIRSTNAME", x) ) }
   aPP[1,2]             := "FirstName"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Eval( bBlock ) )
   oBrowse:addColumn( oColumn )
   
   // XbpGet: FIELD->STREET
   bBlock               := {|x| IIf(x==NIL, FIELD->STREET, WriteField( "FIELD->STREET", x) ) }
   aPP[1,2]             := "Street"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Eval( bBlock ) )
   oBrowse:addColumn( oColumn )
 
   // XbpGet: FIELD->CITY
   bBlock               := {|x| IIf(x==NIL, FIELD->CITY, WriteField( "FIELD->CITY", x) ) }
   aPP[1,2]             := "City"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Eval( bBlock ) )
   oBrowse:addColumn( oColumn )
 
   // Editable combobox: FIELD->STATE
   bBlock               := {|x| IIf(x==NIL, Trim(FIELD->STATE), WriteField( "FIELD->STATE", Trim(x) ) ) }
   aPP[1,2]             := "State"
   aPP[2,2]             := "Combobox"
   oColumn              := XbpComboboxColumn():new( oBrowse,,,, aPP )
   oColumn:comboType := XBPCOMBO_DROPDOWN
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := 2
   oColumn:setItems ( aState )
   oBrowse:addColumn( oColumn )
  
   // XbpGet: FIELD->PHONE
   bBlock               := {|x| IIf(x==NIL, FIELD->PHONE, WriteField( "FIELD->PHONE", x) ) }
   aPP[1,2]             := "Phone"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Eval( bBlock ) )
   oBrowse:addColumn( oColumn )
 
   // XbpGet: FIELD->FAX
   bBlock               := {|x| IIf(x==NIL, FIELD->FAX, WriteField( "FIELD->FAX", x) ) }
   aPP[1,2]             := "Fax"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Eval( bBlock ) )
   oBrowse:addColumn( oColumn )
 
   // XbpGet: FIELD->BLOCKED
   bBlock               := {|x| IIf(x==NIL, FIELD->BLOCKED, WriteField( "FIELD->BLOCKED", x) ) }
   aPP[1,2]             := "Blocked"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oBrowse:addColumn( oColumn )
 
   // XbpGet: FIELD->TOTALSALES
   bBlock               := {|x| IIf(x==NIL, FIELD->TOTALSALES, WriteField( "FIELD->TOTALSALES", x) ) }
   aPP[1,2]             := "TotalSales"
   aPP[2,2]             := "XbpGet"
   oColumn              := XbpGetColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := Len( Str( Eval( bBlock ) ) )
   oBrowse:addColumn( oColumn )

   // XbpMle: FIELD->NOTES
   bBlock               := {|x| IIf(x==NIL, FIELD->NOTES, WriteField( "FIELD->NOTES", x) ) }
   aPP[1,2]             := "Notes"
   aPP[2,2]             := "XbpMle"
   oColumn              := XbpMleColumn():new( oBrowse,,,, aPP )
   oColumn:dataLink     := bBlock
   oColumn:bufferLength := 50
   oBrowse:addColumn( oColumn )
RETURN

// EOF
//////
