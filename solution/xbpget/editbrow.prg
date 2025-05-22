//////////////////////////////////////////////////////////////////////
//
//  EDITBROW.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      Example for an editable browser.
//   
//  Remarks:
//      The Edit feature is achieved with the XbpGetColumn class. This class
//      is programmed in EDITCOL.PRG and communicates with an XbpGet object
//      that actually is used as edit control when the Enter key is pressed 
//      in the browser.
//   
//      The Browse window is built as follows
//   
//        XbpDialog
//         contains XbpBrowse
//                   contains XbpGetColumn
//                                contains XbpGet
//   
//////////////////////////////////////////////////////////////////////


#pragma Library( "XppUI2.LIB" )

#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"
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

   IF ! DbeLoad( "FOXDBE" )
      MsgBox( "Unable to load FOXDBE" )
   ENDIF

   DbeSetDefault( "DBFDBE" )
RETURN

 
PROCEDURE Main ( name )
   LOCAL nEvent, mp1, mp2, oXbp, oBrowse, aStruct, i, imax, bError, cDbe
   LOCAL aPP, oColumn

   DEFAULT name TO GetFilename()

   IF Empty( name )
      MsgBox ( "No Filename specified" + CHR(10) + CHR(10)+ ;
               "Usage: Editbrow [<DbfName>]" )
      RETURN
   ENDIF
 
   cDBE   := "DBFDBE"
   FOR i:=1 TO 2

      bError := ErrorBlock( {|oErr| Break( oErr ) } )
      BEGIN SEQUENCE
         DbUseArea( .T., cDBE, name )
      RECOVER
         ErrorBlock( bError )
         
         IF i== 1
            cDBE := "FOXDBE"
            LOOP
         ENDIF
      ENDSEQUENCE
      ErrorBlock( bError )

      EXIT
   NEXT


   IF ! Used()
      MsgBox( "Unable to open file" + CHR(10) + CHR(10)+ Name )
      QUIT
   ENDIF

   // create std dialog
   oXbp := GuiStdDialog( "Standard GUI Browser for DBF" )

   // create browser in window
   oBrowse := GuiBrowseDb( oXbp:drawingArea )

   // create columns for all fields but exclude FoxPro specific data types
   imax    := FCount()
   aStruct := DbStruct()
   aPP     := {{ XBP_PP_COL_HA_CAPTION, "" }}

   FOR i:=1 TO imax
      IF .NOT. aStruct[i,2] $ "BOVTXYM"
         aPP[1,2] := aStruct[i,1]
         oColumn  := XbpGetColumn():new( oBrowse,,,, aPP )
         oColumn:dataLink := EditBlock( aStruct[i,1] )
         oBrowse:addColumn( oColumn )
      ENDIF
   NEXT

   // Overload resize so that the browser fills the window
   oXbp:drawingArea:resize := ;
       {|mp1,mp2,obj| obj:childList()[1]:setSize(mp2) }
 
   // Delegate keys for editing to the XbpGetColumn object
   oBrowse:keyboard := ;
      {|nKey, x, obj| IIf( nKey > 31 .OR. nKey < 256, ;
                           obj:getColumn( obj:colPos ):keyboard( nKey ), ;
                           NIL ) }
 
   // show browser and set focus on it
   oXbp:show()
   oBrowse:show()
   SetAppFocus( oBrowse )

   DO WHILE nEvent <> xbeP_Close
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
* Get filename with filedialog
******************************************************************
FUNCTION GetFilename()
   LOCAL oDlg := XbpFiledialog():New(), cFile

   oDlg:Title := "Please select DBF file"
   oDlg:Create()
   cFile := oDlg:Open( "..\..\data\misc\*.dbf" )

RETURN cFile


******************************************************************
* Data code block for fields 
******************************************************************
FUNCTION EditBlock( cFieldName )
   LOCAL bBlock, cBlock
   IF FieldPos( cFieldName ) <> 0
      IF ! "->" $ cFieldName
         cFieldName := "FIELD->"+cFieldName
      ENDIF

      cBlock := "{|x| IIf(x==NIL,"+cFieldName+",WriteField('"+cFieldName+"',x) ) }" 
      bBlock := &cBlock
   ENDIF
RETURN bBlock


******************************************************************
* Write value to field when record lock can be obtained
******************************************************************
FUNCTION WriteField( cField, xValue )
   IF RLock()
      &cField := xValue
      DbUnlock()
   ENDIF
RETURN xValue

// EOF
//////
