//////////////////////////////////////////////////////////////////////
//
//  DBBROWSE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample for XbpBrowse
//   
//  Remarks:
//      A simple sample program which demonstrates the Browser on a database.
//   
//  Syntax:
//      DBBROWSE [<cDBFName>]
//   
//  Return:
//      returns 0 always 
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"

 
 
PROCEDURE AppSys
   // no Crt instances are created
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

   DEFAULT name TO GetFilename()

   IF name == NIL
      MsgBox ( "No Filename specified" + CHR(10) + CHR(10)+ ;
               "Usage: DBBrowse [<DbfName>]" )
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
      MsgBox ( "Unable to open file" + CHR(10) + CHR(10)+ Name )
      QUIT
   ENDIF

   // create std dialog
   oXbp := GuiStdDialog( "Standard GUI Browser for DBF" )

   // create browser in window
   oBrowse := GuiBrowseDb( oXbp:drawingArea )

   // create columns for all fields but exclude FoxPro specific data types
   imax    := FCount()
   aStruct := DbStruct()
   FOR i:=1 TO imax
      IF .NOT. aStruct[i,2] $ "BOVTXY"
         oBrowse:addColumn( FieldBlockTrimmed(aStruct[i,1], aStruct[i,2]), , aStruct[i,1] )
      ENDIF
   NEXT

   // overload resize that browser fills the window
   oXbp:drawingArea:resize := ;
       {|mp1,mp2,obj| obj:childList()[1]:setSize(mp2) }
 
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
 
   oBrowse := XbpBrowse():new( oParent,, aPos, aSize,, .F. ):create()

   // navigation codeblocks for the browser
   oBrowse:skipBlock     := {|n| DbSkipper(n) }
   oBrowse:goTopBlock    := {| | DbGoTop()    }
   oBrowse:goBottomBlock := {| | DbGoBottom() }
   oBrowse:phyPosBlock   := {| | Recno()      }
   oBrowse:goPhyPosBlock := {|n| DbGoto(n)    }

   // Code blocks for the vertical scrollbar.
   // Note: DbPosition() returns values in the range from 0 to 100.
   // We multiply this with 10 to increase the granularity of
   // the vertical scrollbar.
   oBrowse:posBlock      := {| | DbPosition()*10    }
   oBrowse:goPosBlock    := {|n| DbGoPosition(n/10) } 
   oBrowse:lastPosBlock  := {| | 1000               }
   oBrowse:firstPosBlock := {| | 0                  }
 
RETURN oBrowse
 

******************************************************************
* Create std dialog window hidden
******************************************************************
FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg
 
   DEFAULT cTitle TO "Standard Dialog Window"
 
   oDlg          := XbpDialog():new( ,,{10,10}, {600,400},, .F. )
   oDlg:icon     := 1
   oDlg:taskList := .T.
   oDlg:title    := cTitle
   oDlg:drawingArea:ClipChildren := .T.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_DEFPROP_SMALL )
 
RETURN oDlg
 

******************************************************************
* Get filename with filedialog
******************************************************************
FUNCTION GetFilename()
   LOCAL oDlg := XbpFiledialog():New(), cFile

   oDlg:Title := "Please select DBF file"
   oDlg:Create()
   cFile := oDlg:Open( "..\..\data\misc\*.DBF" )

RETURN cFile


****************************************************************************
* Data code block for fields 
****************************************************************************
FUNCTION FieldBlockTrimmed( cFieldName, cType )
   LOCAL bBlock
   IF FieldPos( cFieldName ) <> 0
      IF ! "->" $ cFieldName
         cFieldName := "FIELD->"+cFieldName
      ENDIF
      IF cType = "C"
         bBlock := &( "{|x| IIf(x==NIL,RTrim("+cFieldName+"),"+cFieldName+":=x) }" )
      ELSE
         bBlock := &( "{|x| IIf(x==NIL,"+cFieldName+","+cFieldName+":=x) }" )
      ENDIF
   ENDIF
RETURN bBlock

// EOF
//////
