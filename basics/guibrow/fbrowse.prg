//////////////////////////////////////////////////////////////////////
//
//  FBROWSE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample for XbpBrowse
//   
//  Remarks:
//      The Directory() array is used as an example for browsing a two
//      dimensional array with the XbpBrowse class. The first column of
//      the browser displays file icons while all other columns display
//      the contents of the Directory() array.
//   
//      Sorting order can be selected using a context menu
//      (file name, size, date, time)
//   
//      Different sorting orders for display are achieved with a second
//      array which is created in the DirArray() function below.
//   
//  Syntax:
//      FBROWSE
//   
//  Return:
//      returns 0 always 
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Gra.ch"   
#include "Xbp.ch"   
#include "Font.ch"
#include "Directry.ch"

#define  PRES_PARAM     { ;       // Presentation parameters for the browser
         {XBP_PP_COL_HA_FGCLR          , XBPSYSCLR_WINDOWSTATICTEXT   }, ;
         {XBP_PP_COL_HA_BGCLR          , XBPSYSCLR_DIALOGBACKGROUND   }, ;
         {XBP_PP_COL_HA_HEIGHT         , 20                           }, ;
         {XBP_PP_COL_DA_FGCLR          , XBPSYSCLR_WINDOWSTATICTEXT   }, ;
         {XBP_PP_COL_DA_BGCLR          , XBPSYSCLR_WINDOW             }, ;
         {XBP_PP_COL_DA_HILITE_FGCLR   , XBPSYSCLR_HILITEFOREGROUND   }, ;
         {XBP_PP_COL_DA_HILITE_BGCLR   , XBPSYSCLR_HILITEBACKGROUND   }, ;
         {XBP_PP_COL_DA_COMPOUNDNAME   , FONT_DEFPROP_SMALL           }, ;
         {XBP_PP_COL_DA_ROWHEIGHT      , 20                           }, ;
         {XBP_PP_COL_DA_CELLFRAMELAYOUT, XBPFRAME_DOTTED+XBPFRAME_BOX }, ;
         {XBP_PP_COL_DA_FRAMELAYOUT    , XBPFRAME_NONE+XBPFRAME_BOX   }, ;
         {XBP_PP_COL_DA_ROWSEPARATOR   , XBPCOL_SEP_LINE+XBPCOL_SEP_DOTTED }, ;
         {XBP_PP_COL_DA_COLSEPARATOR   , XBPCOL_SEP_LINE+XBPCOL_SEP_DOTTED }, ;
         {XBP_PP_COL_FA_FGCLR          , XBPSYSCLR_WINDOWSTATICTEXT   }, ;
         {XBP_PP_COL_FA_BGCLR          , XBPSYSCLR_3DFACE             }  }

// structure of the array for oBrowse:cargo
#define  ARR_SRC   1              // Directory() array (Data source)
#define  ARR_NO    2              // 'record pointer' for array
#define  ARR_SORT  3              // 'Index' for array
#define  ARR_ORD   4              // Column number for ASort()
#define  ARR_PATH  5              // Current directory

#define  ARR_COUNT 5
                                  // Full qualified name of current directory


PROCEDURE AppSys
// Desktop remains application window
RETURN


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, i, oDlg, oBrowse, oMenu
   LOCAL aArray := DirArray(), aHeading[ F_LEN ]

   // Create dialog window hidden
   oDlg := GuiStdDialog( "GUI Directory Browser" )

   // The browse is resized to fill the entire window
   oDlg:drawingArea:resize := ;
      {|mp1,mp2,obj| obj:childList()[1]:setSize(mp2) }

   // Create browser in window
   oBrowse := GuiArrayBrowse( oDlg:drawingArea, , , PRES_PARAM, aArray )

   // Width and heading for columns
   aHeading[ F_NAME          ] := { 15, "File name"     }
   aHeading[ F_SIZE          ] := {  8, "Size"          }
   aHeading[ F_WRITE_DATE    ] := {  8, "Date"          }
   aHeading[ F_WRITE_TIME    ] := {  8, "Time"          }
   aHeading[ F_ATTR          ] := {  8, "Attribute"     }
   aHeading[ F_EA_SIZE       ] := {  8, "Size of attr." }
   aHeading[ F_CREATION_DATE ] := {  8, "Creation date" }
   aHeading[ F_CREATION_TIME ] := {  8, "Creation time" }
   aHeading[ F_ACCESS_DATE   ] := {  8, "Access date"   }
   aHeading[ F_ACCESS_TIME   ] := {  8, "Access time"   }
                                                     
   // First column displays file icons
   oBrowse:addColumn ( IconBlock( aArray ), 2, "", , XBPCOL_TYPE_FILEMINIICON )

   // Add XbpColumn objects for each column of the array
   FOR i:=1 TO F_LEN
      oBrowse:addColumn( DirBlock( aArray, i ), aHeading[i,1], aHeading[i,2] )
   NEXT

   // Color for sorted column
   oBrowse:getColumn( F_NAME + 1 ):colorBlock := {|| { GRA_CLR_RED, NIL } }

   // Create context menu
   oMenu := ContextMenu( oBrowse )

   // Freeze first column
   oBrowse:setLeftFrozen( { 1 } )

   // Disallow browse cursor to enter first column after xbeK_LEFT
   oBrowse:navigate := ;
     {|mp1,mp2,obj| IIf( mp1 == XBPBRW_Navigate_PrevCol .AND. obj:colPos==2, NIL, mp1 ) }

   // Context menu is activated by a right mouse click
   oBrowse:itemRbDown := {|aPos,nRow,obj| oMenu:popUp( obj, aPos ) }

   // Cange directory after left double click or Enter key
   oBrowse:itemSelected := {|aPos,nRow,obj| ChangeDirectory( obj ) }

   // Display window
   oDlg:show()

   // Display browser. It was hidden up to here. Therefore
   // the :show() method must be called explicitly in order
   // to get positions for columns and cells calculated.
   oBrowse:show()

   // Browse cursor starts in second column
   oBrowse:right()
   oBrowse:forceStable()

   // Set focus to browser
   SetAppFocus( oBrowse )

   // Event loop
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )

      IF nEvent == xbeP_Keyboard .AND. oXbp == oBrowse
         DO CASE
         CASE mp1 == xbeK_ALT_1 ; SortFiles( oBrowse, F_NAME ) 
         CASE mp1 == xbeK_ALT_2 ; SortFiles( oBrowse, F_SIZE ) 
         CASE mp1 == xbeK_ALT_3 ; SortFiles( oBrowse, F_WRITE_DATE ) 
         CASE mp1 == xbeK_ALT_4 ; SortFiles( oBrowse, F_WRITE_TIME ) 
         ENDCASE
      ENDIF
   ENDDO
RETURN


******************************************************************************
* Create an array which contains all data necessary to browse a 2-dim array
* - ARR_SRC : Data source (2-dim array = Directory())
* - ARR_NO  : 'record pointer'
* - ARR_ORD : Number of column for ASort()
* - ARR_SORT: 1-dim array containing an 'index' to ARR_SRC
******************************************************************************
FUNCTION DirArray()
   LOCAL aArray[ ARR_COUNT ], nLen

   aArray[ ARR_SRC  ] := Directory( "*.*", "D" )
   aArray[ ARR_NO   ] := 1
   aArray[ ARR_ORD  ] := F_NAME
   aArray[ ARR_PATH ] := FullPath()

   // Create an array containing successive numbers (the 'index')
   nLen               := Len( aArray[ ARR_SRC ] )
   aArray[ ARR_SORT ] := AEval( Array(nLen), {|x,i| x:=i } ,,, .T. )

   // Sort successive numbers according to values of the data source array
   // (initial sort is by file name)
   ASort( aArray[ ARR_SORT ] ,,, ;
      {|i,j| aArray[ ARR_SRC, i, F_NAME ] < ;
             aArray[ ARR_SRC, j, F_NAME ] } )

RETURN aArray


******************************************************************************
* Create an XbpBrowse object with navigation code blocks for an array
* - aArray : This array contains the data source (2-dim array),
*            plus 'record pointer' and 'index' for the data source.
*            It is referenced in :cargo
******************************************************************************
FUNCTION GuiArrayBrowse( oParent, aPos, aSize, aPresParam, aArray )
   LOCAL oBrowse

   oBrowse := XbpBrowse():new( oParent, ,aPos, aSize, aPresParam ):create()
   oBrowse:cargo         := aArray
   oBrowse:skipBlock     := {|n,obj| SkipArray( n, obj:cargo ) }
   oBrowse:goTopBlock    := {|obj| obj:cargo[ ARR_NO ] := 1 }
   oBrowse:goBottomBlock := {|obj| obj:cargo[ ARR_NO ] := Len( obj:cargo[ ARR_SRC ] ) }
   oBrowse:posBlock      := {|obj| obj:cargo[ ARR_NO ]  }
   oBrowse:phyPosBlock   := {|obj| obj:cargo[ ARR_SORT, obj:cargo[ ARR_NO ] ] }
   oBrowse:goPhyPosBlock := {|n,obj| obj:cargo[ ARR_SORT, obj:cargo[ ARR_NO ] ] := n}
   oBrowse:lastPosBlock  := {|obj| Len( obj:cargo[ ARR_SRC ] ) }
   oBrowse:firstPosBlock := {|obj| 1  }

RETURN oBrowse


******************************************************************************
* Create a context menu to select sorting order
******************************************************************************
FUNCTION ContextMenu( oBrowse )
   LOCAL oMenu := XbpMenu( oBrowse ):new():create()

   oMenu:addItem( { " ~Name ", {|| SortFiles( oBrowse, F_NAME       ) } } )
   oMenu:addItem( { " ~Size ", {|| SortFiles( oBrowse, F_SIZE       ) } } )
   oMenu:addItem( { " ~Date ", {|| SortFiles( oBrowse, F_WRITE_DATE ) } } )
   oMenu:addItem( { " ~Time ", {|| SortFiles( oBrowse, F_WRITE_TIME ) } } )

RETURN oMenu


******************************************************************************
* Return a :dataLink code block for the array created by DirArray()
* - nSubScript is a detached LOCAL variable.
*   It determines the column of the data source to display
******************************************************************************
FUNCTION DirBlock( aArray, nSubScript )
RETURN {|nElement| nElement := aArray[ ARR_NO ], ;
                   nElement := aArray[ ARR_SORT, nElement ], ;
                   aArray[ ARR_SRC, nElement, nSubScript ] }


******************************************************************************
* Return a :dataLink code block for the array created by DirArray()
* It returns a full qualified file name -> to display file icons
******************************************************************************
FUNCTION IconBlock( aArray )
RETURN {|nElement| nElement := aArray[ ARR_NO ], ;
                   nElement := aArray[ ARR_SORT, nElement ], ;
                   aArray[ ARR_SRC, nElement, F_NAME ] }


******************************************************************************
* Function to maintain the 'record pointer' for arrays
* It is called via oBrowse:skipBlock 
******************************************************************************
FUNCTION SkipArray( nWantSkip, aArray )
   LOCAL nDidSkip, nLastRec := Len( aArray[ ARR_SRC ] )

   IF aArray[ ARR_NO ] + nWantSkip < 1              // "BoF"
      nDidSkip := 1 - aArray[ ARR_NO ]
   ELSEIF aArray[ ARR_NO ] + nWantSkip > nLastRec   // "EoF"
      nDidSkip := nLastRec - aArray[ ARR_NO ]
   ELSE
      nDidSkip := nWantSkip
   ENDIF

   aArray[ ARR_NO ] += nDidSkip

RETURN nDidSkip


******************************************************************************
* Change the sorting order of the data source
* The 'index' array is sorted according to values of the data source array
******************************************************************************
PROCEDURE SortFiles( oBrowse, nSubScript )
   LOCAL aArray  := oBrowse:cargo
   LOCAL oColumn := oBrowse:getColumn( aArray[ ARR_ORD ] + 1 )
   LOCAL bBlock  := oColumn:colorBlock

   ASort( aArray[ ARR_SORT ] ,,, ;
      {|i,j| aArray[ ARR_SRC, i, nSubScript ] < ;
             aArray[ ARR_SRC, j, nSubScript ] } )

   oColumn:colorBlock := NIL
   aArray[ ARR_ORD ]  := nSubScript
   oBrowse:getColumn( nSubScript + 1 ):colorBlock := bBlock

   oBrowse:goTop()
   oBrowse:refreshAll()

RETURN


******************************************************************************
* Change directory and update data source array
* Note: The array reference in oBrowse:cargo must not be changed!
* Only arrays may be changed which are referenced in the array oBrowse:cargo!
* Otherwise all oXbpColumn:dataLink code blocks must be re-created, too
******************************************************************************
PROCEDURE ChangeDirectory( oBrowse )
   LOCAL aArray  := oBrowse:cargo
   LOCAL i       := aArray[ ARR_SORT, aArray[ ARR_NO ] ]
   LOCAL aFile   := aArray[ ARR_SRC , i ]
   LOCAL cPath   := aArray[ ARR_PATH ]
   LOCAL lChange := ( "D" $ aFile[ F_ATTR ] )
   LOCAL cDir, nLen

   IF ! lChange
      RETURN                           // Selected file is no directory
   ENDIF

   cDir  := aFile[ F_NAME ]
   cPath := SubStr( cPath, 1, Len(cPath) - 1 )

   IF cDir == "."                      // Current directory is selected
      lChange := .F.

   ELSEIF cDir == ".."                 // Parent directory is selected

      i := RAt( "\", cPath )
      IF i > 0
         cDir := SubStr( cPath, 1, i-1 )
         IF RAt( ":", cPath ) == 2
            cDir += "\"
         ENDIF
      ELSE
         lChange := .F.                // Root directory is current
      ENDIF

   ELSE
      cDir := cPath + "\" + cDir       // Child directory is selected
   ENDIF

   IF ! lChange
      RETURN                           // Directory cannot be changed
   ENDIF

   CurDir( cDir )                      // Change current directory
   aArray[ ARR_PATH ] := FullPath()
   aArray[ ARR_SRC  ] := Directory( "*.*", "D" )
   nLen               := Len( aArray[ ARR_SRC ] )
   aArray[ ARR_SORT ] := AEval( Array(nLen), {|x,i| x:=i } ,,, .T. )
   SortFiles( oBrowse, aArray[ ARR_ORD ] )

RETURN


******************************************************************************
* Create a default dialog window hidden
******************************************************************************
FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg

   DEFAULT cTitle TO "Standard Dialog Window"

   oDlg          := XbpDialog():new( ,,{10,10}, {600,400},, .F. )
   oDlg:icon     :=  1
   oDlg:taskList := .T.
   oDlg:title    := cTitle
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( FONT_DEFPROP_SMALL )

RETURN oDlg


******************************************************************************
* Return full path name
******************************************************************************
FUNCTION FullPath()

   LOCAL cPath := CurDrive() + ":\"
   IF !Empty( CurDir() )
      cPath := cPath + CurDir() + "\"
   ENDIF

RETURN cPath
