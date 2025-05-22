//////////////////////////////////////////////////////////////////////
//
//  SLEDLG.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Class SimpleEditDialog() for demonstrating GroupManager objects.
//      The latter provide for automatic positioning of XbaseParts within
//      sizeable windows.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Xbp.ch"


#ifdef DEBUG

PROCEDURE AppSys
// Ist not needed here
RETURN

PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL oDlg

   oDlg          := SimpleEditDialog():new( AppDesktop(),, {50,50}, {400,300} )
   oDlg:title    := "Simple Edit Dialog"
   oDlg:taskList := .T.
   oDlg:create()

   oDlg:createEntryField( "Day #1:" , "Monday"    )
   oDlg:createEntryField( "Day #2:" , "Tuesday"   )
   oDlg:createEntryField( "Day #3:" , "Wednesday" )
   oDlg:createEntryField( "Day #4:" , "Thursday"  )
   oDlg:createEntryField( "Day #5:" , "Friday"    )
   oDlg:createEntryField( "Day #6:" , "Saturday"  )
   oDlg:createEntryField( "Day #7:" , "Sunday"    )

   oDlg:show()
   SetAppWindow( oDlg )
   SetAppFocus( oDlg:sleMgr:part(1) )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN

#endif


/*
 * Dialog class for basic data input with XbpSLE. The dialog uses
 * three GroupManager objects for automatic positioning of
 * XbpStatic, XbpSLE and XbpPushbutton.
 */
CLASS SimpleEditDialog FROM XbpDialog
   PROTECTED:
   VAR line

   EXPORTED:
   VAR btnMgr, sleMgr, txtMgr READONLY
   VAR data
   METHOD init, create, resize
   METHOD createEntryField
   METHOD show
   METHOD getData, setData
ENDCLASS


******************************************************************************
* Initialize object
******************************************************************************
METHOD SimpleEditDialog:init( oParent, oOwner, aPos, aSize, aPP )

   DEFAULT oParent TO AppDesktop()  , ;
           oOwner  TO SetAppWindow(), ;
           aSize   TO { 400, 300 }  , ;
           aPos    TO { 0, 0 }

   ::xbpDialog:init( oParent, oOwner, aPos, aSize, aPP, .F. )
   ::xbpDialog:drawingArea:clipChildren := .T.


   ::btnMgr    := GroupManager():new( ::drawingArea, , {72,24} ):create()
   ::txtMgr    := GroupManager():new( ::drawingArea, , {72,24} ):create()
   ::sleMgr    := GroupManager():new( ::drawingArea, ,{128,24} ):create()

   ::line      := XbpStatic():new( ::drawingArea, , {12,48}, {0 ,2} )
   ::line:type := XBPSTATIC_TYPE_RAISEDLINE

   ::txtMgr:horizontal := .F.
   ::sleMgr:horizontal := .F.

RETURN self


/*
 * Create the object and calculate minimum size of dialog
 */
METHOD SimpleEditDialog:create( oParent, oOwner, aPos, aSize, aPP )
   LOCAL aMinSize[2]

   ::xbpDialog:create( oParent, oOwner, aPos, aSize, aPP )
   ::drawingArea:setFontCompoundName( "8.Helv" )


   // for a modeless dialog, user defined events must be used here
   ::btnMgr:createButton( "Ok"    , {|| ::data := ::getData() , ;
                                    PostAppEvent( xbeP_Close ) }, .T., .T. )
   ::btnMgr:createButton( "Cancel", {|| ::data := {} , ;
                                    PostAppEvent( xbeP_Close ) }, .T., .T. )
   ::btnMgr:setPos( {12,12} )

   ::line:create()

   aMinSize[1] := ( 12 + 72 + 12 + 128 + 12 )
   aMinSize[2] := ( 12 + 24 + 12 +  12 + 12 )

   aMinSize  := ::calcFrameRect( { 0, 0, aMinSize[1], aMinSize[2] } )
   ::minSize := { aMinSize[3]  , aMinSize[4] }
   ::maxSize := { AppDeskTop():currentSize()[1], aMinSize[4] }

RETURN self



/*
 * Position contained controls after resize
 */
METHOD SimpleEditDialog:resize()
   LOCAL aPos[2], aSize := ::drawingArea:currentSize()
   LOCAL aSleSize

   ::btnMgr:center( aSize[1]-24, 12 )

   ::line:setSize( { aSize[1] - 24, 2 }, .F. )
   aSleSize := { aSize[1] - 12 - 72 - 12 - 12, 24 }

   aPos[1] := 12
   aPos[2] := 50 + 12

   ::txtMgr:setPos( aPos, .F. )
   ::txtMgr:center( aSize[2]-50-24, 50 + 12 )

   aPos[1] += ( 72 + 12 )
   ::sleMgr:setPos( aPos, .F. )
   ::sleMgr:center( aSize[2]-50-24, 50 + 12 )
   ::sleMgr:setSize( aSleSize, .F. )

   ::drawingArea:invalidateRect( {1, 1, aSize[1], aSize[2]} )
RETURN self



/*
 * Create an XbpSLE plus static text
 */
METHOD SimpleEditDialog:createEntryField( cCaption, xData, nBuffLength )

   ::txtMgr:createText( cCaption, XBPSTATIC_TEXT_RIGHT )
   ::sleMgr:createSLE( xData, nBuffLength )

   ::minSize[2] += 24
   ::maxSize[2] := Min( AppDesktop():currentSize()[2], ;
                       ::maxSize[2] + 24 + ::sleMgr:maxDistance )

RETURN self


/*
 * Position controls before the dialog window becomes visible
 */
METHOD SimpleEditDialog:show
   LOCAL aSize := ::currentSize()

   aSize[1] := Max( aSize[1], ::minSize[1] )
   aSize[2] := Max( aSize[2], ::minSize[2] )


   // :setSize() triggers an xbeP_Resize event -> :resize()
   ::xbpDialog:setSize( aSize )

   ::xbpDialog:show()
RETURN


/*
 * Transfer data from array to SLEs
 */
METHOD SimpleEditDialog:setData( aArray )

   AEval( aArray, {|x,i| x:=::sleMgr:part(i):setData(x) } )

RETURN aArray



/*
 * Transfer data from SLEs to array
 */
METHOD SimpleEditDialog:getData
   LOCAL aArray[ ::sleMgr:numParts() ]

   AEval( aArray, {|x,i| x:=::sleMgr:part(i):getData() }, , , .T. )

RETURN aArray
