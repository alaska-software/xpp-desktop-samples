//////////////////////////////////////////////////////////////////////
//
//  APPDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program creates ApplicationPARTs using the commands
//      APPEDIT and APPBROWSE. APPs are selected via a simple
//      selection dialog which is implemented in the user defined
//      AppSelection() class.
//   
//  Remarks:
//      Only one database file is opened. All APPs are registered in the
//      corresponding workarea. Therefore, APPs are synchronized automatically.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Font.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Appedit.ch"
#include "Appbrow.ch"

#define  xbeP_Eval                     xbeP_User + 1

MEMVAR   appObject



/*
 * Application PARTs are easily positioned
 */
PROCEDURE AppEditPlain

 LOCAL oEdit

   APPEDIT INTO oEdit STYLE PLAIN POSITION TOP,LEFT

   oEdit:useVisualStyle := .F.

   APPDISPLAY oEdit MODELESS
RETURN


PROCEDURE AppEdit3D
   APPEDIT STYLE 3D POSITION TOP,CENTER

   APPDISPLAY MODELESS
RETURN



/*
 * Styles can be defined using keywords or #defines
 */
PROCEDURE AppEditFancy1
   APPEDIT STYLE FANCY POSITION CENTER

   APPDISPLAY MODELESS
RETURN


PROCEDURE AppEdit3D1

 LOCAL oEdit

   APPEDIT INTO oEdit POSITION TOP,RIGHT ;
           STYLE APPSTYLE_ACTIONICONS+APPSTYLE_RAISED+APPSTYLE_ACTIONTOP

   oEdit:useVisualStyle := .F.

   APPDISPLAY oEdit MODELESS
RETURN




/*
 * User defined background bitmaps are supported
 */
PROCEDURE AppEditFancy2

 LOCAL oEdit

   APPEDIT INTO oEdit STYLE FANCY POSITION CENTER BITMAP 2001 FIT

   oEdit:useVisualStyle := .F.

   APPDISPLAY oEdit MODELESS
RETURN



/*
 * Syntax of APPBROWSE is almost identical to APPEDIT
 */
PROCEDURE AppBrowseSystem
   APPBROWSE STYLE SYSTEM POSITION BOTTOM,LEFT

   APPDISPLAY MODELESS
RETURN


PROCEDURE AppBrowsePlain

 LOCAL oBrowse

   APPBROWSE INTO oBrowse STYLE PLAIN POSITION BOTTOM,LEFT

   oBrowse:useVisualStyle := .F.

   APPDISPLAY oBrowse MODELESS
RETURN


PROCEDURE AppBrowse3D

 LOCAL oBrowse

   APPBROWSE INTO oBrowse STYLE 3D POSITION BOTTOM,CENTER

   oBrowse:useVisualStyle := .F.

   APPDISPLAY oBrowse MODELESS
RETURN


PROCEDURE AppBrowseFancy

 LOCAL oBrowse

   APPBROWSE INTO oBrowse STYLE FANCY POSITION BOTTOM, RIGHT

   oBrowse:useVisualStyle := .F.

   APPDISPLAY oBrowse MODELESS
RETURN



/*
 * Two Application Parts sharing the same parent dialog window
 */
PROCEDURE TwoApps
   LOCAL oDlg

   oDlg := XbpDialog():new()
   oDlg:taskList  := .T.
   oDlg:title     := "APPEDIT combined with APPBROWSE"
   oDlg:maxButton := .F.
   oDlg:close     := {|mp1,mp2,obj| obj:destroy() }
   oDlg:border    := XBPDLG_RAISEDBORDERTHIN_FIXED
   oDlg:create(,, {100,10}, {600,580} )

   APPBROWSE  ;
      PARENT oDlg:drawingArea ;
    POSITION TOP ;
        SIZE 40, 100 PERCENT

   APPDISPLAY MODELESS

   APPEDIT STYLE APPSTYLE_RAISED      + APPSTYLE_ACTIONBOTTOM + ;
                 APPSTYLE_ACTIONICONS + APPSTYLE_HORIZONTAL ;
    PARENT oDlg:drawingArea ;
  POSITION BOTTOM ;
      SIZE 60, 100 PERCENT

   APPDISPLAY MODELESS

RETURN



PROCEDURE AppSys
// Desktop remains application window
RETURN

PROCEDURE AppQuit
   CLOSE ALL
   QUIT
RETURN


******************************************************************************
******************************************************************************
PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp

   SET DEFAULT TO "..\..\data\misc"
   USE customer

   SET TYPEAHEAD TO 30

   oXbp       := AppSelection():new():create()
   oXbp:close := {|| AppQuit() }

   oXbp:addItem( "APPEDIT PLAIN *)"   , {|| AppEditPlain()   } )
   oXbp:addItem( "APPEDIT 3D"       , {|| AppEdit3D()      } )
   oXbp:addItem( "APPEDIT 3D1 *)"     , {|| AppEdit3D1()     } )
   oXbp:addItem( "APPEDIT FANCY 1"  , {|| AppEditFancy1()  } )
   oXbp:addItem( "APPEDIT FANCY 2"  , {|| AppEditFancy2()  } )
   oXbp:addItem( "APPBROWSE PLAIN *)" , {|| AppBrowsePlain() } )
   oXbp:addItem( "APPBROWSE 3D *)"    , {|| AppBrowse3D()    } )
   oXbp:addItem( "APPBROWSE FANCY *)" , {|| AppBrowseFancy() } )
   oXbp:addItem( "APPBROWSE SYSTEM" , {|| AppBrowseSystem() } )
   oXbp:addItem( "APPBROWSE+APPEDIT"  , {|| TwoApps()        } )

   oXbp:show()
   SetAppFocus( oXbp:listBox )

   DO WHILE .T.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      IF nEvent == xbeP_Eval
         Eval( mp1 )
      ELSE
         oXbp:handleEvent( nEvent, mp1, mp2 )
      ENDIF
   ENDDO
RETURN



/*
 * Simple dialog to select Application Parts
 */
CLASS AppSelection FROM XbpDialog
   PROTECTED:
   VAR aItems
   VAR aBlocks

   EXPORTED:
   VAR staticBox
   VAR staticTxt
   VAR listBox
   VAR buttonOK

   METHOD init
   METHOD create
   METHOD addItem
   METHOD itemSelected
ENDCLASS



/*
 * Initialize object
 */
METHOD AppSelection:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO SetAppWindow(), ;
           aPos     TO {76,162}, ;
           aSize    TO {216,360}, ;
           lVisible TO .F.

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:border   := XBPDLG_RECESSEDBORDERTHIN_FIXED
   ::XbpDialog:icon     := 1
   ::XbpDialog:taskList := .T.
   ::XbpDialog:maxButton:= .F.
   ::XbpDialog:title    := "Application Parts"
   ::XbpDialog:visible  := lVisible

   ::staticBox := XbpStatic():new( ::drawingArea, , {17,67}, {176,248} )
   ::listBox   := XbpListBox():new( ::staticBox, , {4,3}, {168,242} )
   ::staticTxt := XbpStatic():new( ::drawingArea, , {17,45}, {150,20} )
   ::staticTxt:caption := "*) Visual styles disabled"

   ::buttonOK  := XbpPushButton():new( ::drawingArea, , {53,13}, {96,30} )
   ::aItems    := {}
   ::aBlocks   := {}

   ::staticBox:type := XBPSTATIC_TYPE_RAISEDRECT
   ::listBox:itemSelected := {|| ::itemSelected() }
   ::buttonOK:caption := "Close"
   ::buttonOK:activate := {|| PostAppEvent( xbeP_Close,,, ::XbpDialog ) }
RETURN self



/*
 * Request system resources
 */
METHOD AppSelection:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   ::staticBox:create()
   ::staticTxt:create()
   ::listBox:create()
   ::buttonOK:create()

RETURN self



/*
 * Add list box item and code block
 */
METHOD AppSelection:addItem( cItem, bBlock )

   IF Valtype( cItem ) + Valtype( bBlock ) == "CB"
      ::ListBox:addItem( cItem )
      AAdd( ::aBlocks, bBlock )
   ENDIF

RETURN self



/*
 * Selection -> put code block into event queue
 *              and post a user defined event
 */
METHOD AppSelection:itemSelected
   LOCAL aItem := ::listBox:getData()
   LOCAL nItem := IIf( Empty( aItem ), 0, aItem[1] )

   IF nItem > 0
      PostAppEvent( xbeP_Eval, ::aBlocks[nItem] )
   ENDIF

RETURN self
