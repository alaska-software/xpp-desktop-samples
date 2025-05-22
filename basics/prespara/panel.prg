//////////////////////////////////////////////////////////////////////
//
//  PANEL.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      The program shows the Xbase++ #define constants that correspond
//      with the color settings of the Windows system control panel.
//   
//////////////////////////////////////////////////////////////////////



#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

PROCEDURE AppSys
RETURN


PROCEDURE Main
   LOCAL nEvent, oXbp, mp1, mp2

   PanelDialog():New():Create()

   nEvent := xbe_None
   WHILE nEvent != xbeP_Close
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent ( nEvent, mp1, mp2 ) 
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO

RETURN



/*
 * Dialog displays selectable items of the system control panel
 */
CLASS PanelDialog FROM XbpDialog
   EXPORTED:
      VAR colorDialog
      VAR aColors

      * Contained control elements
      VAR ListBox   
      VAR Static1
      VAR Static2
      VAR Static3

      METHOD init
      METHOD create
      METHOD setColor
ENDCLASS




METHOD PanelDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {23,97}, ;
           aSize    TO {240,308}, ;
           lVisible TO .T.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, FONT_HELV_SMALL } )

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "Control panel settings"

   ::ListBox         := XbpListBox():new( ::drawingArea, , {18,12}, {200,180} )

   ::Static1         := XbpStatic():new( ::drawingArea, , {18,252}, {200,24} )
   ::Static1:caption := "Open Control panel -->"
   ::Static1:options := XBPSTATIC_TEXT_LEFT + XBPSTATIC_TEXT_VCENTER

   ::Static2         := XbpStatic():new( ::drawingArea, , {18,228}, {200,24} )
   ::Static2:caption := "Appearance -->"
   ::Static2:options := XBPSTATIC_TEXT_LEFT + XBPSTATIC_TEXT_VCENTER

   ::Static3         := XbpStatic():new( ::drawingArea, , {18,204}, {200,24} )
   ::Static3:caption := "Select item"
   ::Static3:options := XBPSTATIC_TEXT_LEFT + XBPSTATIC_TEXT_VCENTER

   ::aColors         := { ;
      { "3D Objects            ", XBPSYSCLR_3DFACE          , XBPSYSCLR_BUTTONTEXT        }, ;
      { "Active Title Bar      ", XBPSYSCLR_ACTIVETITLE     , XBPSYSCLR_ACTIVETITLETEXT   }, ;
      { "Active Window Border  ", XBPSYSCLR_ACTIVEBORDER    , NIL                         }, ;
      { "Application Background", XBPSYSCLR_APPWORKSPACE    , NIL                         }, ;
      { "Desktop               ", XBPSYSCLR_BACKGROUND      , NIL                         }, ;
      { "Inactive Title Bar    ", XBPSYSCLR_INACTIVETITLE   , XBPSYSCLR_INACTIVETITLETEXT }, ;
      { "Inactive Window Border", XBPSYSCLR_INACTIVEBORDER  , NIL                         }, ;
      { "Menu                  ", XBPSYSCLR_MENU            , XBPSYSCLR_MENUTEXT          }, ;
      { "Messagebox            ", NIL                       , XBPSYSCLR_WINDOWTEXT        }, ;
      { "Selected Items        ", XBPSYSCLR_HILITEBACKGROUND, XBPSYSCLR_HILITEFOREGROUND  }, ;
      { "ToolTip               ", XBPSYSCLR_INFOBACKGROUND  , XBPSYSCLR_INFOTEXT          }, ;
      { "Window                ", XBPSYSCLR_WINDOW          , XBPSYSCLR_WINDOWTEXT        }  ;
   }

RETURN self




METHOD PanelDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::drawingArea:setColorBG( GRA_CLR_PALEGRAY )
   ::ListBox:create()
   ::Static1:create()
   ::Static2:create()
   ::Static3:create()

   AEval( ::aColors, {|a| ::ListBox:additem( a[1] ) } )

   ::listBox:itemMarked := {|| ::setColor() }
   ::ListBox:setData( {1}, .T. )

   aPos    := ::currentPos()
   aPos[1] += ::currentSize()[1]+24
   ::colorDialog := ColorDialog():new( AppDesktop(), self, aPos ):create()
   ::colorDialog:setColor( ::aColors[1,2], ::aColors[1,3] )
 
   SetAppFocus( ::ListBox )
RETURN self




METHOD PanelDialog:setColor
   LOCAL aItem := ::ListBox:getData()

   IF Empty( aItem )
      RETURN self
   ENDIF

   ::colorDialog:setColor( ::aColors[ aItem[1] ,2], ::aColors[ aItem[1], 3] )
RETURN Self



/*
 * Dialog displays XBPSYSCLR_ #define constants
 */
CLASS ColorDialog FROM XbpDialog
   EXPORTED:
   VAR aColors
   VAR Static1
   VAR Static2
   VAR Static3
   VAR Static4

   METHOD init
   METHOD create
   METHOD setColor
   METHOD getColorDefine
ENDCLASS




METHOD ColorDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {509,512}, ;
           aSize    TO {280,308}, ;
           lVisible TO .T.

   DEFAULT aPP TO {}

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList      := .T.
   ::XbpDialog:sysMenu       := .F.
   ::XbpDialog:moveWithOwner := .T.
   ::XbpDialog:title         := "#define XBPSYSCLR_*"

   ::Static1         := XbpStatic():new( ::drawingArea, , {24,228}, {156,24} )
   ::Static1:caption := "Foreground color"
   ::Static1:options := XBPSTATIC_TEXT_VCENTER

   ::Static2         := XbpStatic():new( ::drawingArea, , {24,156}, {228,60} )
   ::Static2:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::Static3         := XbpStatic():new( ::drawingArea, , {24,120}, {156,24} )
   ::Static3:caption := "Background color"
   ::Static3:options := XBPSTATIC_TEXT_VCENTER

   ::Static4         := XbpStatic():new( ::drawingArea, , {24,36}, {228,60} )
   ::Static4:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER

   ::aColors := { ;
      { "XBPSYSCLR_3DFACE           ", XBPSYSCLR_3DFACE            }, ;
      { "XBPSYSCLR_ACTIVEBORDER     ", XBPSYSCLR_ACTIVEBORDER      }, ;
      { "XBPSYSCLR_ACTIVETITLE      ", XBPSYSCLR_ACTIVETITLE       }, ;
      { "XBPSYSCLR_ACTIVETITLETEXT  ", XBPSYSCLR_ACTIVETITLETEXT   }, ;
      { "XBPSYSCLR_APPWORKSPACE     ", XBPSYSCLR_APPWORKSPACE      }, ;
      { "XBPSYSCLR_BACKGROUND       ", XBPSYSCLR_BACKGROUND        }, ;
      { "XBPSYSCLR_BUTTONTEXT       ", XBPSYSCLR_BUTTONTEXT        }, ;
      { "XBPSYSCLR_HILITEBACKGROUND ", XBPSYSCLR_HILITEBACKGROUND  }, ;
      { "XBPSYSCLR_HILITEFOREGROUND ", XBPSYSCLR_HILITEFOREGROUND  }, ;
      { "XBPSYSCLR_INACTIVEBORDER   ", XBPSYSCLR_INACTIVEBORDER    }, ;
      { "XBPSYSCLR_INACTIVETITLE    ", XBPSYSCLR_INACTIVETITLE     }, ;
      { "XBPSYSCLR_INACTIVETITLETEXT", XBPSYSCLR_INACTIVETITLETEXT }, ;
      { "XBPSYSCLR_INFOBACKGROUND   ", XBPSYSCLR_INFOBACKGROUND    }, ;
      { "XBPSYSCLR_INFOTEXT         ", XBPSYSCLR_INFOTEXT          }, ;
      { "XBPSYSCLR_MENU             ", XBPSYSCLR_MENU              }, ;
      { "XBPSYSCLR_MENUTEXT         ", XBPSYSCLR_MENUTEXT          }, ;
      { "XBPSYSCLR_WINDOW           ", XBPSYSCLR_WINDOW            }, ;
      { "XBPSYSCLR_WINDOWTEXT       ", XBPSYSCLR_WINDOWTEXT        }  ;
   }
RETURN self




METHOD ColorDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::drawingArea:setColorBG( GRA_CLR_PALEGRAY )
   ::Static1:create()
   ::Static2:create()
   ::Static3:create()
   ::Static4:create()
RETURN self




METHOD ColorDialog:setColor( nBackGround, nForeGround )
   LOCAL cBackground := ::getColorDefine( nBackGround )
   LOCAL cForeground := ::getColorDefine( nForeGround )

   IF cForeGround == NIL
      ::static2:setCaption( "not applicable" )
      ::static2:setColorFG( GRA_CLR_BLACK )
   ELSE
      ::static2:setCaption( cForeGround )
      ::static2:setColorFG( nForeGround )
   ENDIF

   IF cBackGround == NIL
      ::static4:setCaption( "not applicable" )
      ::static4:setColorBG( GRA_CLR_PALEGRAY )
   ELSE
      ::static4:setCaption( cBackGround )
      ::static4:setColorBG( nBackGround )
   ENDIF
RETURN self




METHOD ColorDialog:getColorDefine( nDefine )
   LOCAL i:= AScan( ::aColors, {|a| a[2] == nDefine } )
RETURN IIF( i==0, NIL, ::aColors[i,1] )
