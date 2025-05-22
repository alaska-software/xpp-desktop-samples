//////////////////////////////////////////////////////////////////////
//
//  SCDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample code for shortcut manager class
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"


******************************************************************************
* Declaration of the form class, please note that this class must be
* derived from the SCManager class
******************************************************************************
CLASS NewForm FROM _NewForm, SCManager
   EXPORTED:
      METHOD init
      METHOD create
      METHOD keyboard, ActivateTab
ENDCLASS


******************************************************************************
* Initialize form
******************************************************************************
METHOD NewForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Init the form
   ::_NewForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::Title := "Test Form for ShortCut Manager"
   ::MaxButton := .F.
   ::Border    := XBPDLG_DLGBORDER

   * Init the tabpages
   ::TabPage2:Minimized   := .T.
   ::TabPage1:TabActivate := { || ::ActivateTab( ::TabPage1 ) }
   ::TabPage2:TabActivate := { || ::ActivateTab( ::TabPage2 ) }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD NewForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Create the form
   ::_NewForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Init Shortcuts
   ::InitShortCuts( self )

   * Show the form
   ::show()
   SetAppFocus( ::ButtonOK )

RETURN self


******************************************************************************
* The overloaded keyboard method has to call the method of the SCManager 
* base class to evaluate the shortcuts
******************************************************************************
METHOD NewForm:Keyboard( nKey )

   LOCAL lHandled := FALSE

   lHandled := ::HandleShortCut( nKey )
   IF ! lHandled
      RETURN ::xbpDialog:keyBoard( nKey )
   ENDIF

RETURN self

******************************************************************************
* Small service function to activate the tab pages
******************************************************************************
METHOD NewForm:ActivateTab( oTab )

   IF oTab:Minimized
      IF oTab == ::TabPage1
         ::TabPage2:Minimize()
         ::TabPage1:Maximize()
      ELSEIF oTab == ::TabPage2
         ::TabPage1:Minimize()
         ::TabPage2:Maximize()
      ENDIF
   ENDIF

RETURN self


******************************************************************************
* Main procedure
******************************************************************************
PROCEDURE Main

   LOCAL nEvent, oXbp, mp1, mp2

   NewForm():New():Create(,,{ 100, 100 })

   nEvent := xbe_None
   WHILE nEvent != xbeP_Close
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent ( nEvent, mp1, mp2 ) 
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO

RETURN


******************************************************************************
* Empty appsys
******************************************************************************
PROCEDURE AppSys
RETURN

//EOF
/////
