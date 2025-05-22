//////////////////////////////////////////////////////////////////////
//
//  SCMANAGE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Shortcut manager base class for dialogs.
//      To use shortcut keys in your own dialog, you have to derive the 
//      dialog from SCManager and call :InitShortCuts() right after calling
//      the dialogs :Create() method. In the keyboard callback the method
//      :HandleShortCut() needs to be called in order to handle the key.
//      The :InitShortCuts() method only uses the captions of the objects
//      to construct the shortcuts.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////



#include "common.ch"
#include "appevent.ch"
#include "xbp.ch"

******************************************************************************
* Declaration of the shortcut manager base class
******************************************************************************
CLASS SCManager

   HIDDEN:
      VAR    aShortCuts                      // Shortcuts = { nKey, oXbp, bBlock }
      METHOD CreateShortCut                  // service method

   EXPORTED:
      METHOD InitShortCuts                   // initialize the ALT key combinations
      METHOD HandleShortcut                  // handle the keystrokes for the dialog
      METHOD AddShortCut                     // add a shortcut
      METHOD GetShortCut                     // inquire shortcut to key

ENDCLASS


******************************************************************************
* Call shortcut, this is called within the :keyboard method.
******************************************************************************
METHOD SCManager:HandleShortcut( nKeyCode)
   LOCAL lHandled := FALSE, nPos

   // Is the key assigned to an action?
   nPos := 1
   WHILE lHandled == FALSE .AND. nPos <= Len( ::aShortCuts )

      // Call codeblock when the object is visible.
      IF ::aShortCuts[nPos, 1] == nKeyCode .AND. ;
         ::aShortCuts[nPos, 2]:IsVisible() .AND. ;
         ::aShortCuts[nPos, 2]:IsEnabled()

         Eval( ::aShortCuts[nPos, 3], ::aShortCuts[nPos, 2] )
         lHandled := TRUE
      ENDIF
      nPos++

   ENDDO
RETURN lHandled


******************************************************************************
* Init the shortcuts for a dialog.
******************************************************************************
METHOD SCManager:InitShortCuts( oXbp )
   LOCAL i, aChilds := oXbp:childList(), lChilds := LEN( aChilds)
   LOCAL nShortCut

   DEFAULT ::aShortCuts TO { ;
      { xbeK_ESC,    self, {|| PostAppEvent( xbeP_Close,,, self) } },;
      { xbeK_RETURN, self, {|| PostAppEvent( xbeP_Close,,, self) } } }

   FOR i := 1 TO lChilds

      // Static text
      IF aChilds[i]:isDerivedFrom( XbpStatic() )

         IF aChilds[i]:Type == XBPSTATIC_TYPE_TEXT
            // Is a shortcut Key assigned?
            IF !Empty( nShortCut := ::CreateShortCut( aChilds[i] ) )
               // Add the shortcut to set focus to the _next_ control to the array
               IF i < lChilds
                  ::AddShortCut( nShortCut, aChilds[i+1], { |o| SetAppFocus( o ) } )
               ENDIF
            ENDIF
         ENDIF

      // Pushbutton
      ELSEIF aChilds[i]:isDerivedFrom( XbpPushbutton() )

         // Is a shortcut Key assigned?
         IF !Empty( nShortCut := ::CreateShortCut( aChilds[i] ) )
            ::AddShortCut( nShortCut, aChilds[i], { |o| SetAppFocus( o ), ;
                           PostAppEvent( xbeP_Activate,,,o ) } )
         ENDIF

      // Radiobutton
      ELSEIF aChilds[i]:isDerivedFrom( XbpRadiobutton() )

         // Is a shortcut Key assigned?
         IF !Empty( nShortCut := ::CreateShortCut( aChilds[i] ) )
            ::AddShortCut( nShortCut, aChilds[i], { |o| SetAppFocus( o ),   ;
                           EnableRadioButton( o ), ;
                           PostAppEvent( xbeP_Activate,,,o ) } )
         ENDIF

      // Checkbox
      ELSEIF aChilds[i]:isDerivedFrom( XbpCheckbox() )

         // Is a shortcut Key assigned?
         IF !Empty( nShortCut := ::CreateShortCut( aChilds[i] ) )
            ::AddShortCut( nShortCut, aChilds[i], { |o| SetAppFocus( o ),   ;
                           o:SetData( !o:GetData() ), ;
                           PostAppEvent( xbeP_Activate,,,o ) } )
         ENDIF

      // 3State
      ELSEIF aChilds[i]:isDerivedFrom( Xbp3State() )

         // Is a shortcut Key assigned?
         IF !Empty( nShortCut := ::CreateShortCut( aChilds[i] ) )
            ::AddShortCut( nShortCut, aChilds[i], { |o| SetAppFocus( o ), ;
                           o:SetData( IF ( o:GetData() + 1 > 2, 0, o:GetData() + 1 ) ), ;
                           PostAppEvent( xbeP_Activate,,,o ) } )
         ENDIF

      // TabPage
      ELSEIF aChilds[i]:isDerivedFrom( XbpTabPage() )

         // Is a shortcut Key assigned?
         IF !Empty( nShortCut := ::CreateShortCut( aChilds[i] ) )
            ::AddShortCut( nShortCut, aChilds[i], { |o| SetAppFocus( o ), ;
                           PostAppEvent( xbeTab_TabActivate,,,o ) } )
         ENDIF
      ENDIF

      // Call the method recursive when the childlist is not empty
      IF ! EMPTY( aChilds[ i]:childList())
         ::InitShortCuts( aChilds[i])
      ENDIF
   NEXT
RETURN self


******************************************************************************
* Save shortcut
******************************************************************************
METHOD SCManager:AddShortCut( nKey, oXbp, bBlock )

   Aadd( ::aShortCuts, { nKey, oXbp, bBlock } )

RETURN TRUE


******************************************************************************
* Inquire Shortcut
******************************************************************************
METHOD SCManager:GetShortCut( nKey )
   LOCAL bRet, nPos := AScan( ::aShortCuts, { |a| a[1] == nKey } )

   IF nPos != 0
      RETURN ::aShortCuts[nPos,2]
   ENDIF

RETURN NIL


******************************************************************************
* Create shortcut key in the form <ALT>-<character>
******************************************************************************
METHOD SCManager:CreateShortCut( o)
   LOCAL nRet, nPos
   IF VALTYPE( o:caption) == "C"
      IF ( nPos := AT( "~", o:caption)) != 0
         nRet := xbeK_ALT_A +       ;
               ASC( UPPER( SUBSTR( o:caption, nPos + 1, 1))) - 65
      ENDIF
   ENDIF
RETURN nRet


******************************************************************************
* Utility function to activate radio button
******************************************************************************
STATIC PROCEDURE EnableRadioButton( oXbp )

   LOCAL aSiblings := oXbp:SetParent():ChildList(), nPos

   nPos := AScan ( aSiblings, { |o| o:IsDerivedFrom( XbpRadiobutton() ) .AND. ;
                                    o:GetData() == TRUE } )
   IF nPos > 0
      aSiblings[nPos]:SetData( FALSE )
   ENDIF
   oXbp:SetData( TRUE )

RETURN

// EOF
