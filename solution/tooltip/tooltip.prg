//////////////////////////////////////////////////////////////////////
//
//  TOOLTIP.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Tooltip help system
//   
//      This example shows how tooltips can be implemented in an Xbase++
//      application. For this, the example introduces a user-defined class
//      "MagicHelp" that is used for tracking mouse pointer movement and
//      user interaction. The text that is to be displayed in a tooltip
//      window is maintained in a database. Objects of class "MagicHelpLabel"
//      are used to retrieve the text for each Xbase Part. Alternatively,
//      the tooltip text can be assigned to the ":toolTipText" instance
//      variable.
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"

PROCEDURE Main
   LOCAL nEvent, mp1:=0, mp2:=0
   LOCAL oDlg, oXbp, drawingArea, oXbp1
   LOCAL oHelp

   SET DEFAULT TO "..\..\data\misc"

   oHelp := MagicHelp():New()

   oDlg := XbpDialog():new( AppDesktop() , , {172,80}, {487,315}, , .F.)
   oDlg:taskList := .T.
   oDlg:title := "SampleDialog"
   oDlg:visible := .F.
   oDlg:maxSize := oDlg:currentSize()
   oDlg:create()
   oDlg:helpLink := MagicHelpLabel():New(1)

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   oXbp1 := XbpStatic():new( drawingArea, , {12,12}, {456,60} )
   oXbp1:clipSiblings := .T.
   oXbp1:type := XBPSTATIC_TYPE_RAISEDBOX
   oXbp1:create()
   oXbp1:helpLink := MagicHelpLabel():New(2)

   oXbp := XbpPushButton():new( oXbp1, , {12,12}, {96,24} )
   oXbp:caption := "Pushbutton"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| NIL }
   oXbp:helpLink := MagicHelpLabel():New(10)

   oXbp := XbpPushButton():new( oXbp1, , {120,12}, {96,24} )
   oXbp:caption := "Pushbutton"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| NIL }
   oXbp:helpLink := MagicHelpLabel():New(11)

   oXbp := XbpPushButton():new( oXbp1, , {240,12}, {96,24} )
   oXbp:caption := "Pushbutton"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| NIL }
   oXbp:helpLink := MagicHelpLabel():New(12)

   oXbp := XbpPushButton():new( oXbp1, , {348,12}, {96,24} )
   oXbp:caption := "Pushbutton"
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:activate := {|| NIL }
   oXbp:helpLink := MagicHelpLabel():New(13)


   oXbp := XbpStatic():new( drawingArea, , {12,192}, {84,24} )
   oXbp:caption := "Firstname"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,192}, {372,24} )
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:helpLink := MagicHelpLabel():New(6)


   oXbp := XbpStatic():new( drawingArea, , {12,156}, {84,24} )
   oXbp:caption := "Lastname"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,156}, {372,24} )
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:helpLink := MagicHelpLabel():New(7)


   oXbp := XbpStatic():new( drawingArea, , {12,120}, {84,24} )
   oXbp:caption := "Address1"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,120}, {372,24} )
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:helpLink := MagicHelpLabel():New(8)


   oXbp := XbpStatic():new( drawingArea, , {12,84}, {84,24} )
   oXbp:caption := "Address2"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:create()

   oXbp := XbpSLE():new( drawingArea, , {96,84}, {372,24} )
   oXbp:clipSiblings := .T.
   oXbp:create()
   oXbp:helpLink := MagicHelpLabel():New(9)


   oXbp := XbpStatic():new( drawingArea, , {12,228}, {456,48} )
   oXbp:caption     := "MagicHelp Sample"
   oXbp:toolTipText := "Tooltip text for static text object"
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER
   oXbp:setColorFG(GRA_CLR_WHITE)
   oXbp:setColorBG(GRA_CLR_BLACK)
   oXbp:setFontCompoundName(FONT_HELV_LARGE+FONT_STYLE_BOLD)
   oXbp:create()

   oDlg:show()
   SetAppWindow(oDlg); SetAppFocus( oDlg)
   oHelp:start()

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN

PROCEDURE APPSYS()
RETURN

PROCEDURE DBESYS()
   LOCAL lSuccess:= .T.

   IF !DbeLoad( "CDXDBE",.T.)
      lSuccess := .F.
   ENDIF

   IF ! DbeLoad( "FOXDBE",.T.)
      lSuccess := .F.
   ENDIF

   IF ! DbeBuild( "FOXCDX", "FOXDBE", "CDXDBE" )
      lSuccess := .F.
   ENDIF

   IF ! lSuccess
      Alert( "FOXCDX Database-Engine could not be built" , {"OK"} )
   ENDIF
RETURN


CLASS MagicHelp FROM Thread

   HIDDEN:
   METHOD DisplayToolTip()

   EXPORTED:
   VAR producerID
   VAR oLastMotionXBP
   VAR oLastTipXBP
   VAR nLastTipTime
   VAR aLastMotionPos
   VAR oTip
   VAR lTipIsShown
   VAR nTipSensitivity

   INLINE METHOD init()
     ::nTipSensitivity := 1
     ::thread:init()
     ::producerID := ThreadID()
     ::nLastTipTime := 0
     ::name := "Tooltip service"
   RETURN

   EXPORTED:
   METHOD execute()
   METHOD atStart()
   METHOD atEnd()
   METHOD showTip()
   METHOD hideTip()
   METHOD PaintTheTip()
ENDCLASS

METHOD MagicHelp:atStart()
   ::lTipIsShown       := .F.
   ::oLastMotionXBP    := NIL
   ::aLastMotionPos    := NIL
  /*
   * The HELP Database and Index are located in the ..\..\data\misc\
   * directory. The Index expression is as followed:
   * INDEX ON StrZero(lang_id,4,0)+StrZero(help_id,4,0) TAG ID TO MHELP
   */
   USE MHELP.DBF INDEX MHELP.CDX SHARED
RETURN


METHOD MagicHelp:atEnd()
   DbCloseAll()
RETURN


METHOD MagicHelp:Execute()

   LOCAL nEvent, mp1:=0, mp2:=0, oXbp:=NIL
   LOCAL nLastMotionTime := 0
 LOCAL lInputLock := .F.
 LOCAL aPos
 LOCAL oTmp

     /*
    * Tool tip thread loop. The loop is used
    * to examine the various preconditions
    * that must be met for tool tips to be
    * shown. All decisions are based on the
    * the current position of the mouse 
    * pointer. The algorithm is as follows:
    * o If the mouse pointer is placed over
    *   an Xbase Part and remains stationary
    *   for a preset time period, the tool
    *   tip is shown.
    * o Quickly moving the pointer over a
    *   sibling object causes the tool tip
    *   of the sibling object to be activated
    *   instead ("browse tool tips" mode).
    * o Moving the pointer over an Xbase 
    *   Part that is not a sibling object,
    *   or moving the pointer slowly over 
    *   another object hides the tool tip 
    *   currently active.
    * o Using the keyboard or clicking the 
    *   mouse installs a so-called "input
    *   lock". This hides a tool tip currently
    *   active. Furthermore, no tool tip 
    *   are being displayed as long as the
    *   mouse pointer remains over the 
    *   Xbase Part it was placed over when
    *   the input lock was activated.
      */
   DO WHILE .T.
      Sleep( 10 )

      // Process events for the tool tip
      // window, if any
      nEvent := AppEvent( @mp1, @mp2, @oXbp, 0.1 )
      IF nEvent != xbe_None
         oXbp:HandleEvent( nEvent, mp1, mp2 )
      ENDIF

      // Use LastAppEvent() to sniff into the 
      // event queue of the application thread.
      // Install the input lock if the user's
      // currently working with either the 
      // mouse or the keyboard
      nEvent := LastAppEvent(@mp1,@mp2,@oXbp,::producerID)
      IF oXbp == NIL 
         LOOP
      ENDIF

      IF nEvent == xbeP_Keyboard .OR. ;
         nEvent == xbeM_LbDown   .OR. ;
         nEvent == xbeM_MbDown   .OR. ;
         nEvent == xbeM_RbDown   .OR. ;
         nEvent == xbeM_LbUp     .OR. ;
         nEvent == xbeM_MbUp     .OR. ;
         nEvent == xbeM_RbUp     .OR. ;
         nEvent == xbeM_LbClick  .OR. ;
         nEvent == xbeM_MbClick  .OR. ;
         nEvent == xbeM_RbClick
         ::HideTip()
         ::oLastMotionXBP := NIL
         ::aLastMotionPos := NIL
         lInputLock := .T.
         LOOP
      ENDIF

      // Determine current mouse pointer 
      // position as well as the object
      // it currently resides over
      aPos := GetPointerPos()
      oXbp := XbpFromPoint( aPos )

      IF oXbp == NIL .OR. (oXbp == ::oLastTipXBP .AND. ;
         ::lTipIsShown == .T.)
         LOOP
      ENDIF

      // Ignore, if the pointer is placed
      // over the object with the focus
      // if an input lock is active
      oTmp := SetAppFocus()
      IF oTmp == oXbp .AND. lInputLock == .T.
         ::oLastMotionXBP := NIL
         ::aLastMotionPos := NIL
         LOOP
      ENDIF

      // If the pointer has moved over
      // another object, store the object
      // and the pointer position. Reset
      //  the tool tip timeout.
      IF oXbp != ::oLastMotionXBP
         ::oLastMotionXbp := oXbp
         ::aLastMotionPos := aPos
         nLastMotionTime  := Seconds()
         LOOP
      ENDIF

      // The pointer resides over the same
      // object as last time. Display the
      // tool tip if the timeout has elapsed.
      // Also reset the input lock.
      IF (Seconds() - nLastMotionTime) > ::nTipSensitivity 
         IF ::aLastMotionPos[1] == aPos[1] .AND. ;
            ::aLastMotionPos[2] == aPos[2] 
            ::oLastMotionXBP := oXbp
               ::showTip()
            lInputLock := .F.
            LOOP
         ENDIF
            ENDIF

      // Check whether the current object has
      // the same parent as the one the last
      // tool tip was displayed over. Activate
      // browse mode, if so.
      IF ValType(::oLastTipXBP) == "O" .AND. ;
         oXbp:setParent() == ::oLastTipXBP:setParent() .AND. ;
         oXbp != ::oLastTipXBP

         IF ::nLastTipTime>0 .AND. ;
            (Seconds()-::nLastTipTime)<=0.5
         ::hideTip()

         ::oLastMotionXBP  := oXbp
            ::aLastMotionPos := aPos
         nLastMotionTime := Seconds()

            ::showTip()
            lInputLock := .F.
            LOOP
         ENDIF
         ENDIF

     // None of the known conditions for 
     // displaying tool tips has been met. 
     // Make sure no tool tip is currently
     // visible.
     ::aLastMotionPos := aPos
          ::hideTip()

   ENDDO

RETURN

METHOD MagicHelp:showTip()
   IF(!::lTipIsShown)
      ::DisplayToolTip(::oLastMotionXBP)
      ::oTip:show()
      ::lTipIsShown  := .T.
      ::oLastTipXBP  := ::oLastMotionXBP
   ENDIF
RETURN

METHOD MagicHelp:hideTip()
   IF(::lTipIsShown)
      ::oTip:hide()
      ::oTip:destroy()
      ::lTipIsShown := .F.
      ::nLastTipTime := Seconds()
   ENDIF
RETURN

METHOD MagicHelp:DisplayToolTip(oXbpRequestingHint)
   LOCAL cText := "(nil)"
   LOCAL cID   := ""
   LOCAL aPos
   LOCAL oPS
   LOCAL aSize := Array(2)
   LOCAL aPoints
   LOCAL oFont

  /*
   * Calculate absolute position of motion event and adjust it
   * about the mouse pointer size
   */
   aPos  := AClone( ::aLastMotionPos )
   aPos[1] += 6
   aPos[2] -= 30

  /*
   * Check if the XBP for which we have to post a hint has an
   * associated Helplabel, and if so, retrieve the ID and
   * hint message from the database. If there is no HelpLabel
   * object, check whether a tooltip text is stored in the
   * XBP's :toolTipText intance variable.
   */
   IF(ValType(oXbpRequestingHint:helpLink)=="O" .AND. ;
      oXbpRequestingHint:helpLink:isDerivedFrom("MagicHelpLabel"))

      cID := oXbpRequestingHint:helpLink:getID()
      IF(DbSeek(cID,.F.))
         cText := " "+AllTrim(FIELD->HINT)+" "
      ENDIF
   ELSEIF IsMemberVar(oXbpRequestingHint, "toolTipText") == .T. .AND.;
          ValType(oXbpRequestingHint:toolTiptext) == "C" .AND. ;
          Len(oXbpRequestingHint:toolTiptext)     > 0
      cText := oXbpRequestingHint:toolTiptext
   ENDIF

  /*
   * Ok, now let's show the tip
   */
   ::oTip := XbpStatic():new()
   ::oTip:options := XBPSTATIC_TEXT_BOTTOM
   ::oTip:Visible := .F.
   ::oTip:Options := XBPSTATIC_TEXT_CENTER + XBPSTATIC_TEXT_VCENTER
   ::oTip:Caption := cText 
   oFont := XbpFont():New():Create( XBPSYSFNT_STATUS )
   ::oTip:SetFont( oFont )
   IF IsThemeActive() == .T. .AND. .NOT. "XP" $ Os()
      ::oTip:StyleClass = "TOOLTIP"
   ENDIF
   ::oTip:DrawMode:= XBP_DRAW_OWNER
   ::oTip:Draw    := {|oPS,aInfo| ::PaintTheTip(oPS,aInfo)}
   ::oTip:Create( AppDesktop(), AppDesktop(), aPos, {0,0} )

   oPS := ::oTip:lockPS()
   GraSetFont( oPS, oFont )
   aPoints := GraQueryTextBox( oPS, cText )
   ::oTip:unlockPS()

   aSize[1] := (aPoints[3,1] - aPoints[1,1]) + 8
   aSize[2] := (aPoints[1,2] - aPoints[2,2]) + 4
   ::oTip:SetSize( aSize, .F. )
   ::oTip:Show()

RETURN(SELF)


METHOD MagicHelp:PaintTheTip( oPS, aInfo )
   LOCAL aAreaAttr, aStringAttr
   LOCAL aRect

   aRect := {aInfo[XBP_DRAWINFO_RECT][1],aInfo[XBP_DRAWINFO_RECT][2],;
             aInfo[XBP_DRAWINFO_RECT][3],aInfo[XBP_DRAWINFO_RECT][4]}

   aAreaAttr := Array( GRA_AA_COUNT )
   aAreaAttr [ GRA_AA_COLOR ] := XBPSYSCLR_INFOBACKGROUND
   GraSetAttrArea( oPS, aAreaAttr )

   IF IsThemeActive() == .T. .AND. .NOT. "XP" $ Os()
      GraBox( oPS, {aRect[1],aRect[2]}, {aRect[3],aRect[4]}, GRA_FILL)
      GraBackground( oPS, {aRect[1],aRect[2]}, {aRect[3],aRect[4]} )
   ELSE
      GraBox( oPS, {aRect[1],aRect[2]}, {aRect[3],aRect[4]}, GRA_OUTLINEFILL)
   ENDIF

   GraSetFont( oPS, ::oTip:SetFont() )
   aStringAttr := Array( GRA_AS_COUNT )
   aStringAttr [ GRA_AS_COLOR ] := XBPSYSCLR_INFOTEXT
   GraSetAttrString( oPS, aStringAttr )
   GraStringAt( oPS, {4,4}, ::oTip:Caption )
RETURN self


CLASS MagicHelpLabel FROM XbpHelpLabel
   CLASS VAR nLangID
   VAR nID

   INLINE CLASS METHOD initClass()
    ::nLangID := 1
   RETURN

   INLINE METHOD init(nID)
      ::nID := nID
   RETURN

   EXPORTED:
   INLINE CLASS METHOD setLanguage(nID)
      ::nLangID := nID
   RETURN

   INLINE METHOD getID()
   RETURN(StrZero(::nLangID,4)+StrZero(::nID,4))
ENDCLASS

