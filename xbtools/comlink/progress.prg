//////////////////////////////////////////////////////////////////////
//
//  PROGRESS.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Class to display a progress bar in a separate box.
//   
//////////////////////////////////////////////////////////////////////


#include "gra.ch"
#include "xbp.ch"
#include "common.ch"
#include "appevent.ch"

CLASS ProgressBox FROM XbpDialog
    EXPORTED:
        VAR Mess1Text               // Message Text Static 1
        VAR Mess2Text               // Message Text Static 2
        VAR ProgressIndicator       // Box that can be filled up
        VAR PercentText             // xxx % Text
        VAR nMaxValue               // Max. (100%) value
        VAR nCurValue               // Currrent value to be displayed
        VAR cText                   // Text variable

        METHOD init
        METHOD create
        METHOD reset
        METHOD showProgress
        METHOD paint
ENDCLASS

******************************************************************************
* Initialize dialog
******************************************************************************
METHOD ProgressBox:init(oParent, oOwner, aPos, aSize, aPP, lVisible)

   DEFAULT oParent  TO SetAppWindow():setParent(), ;
           aPos     TO {68,320}, ;
           aSize    TO {240,80}, ;
           lVisible TO .F.

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Mess1Text := XbpStatic():new( ::drawingArea, ::drawingArea, {5,46}, {310,18} )
   ::Mess2Text := XbpStatic():new( ::drawingArea, ::drawingArea, {5,28}, {310,18} )

   ::ProgressIndicator := XbpStatic():new( ::drawingArea, ::drawingArea, {6,6}, {180,16} )
   ::PercentText := XbpStatic():new( ::drawingArea, ::drawingArea, {192,6}, {72,18} )
RETURN self


******************************************************************************
* Request system resources.
******************************************************************************
METHOD ProgressBox:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:border    := XBPDLG_DLGBORDER
   ::XbpDialog:taskList  := .F.
   ::XbpDialog:titleBar  := .F.
   ::XbpDialog:sysMenu   := .F.
   ::XbpDialog:minButton := .F.
   ::XbpDialog:maxButton := .F.
   ::XbpDialog:title     := ""
   ::XbpDialog:visible   := .F.
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY     )
   ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )

   ::Mess1Text:caption   := ""
   ::Mess1Text:options   := XBPSTATIC_TEXT_VCENTER
   ::Mess1Text:create()

   ::Mess2Text:caption   := ""
   ::Mess2Text:options   := XBPSTATIC_TEXT_VCENTER
   ::Mess2Text:create()

   ::ProgressIndicator:caption := ""
   ::ProgressIndicator:type    := XBPSTATIC_TYPE_RECESSEDBOX
   ::ProgressIndicator:create()
   ::ProgressIndicator:setColorFG( GRA_CLR_WHITE )

   ::PercentText:caption := "   "
   ::PercentText:options := XBPSTATIC_TEXT_VCENTER
   ::PercentText:create()

   ::show()
RETURN self


******************************************************************************
* Since the box must be drawn everytime a paint message arrives, 
* we must overload the default paint method for this dialog.
******************************************************************************
METHOD ProgressBox:paint( aRect )
   LOCAL oMicroPS
   LOCAL aPos       := ::ProgressIndicator:currentPos()
   LOCAL nBoxLen    := ::ProgressIndicator:currentSize()[1]-8
   LOCAL nBoxHeight := ::ProgressIndicator:currentSize()[2]
   LOCAL nPercent
   LOCAL nLen    
   LOCAL aAttr

   IF ::nMaxValue == NIL
      RETURN
   ENDIF

   oMicroPS   := ::ProgressIndicator:lockPS()
   nPercent   := Min( ::nCurValue * 100 / ::nMaxValue, 100 )
   nLen       := Min( nBoxLen * nPercent / 100, nBoxLen )

   IF aRect != NIL                     // We call this method always with
       ::XbpDialog:paint(aRect)        // aRect == NIL. If <> NIL it must
   ENDIF                               // be delegated to XbpDialog!!!
   IF ::cText != NIL
       ::Mess2Text:setCaption(::cText)
   ENDIF

   ::PercentText:setCaption(AllTrim(Str(nPercent, 3, 0))+" %")

   // Farb-Attribute der Bereiche setzen.
   aAttr                := Array(GRA_AA_COUNT)
   aAttr[GRA_AA_COLOR]  := GRA_CLR_DARKRED
   aAttr[GRA_AA_SYMBOL] := GRA_SYM_SOLID
   GraSetAttrArea(oMicroPS, aAttr)

   // Draw the progress bar
   GraBox(oMicroPS, {aPos[1]-5, aPos[2]-4}, {aPos[1]+nLen, aPos[2]+nBoxHeight-8}, GRA_FILL)

   // release micro PS
   ::ProgressIndicator:unlockPS( oMicroPS )
RETURN


******************************************************************************
* Refresh progress box -> Set values and call paint method.
******************************************************************************
METHOD ProgressBox:showProgress(nMaxValue, nCurValue, cText)
   ::cText     := cText
   ::nMaxValue := nMaxValue
   ::nCurValue := nCurValue
   ::paint()
RETURN


******************************************************************************
* Reset progress box to initial state and force a repaint
******************************************************************************
METHOD ProgressBox:reset
   ::nCurValue := 0
   ::PercentText:setCaption("     ")
   ::hide()
   ::show()
RETURN


