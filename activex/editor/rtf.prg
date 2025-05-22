//////////////////////////////////////////////////////////////////////
//
//  RTF.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//////////////////////////////////////////////////////////////////////


#include "Xbp.ch"
#include "Appevent.ch"
#include "DLL.CH"

#PRAGMA LIBRARY( "XPPUI2.LIB" )

#define CRLF   Chr(13) + Chr(10)

******************************************************************************
* Main procedure to test a form
******************************************************************************
PROCEDURE Main()

   LOCAL oForm
   LOCAL nEvent
   LOCAL mp1 := NIL, mp2 := NIL, oXbp := NIL

   oForm := RTFForm():new( SetAppWindow() )
   oForm:visible       := .F.
   oForm:minSize       := {400,200}
   oForm:create()
   oForm:close         := {|| AppQuit()}
   oForm:SetInputFocus := {|| SetAppFocus(oForm:oRTF)}

   CenterControl( oForm )

   oForm:show()
   SetAppFocus( oForm:oRTF )

   nEvent := xbe_None
   DO WHILE nEvent != xbeP_Quit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN

******************************************************************************
* Overloaded AppSys() procedure to prevent creation of default XbpCrt window
******************************************************************************
PROCEDURE AppSys
RETURN


******************************************************************************
* Quit the Application
******************************************************************************
PROCEDURE AppQuit()

  IF ConfirmBox(SetAppWindow(),                   ;
                "Are you sure you want to quit?", ;
                "Editor Sample Application" ) == XBPMB_RET_OK
     PostAppEvent( xbeP_Quit )
  ENDIF

RETURN


******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _RtfForm class.
******************************************************************************
CLASS RtfForm FROM _RtfForm
   EXPORTED:
      VAR cFileName
      METHOD Init
      METHOD Create
      METHOD GetFileName
      METHOD Refresh
      METHOD LoadFile
      METHOD Resize

      METHOD ToolbarResize
      METHOD ToolButtonClick
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD RtfForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_RtfForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Title := "Editor Sample"

   * Preparations of callback slots
   ::oFontName:ItemSelected := { || ::oRtf:SelFontName := Trim(::oFontName:XbpSLE:GetData()) }
   ::oFontSize:ItemSelected := { || ::oRtf:SelFontSize := Val(::oFontSize:XbpSLE:GetData()+".00") }
   ::Close                  := { || ::Destroy() }
   ::oToolbar:resize        := { |aOld,aNew| ::ToolbarResize(aOld,aNew) }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD RtfForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

 LOCAL aFonts, aSizes, aFontNames
 LOCAL oButton, oPanel
 LOCAL nTmp
 LOCAL bOldBlock
 LOCAL oError

   // Create editor dialog. BEGIN/END SEQUENCE is used to
   // handle eventual errors, eg. due to a missing component.
   bOldBlock := ErrorBlock( {|e| Break(e)} )
   BEGIN SEQUENCE
      ::_RtfForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   RECOVER USING oError
      ErrorBlock( bOldBlock )

      IF oError:subCode == 6500
         // Handle the error when the control is not installed

         MsgBox( "Error creating ActiveX Control. Please make sure MS" + CRLF +;
                 "Common Controls 6.0 is installed on your computer.",         ;
                 "Editor Sample" )
         QUIT
      ELSE

         // Handle all other errors
         Break( oError )     
      ENDIF
   END SEQUENCE
   ErrorBlock( bOldBlock )

   //
   // Add the toolbar buttons
   //
   ::oToolbar:LoadImageSet( XBPTOOLBAR_STDIMAGES_SMALL )
   ::oToolbar:ButtonClick := {|oButton| ::ToolButtonClick(oButton)}

   oButton       := ::oToolbar:AddItem()
   oButton:Image := STD_IMAGE_FILEOPEN
   oButton:Key   := "Open"
   oButton       := ::oToolbar:AddItem()
   oButton:Image := STD_IMAGE_FILESAVE
   oButton:Key   := "Save"
   oButton       := ::oToolbar:AddItem()
   oButton:Image := STD_IMAGE_PRINT
   oButton:Key   := "Print"

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_SEPARATOR

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_PLACEHOLDER

   ::oFontName:setPos( {oButton:Left, oButton:Top - oButton:Height} )

   oButton:width := ::oFontName:CurrentSize()[1]

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_SEPARATOR

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_PLACEHOLDER

   ::oFontSize:setPos( {oButton:Left, oButton:Top - oButton:Height} )

   oButton:width := ::oFontSize:CurrentSize()[1]

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_SEPARATOR

   oButton       := ::oToolbar:AddItem()
   oButton:Image := 100
   oButton:Key   := "Bold"
   oButton       := ::oToolbar:AddItem()
   oButton:Image := 101
   oButton:Key   := "Italic"
   oButton       := ::oToolbar:AddItem()
   oButton:Image := 102
   oButton:Key   := "Underline"

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_SEPARATOR

   oButton       := ::oToolbar:AddItem()
   oButton:Image := 103
   oButton:Style := XBPTOOLBAR_BUTTON_BUTTONGROUP
   oButton:Key   := "Left"
   oButton       := ::oToolbar:AddItem()
   oButton:Image := 104
   oButton:Style := XBPTOOLBAR_BUTTON_BUTTONGROUP
   oButton:Key   := "Center"
   oButton       := ::oToolbar:AddItem()
   oButton:Image := 105
   oButton:Style := XBPTOOLBAR_BUTTON_BUTTONGROUP
   oButton:Key   := "Right"

   oButton       := ::oToolbar:AddItem()
   oButton:Style := XBPTOOLBAR_BUTTON_SEPARATOR

   oButton       := ::oToolbar:AddItem()
   oButton:Image := 106
   oButton:Key   := "Bullet"

   //
   // Add the statusbar panels
   //
   oPanel          := ::oStatusbar:GetItem( 1 )
   oPanel:AutoSize := XBPSTATUSBAR_AUTOSIZE_SPRING

   oPanel          := ::oStatusbar:AddItem()
   oPanel:Style    := XBPSTATUSBAR_PANEL_CAPSLOCK
   oPanel:Alignment:= XBPALIGN_HCENTER
   oPanel:AutoSize := XBPSTATUSBAR_AUTOSIZE_CONTENTS
   oPanel:Width    -= 50

   oPanel         := ::oStatusbar:AddItem()
   oPanel:Style   := XBPSTATUSBAR_PANEL_NUMLOCK
   oPanel:Alignment:= XBPALIGN_HCENTER
   oPanel:AutoSize:= XBPSTATUSBAR_AUTOSIZE_CONTENTS
   oPanel:Width    -= 50

   oPanel         := ::oStatusbar:AddItem()
   oPanel:Style   := XBPSTATUSBAR_PANEL_INSERT
   oPanel:Alignment:= XBPALIGN_HCENTER
   oPanel:AutoSize:= XBPSTATUSBAR_AUTOSIZE_CONTENTS
   oPanel:Width    -= 50

   //
   // Fill the font list
   //
   aFonts := XbpFont():New(::LockPS()):List()
   ::UnlockPS()
   aFontNames := {}
   AEval( aFonts, { |o| IF(AScan(aFontNames,o:FamilyName)==0, ;
                           AAdd(aFontNames,o:FamilyName), NIL) } )
   ASort( aFontNames )
   AEval( aFontNames, { |c| ::oFontName:AddItem( c ) } )

   aSizes := { "6", "8", "10", "11", "12", "14", "16", "18", "20", ;
               "22", "24", "32", "36", "48", "52", "72" }
   AEval( aSizes, { |c| ::oFontSize:AddItem( c ) } )

   ::oRtf:SelChange := { || ::Refresh() }

   ::Refresh()

RETURN self

******************************************************************************
* Refresh the buttons on the form
******************************************************************************
METHOD RtfForm:Refresh()

 LOCAL xTmp
 LOCAL nCol
 LOCAL nLine

   //
   // Display the current line and column in the statusbar
   //
   nLine := ::oRTF:GetLineFromChar( ::oRTF:SelStart ) 
   nCol  := ::oRTF:SelStart - ::oRTF:GetLineStart( nLine )

   xTmp  := "Line: "  + LTrim( Str(nLine +1) ) + ;
            "   Column: " + LTrim( Str(nCol +1) )            

   ::oStatusbar:GetItem(1):Caption := xTmp

   //
   // Update the toolbar elements with respect to the current
   // selection in the edit control. Note that the value NIL
   // may be returned for conflicting properties. If both
   // italic and non-italic text had been selected, for
   // for instance, ":SelItalic" contains the value NIL.
   //
   xTmp := ::oRtf:SelFontName
   IF Empty(xTmp) == .T.
      ::oFontName:XbpSle:SetData( "" )
   ELSE
      ::oFontName:XbpSle:SetData( xTmp )
   ENDIF
   xTmp := ::oRtf:SelFontSize
   IF Empty(xTmp) == .T.
      ::oFontSize:XbpSle:SetData( "" )
   ELSE
      ::oFontSize:XbpSle:SetData( AllTrim( Str( Int(xTmp) ) ) )
   ENDIF

   xTmp := ::oRtf:SelBold
   IF xTmp == NIL
      ::oToolbar:GetItem("Bold"):MixedState := .T.
   ELSE
      ::oToolbar:GetItem("Bold"):MixedState := .F.
      ::oToolbar:GetItem("Bold"):Pressed    := xTmp
   ENDIF

   xTmp := ::oRtf:SelItalic
   IF xTmp == NIL
      ::oToolbar:GetItem("Italic"):MixedState := .T.
   ELSE
      ::oToolbar:GetItem("Italic"):MixedState := .F.
      ::oToolbar:GetItem("Italic"):Pressed    := xTmp
   ENDIF

   xTmp := ::oRtf:SelUnderline
   IF xTmp == NIL
      ::oToolbar:GetItem("Underline"):MixedState := .T.
   ELSE
      ::oToolbar:GetItem("Underline"):MixedState := .F.
      ::oToolbar:GetItem("Underline"):Pressed    := xTmp
   ENDIF

   xTmp := ::oRtf:SelAlignment
   IF xTmp == NIL
      ::oToolbar:GetItem("Left"):MixedState   := .T.
      ::oToolbar:GetItem("Right"):MixedState  := .T.
      ::oToolbar:GetItem("Center"):MixedState := .T.
   ELSE
      ::oToolbar:GetItem("Left"):MixedState   := .F.
      ::oToolbar:GetItem("Right"):MixedState  := .F.
      ::oToolbar:GetItem("Center"):MixedState := .F.
      IF xTmp == XBPRTF_ALIGN_LEFT
         ::oToolbar:GetItem("Left"):Pressed := .T.
      ELSEIF xTmp == XBPRTF_ALIGN_RIGHT
         ::oToolbar:GetItem("Right"):Pressed := .T.
      ELSEIF xTmp == XBPRTF_ALIGN_CENTER
         ::oToolbar:GetItem("Center"):Pressed := .T.
      ENDIF
   ENDIF

   xTmp := ::oRtf:SelBullet
   IF xTmp == NIL
      ::oToolbar:GetItem("Bullet"):MixedState := .T.
   ELSE
      ::oToolbar:GetItem("Bullet"):MixedState := .F.
      ::oToolbar:GetItem("Bullet"):Pressed    := xTmp
   ENDIF

RETURN self


******************************************************************************
* A tool button was clicked. Execute associated command.
******************************************************************************
METHOD RtfForm:ToolButtonClick( oButton )

   DO CASE
      CASE oButton:Key == "Open"
           ::oRtf:LoadFile(::GetFileName(.T.))
      CASE oButton:Key == "Save"
           ::oRtf:SaveFile(::GetFileName(.F.))
      CASE oButton:Key == "Print"
           ::oRtf:Print()
      CASE oButton:Key == "Bold"
           ::oRtf:SelBold := IF(Empty(::oRtf:SelBold),.T.,.F.)
      CASE oButton:Key == "Italic"
           ::oRtf:SelItalic := IF(Empty(::oRtf:SelItalic),.T.,.F.)
      CASE oButton:Key == "Underline"
           ::oRtf:SelUnderline := IF(Empty(::oRtf:SelUnderline),.T.,.F.)
      CASE oButton:Key == "Left"
           ::oRtf:SelAlignment := XBPRTF_ALIGN_LEFT
      CASE oButton:Key == "Center"
           ::oRtf:SelAlignment := XBPRTF_ALIGN_CENTER
      CASE oButton:Key == "Right"
           ::oRtf:SelAlignment := XBPRTF_ALIGN_RIGHT
      CASE oButton:Key == "Bullet"
          ::oRtf:SelBullet := IF(Empty(::oRtf:SelBullet),.T.,.F.)
   ENDCASE

   ::Refresh()

RETURN self


******************************************************************************
* Get a file name
******************************************************************************
METHOD RtfForm:GetFileName(lOpen)

   LOCAL oDlg := XbpFileDialog():New():Create(self)

   IF lOpen
      ::cFileName := oDlg:Open("*.rtf")
      IF ! File( ::cFileName )
        ::cFileName := ""
        MsgBox( "File does not exist", "Error" )
      ENDIF
   ELSE
      ::cFileName := oDlg:SaveAs(::cFileName)
   ENDIF
   oDlg:Destroy()
   IF Empty( ::cFileName )
      ::cFileName := ""
   ENDIF

RETURN ::cFileName


******************************************************************************
* Load a file
******************************************************************************
METHOD RtfForm:LoadFile(cFile)

   ::cFileName := cFile
   ::oRtf:LoadFile(::cFileName)

RETURN ::cFileName


******************************************************************************
* Dialog has been resized. Resize and realign child controls
******************************************************************************
METHOD RtfForm:Resize(aOldSize, aNewSize)

 LOCAL oButton
 LOCAL nTmp
 LOCAL nSLEY
 LOCAL cTmp
 LOCAL aSizeDA   := ::CalcClientRect( {0,0,aNewSize[1],aNewSize[2]} )
 LOCAL aSizeTBar := ::oToolbar:CurrentSize()
 LOCAL aSizeSBar := ::oStatusbar:CurrentSize()

   aSizeDA := {aSizeDA[3] - aSizeDA[1],aSizeDA[4] - aSizeDA[2]}

   ::oToolbar:SetSize( {aSizeDA[1], aSizeTBar[2]} )
   ::oToolbar:SetPos( {0, aSizeDA[2]-aSizeTBar[2]} )

   oButton := ::oToolbar:GetItem("Save")

   nSLEY   := ::oFontSize:CurrentSize()[2] - (::oFontSize:ListBoxSize()[2] -2)
   nTmp    := ::oToolbar:CurrentSize()[2]  - nSLEY
   nTmp    -= (::oFontSize:ListBoxSize()[2]- 2)
   nTmp    -= abs(oButton:Height - nSLEY) /2  

   cTmp := ::oFontName:XbpSle:GetData()
   ::oFontName:SetPos( {::oFontName:CurrentPos()[1],nTmp} )
   ::oFontName:XbpSle:SetData( cTmp )

   cTmp := ::oFontSize:XbpSle:GetData()
   ::oFontSize:setPos( {::oFontSize:CurrentPos()[1],nTmp} )
   ::oFontSize:XbpSle:SetData( cTmp )

   ::oStatusbar:SetSize( { aSizeDA[1], aSizeSBar[2] } )
   ::oRtf:SetSize( { aSizeDA[1], aSizeDA[2]-aSizeTBar[2]-aSizeSBar[2] } )

RETURN self


******************************************************************************
* Toolbar object was resized, possibly due to a button row wrapping around. 
* Realign all controls in the form.
******************************************************************************
METHOD RtfForm:ToolbarResize(aOldSize, aNewSize)

 LOCAL aSize

   IF aOldSize[2] != aNewSize[2]
      aSize := ::currentSize()
      ::Resize( aSize, aSize )
   ENDIF

RETURN self

//EOF
/////
