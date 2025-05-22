//////////////////////////////////////////////////////////////////////
//
//  MAIN.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Example illustrating the integration of an ActiveX control.
//       This example shows the basic steps required to integrate
//       a control only via the interfaces provided by the Xbase++
//       automation support layer and the ActiveX container class.
//       This example is a good starting point for Xbase++ developers
//       that need to use ActiveX controls from external sources.
//   
//////////////////////////////////////////////////////////////////////


#include "XBP.CH"
#include "AppEvent.CH"

// Include the Automation Support Library
#PRAGMA LIBRARY( "ASCOM10.LIB" )

// Include the UI Extension Library (required for "XbpFileDialog")
#PRAGMA LIBRARY( "XPPUI2.LIB" )

#define CRLF   Chr(13) + Chr(10)

******************************************************************************
* Application's Main() Procedure
******************************************************************************
PROCEDURE Main()

 LOCAL oForm
 LOCAL oControl
 LOCAL nEvent
 LOCAL bOldError
 LOCAL oButton
 LOCAL mp1 := NIL, mp2 := NIL, oXbp := NIL

   /*
   ** Create application's main form. Note that
   ** the ":clipChildren" instance variable of
   ** the form's ":drawingArea" is set to .T.
   ** (TRUE) to prevent the dialog from drawing
   ** over the ActiveX control about to be
   ** created.
   */
   oForm := XbpDialog():new( AppDesktop() )
   oForm:taskList := .T.
   oForm:title    := "Acrobat Reader ActiveX Control Sample"
   oForm:drawingArea:clipChildren := .T.
   oForm:create( ,,{100,100}, {640,480},, .F. )

   // Assign callback codeblock for processing
   // quit requests by the user. In this example,
   // we'll use this to display a confirmation
   // dialog
   oForm:close  := {|| AppQuit()}

   // Assign callback codeblock for processing
   // resize operations on the main form. In this
   // example, the codeblock just re-aligns the
   // Acrobat Reader ActiveX control with the
   // form's borders.
   oForm:resize := {|aOld,aNew,oSelf| ResizeDlg(oSelf,aOld,aNew) }

   /*
   ** Create "Acrobat Reader" ActiveX control as a child
   ** of the main form. The ActiveX control instance is
   ** created using its generic id (called a "ProgId").
   ** The ProgId was extracted from the ActiveX control's
   ** documentation or a Type Library viewer application
   */
   oControl := XbpActiveXControl():new( oForm:drawingArea )

   // Specify the generic ProgId that identifies the
   // Acrobat Reader ActiveX control.
   oControl:CLSID  := "{CA8A9780-280D-11CF-A24D-444553540000}"

   // Create ActiveX control. BEGIN/END SEQUENCE is
   // used to handle eventual errors, eg. due to a
   // missing component.
   bOldError := ErrorBlock( {|e| Break(e)} )
   BEGIN SEQUENCE
   oControl:create(,, {10,60},{610,370} )
   RECOVER
      ErrorBlock( bOldError )

      MsgBox( "Error creating ActiveX control. Please make sure" + CRLF +;
              "Acrobat Reader is installed on your computer.",           ;
              "Acrobat Reader Sample" )
      QUIT
   END SEQUENCE
   ErrorBlock( bOldError )

   /*
   ** Configure ActiveX control to tailor its appearance
   ** and behaviour to our specific needs. The following
   ** section is control-specific and will vary with the
   ** component used. The properties and configuration
   ** options a control supports can be found in the
   ** documentation that accompany the ActiveX control.
   ** Note that methods of the component can be called
   ** just like any other method in Xbase++, and that
   ** properties can be accessed just like a normal
   ** instance variable.
   **
   ** The Adobe Acrobat Reader control doesn't have
   ** many options to configure. In fact, the only thing
   ** we'll change here is to make sure the control
   ** displays its toolbar and scrollbars, if required.
   */
   // Have control show its toolbar
   oControl:SetShowToolBar( .T. )

   // Einschalten der Anzeige der Scrollbars des ActiveX
   // Controls
   oControl:SetShowScrollbars( .T. )

   // Set the form as the default window and activate it
   oForm:Show()
   SetAppWindow( oForm )
   SetAppFocus( oForm )

   /*
   ** Add a push button for loading a PDF document
   ** into the ActiveX control
   */
   oButton := XbpPushButton():new( oForm:drawingArea )
   oButton:caption  := "Load PDF document..."
   oButton:activate := {|| LoadPDF(oControl) }
   oButton:create( ,, {240,10}, {140,30} )

   /*
   ** Application Event Loop
   ** This loop retrieves messages from the application's
   ** event queue and dispatches them to the individual
   ** controls. Every thread of the application that creates
   ** Xbase PARTs and/or ActiveX controls has to have
   ** such an event loop.
   */
   nEvent := xbeP_None
   DO WHILE nEvent != xbeP_Quit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

RETURN


******************************************************************************
* Procedure AppSys()
******************************************************************************
PROCEDURE AppSys()
  // This overloaded procedure prevents creation of the XbpCrt
  // window normally created during application startup
RETURN


******************************************************************************
* AppQuit() - Process quit request from the user
******************************************************************************
PROCEDURE AppQuit()

  IF ConfirmBox(SetAppWindow(),                   ;
                "Are you sure you want to quit?", ;
                "Xbase++ Sample Application" ) == XBPMB_RET_OK
     PostAppEvent( xbeP_Quit )
  ENDIF
RETURN


******************************************************************************
* ResizeDlg() - Process resize operations on the form passed in "oDlg".
*               Parameters "aOld" and "aNew" specify the previous and new
*               dimensions of the dialog, respectively. This function assumes
*               that the dialog passed has a push button at the bottom and
*               an ActiveX control above it. The procedure centers the push
*               button horizontally and align's the ActiveX control's right
*               and top borders with the corresponding borders of the dialog.
******************************************************************************
PROCEDURE ResizeDlg( oDlg, aOld, aNew )

 LOCAL nXDiff := aNew[1] - aOld[1]
 LOCAL nYDiff := aNew[2] - aOld[2]
 LOCAL aSize
 LOCAL aPos
 LOCAL aChilds
 LOCAL oChild
 LOCAL i

   aChilds := oDlg:drawingArea:childList()
   FOR i:=1 TO Len(aChilds)
      oChild := aChilds[i]

      IF oChild:isDerivedFrom("XbpPushbutton") == .T.
         aPos := oChild:currentPos()
         oChild:setPos( {aPos[1]+Int(nXDiff /2),aPos[2]} )
      ELSEIF oChild:isDerivedFrom("XbpActiveXControl") == .T.
         aSize := oChild:currentSize()
         oChild:setSize( {aSize[1]+nXDiff,aSize[2]+nYDiff} )
      ENDIF
   NEXT

RETURN


******************************************************************************
* LoadPDF() - Load a PDF document into the Acrobat Reader ActiveX control
*             passed in "oPDF". The procedure uses interfaces exposed by the
*             control to load the document. An "XbpFileDialog" object is
*             used to prompt the user for the file name of the document to
*             load.
******************************************************************************
PROCEDURE LoadPDF( oPDF )

 LOCAL oDlg
 LOCAL cFileName

   oDlg := XbpFileDialog():new( AppDesktop() )
   oDlg:center       := .T.
   oDlg:defExtension := "PDF"
   oDlg:fileFilters  := { {"PDF Documents","*.PDF"}, {"All Files","*.*"} }
   oDlg:restoreDir   := .T.
   oDlg:title        := "Load a PDF document..."
   oDlg:create()

   cFileName := oDlg:open( ,,, .F. )

   IF Empty(cFileName) == .T.
      RETURN
   ENDIF

   oPDF:loadFile( cFileName )

RETURN

