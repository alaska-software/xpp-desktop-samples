//////////////////////////////////////////////////////////////////////
//
//  WEBBROWSER.PRG
//
//  Copyright:
//      Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#define CRLF   Chr(13) + Chr(10)

******************************************************************************
* Main procedure to test a form
******************************************************************************
PROCEDURE Main

 LOCAL oForm
 LOCAL nEvent
 LOCAL mp1 := NIL, mp2 := NIL, oXbp := NIL

   oForm := XbpWebBrowser():New()
   oForm:create( ,,{100,100} )

   oForm:close := {|| AppQuit()}

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
   CLOSE ALL
   QUIT
RETURN


******************************************************************************
* Sample class which implements a simple Web browser
******************************************************************************
CLASS XbpWebBrowser FROM _XbpWebBrowser

   EXPORTED:
      VAR    aHistory        READONLY

      VAR    cLastPost       READONLY
      VAR    cLastURL        READONLY

      VAR    lIsNavigating   READONLY
      VAR    HistoryViewer   READONLY

      METHOD Init
      METHOD create

      METHOD URLKeyboard
      METHOD DocumentComplete
      METHOD StatusBarChange
      METHOD BeforeNavigate

      METHOD DAResize

      METHOD PushHistory, PopHistory
      METHOD ClearHistory

      INLINE METHOD Keyboard( nKey )
         IF nKey == xbeK_ALT_H
            ::ViewHistory()
         ENDIF
         ::XbpDialog:Keyboard( nKey )
      RETURN SELF

      METHOD ViewHistory

ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD XbpWebBrowser:Init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT aPP TO {}

   * Execute method of the super class
   ::_XbpWebBrowser:Init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::aHistory := {}

   ::cLastPost := ""
   ::cLastURL  := {}
   ::lIsNavigating := .F.

   ::Title := "WebBrowser Sample (Press ALT-H to view history)"

   ::BackButton:Caption    := "Back"
   ::ForwardButton:Caption := "Forward"
   ::HomeButton:Caption    := "Home"
   ::StopButton:Caption    := "Stop"

   ::Close := { || AppQuit() }

   ::Drawingarea:Resize    := {|aOld,aNew| ::DAResize(aOld,aNew)}

RETURN SELF


******************************************************************************
* Request system resources
******************************************************************************
METHOD XbpWebBrowser:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

 LOCAL aSizeTmp := Array( 4 )
 LOCAL oPanel
 LOCAL bOldError
 LOCAL oError

   // Create browser dialog. BEGIN/END SEQUENCE is used to
   // handle eventual errors, eg. due to a missing component.
   bOldError := ErrorBlock( {|e| Break(e)} )
   BEGIN SEQUENCE
      ::_XbpWebBrowser:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   RECOVER USING oError
      ErrorBlock( bOldError )

      IF oError:subCode == 6500
         // Handle the error when the control is not installed

         MsgBox( "Error creating ActiveX Control. Please make sure MS" + CRLF +;
                 "Common Controls 6.0 is installed on your computer.",         ;
                 "WebBrowser Sample" )
         QUIT
      ELSE

         // Handle all other errors
         Break( oError )     
      ENDIF
   END SEQUENCE
   ErrorBlock( bOldError )

   // Adjust dialog dimensions in case dialog frame
   // borders have been reset on the client machine
   aSizeTmp[1] := 0
   aSizeTmp[2] := 0
   aSizeTmp[3] := ::BrowserControl:currentPos()[1] + ;
                  ::BrowserControl:currentSize()[1]
   aSizeTmp[4] := ::BackButton:currentPos()[2]  + ;
                  ::BackButton:currentSize()[2] + 10
   aSizeTmp := ::CalcFrameRect( aSizeTmp )
   ::setSize( {aSizeTmp[3],aSizeTmp[4]} )

   ::ForwardButton:Activate  := { || ::BrowserControl:Forward() }
   ::BackButton:Activate     := { || ::BrowserControl:Back() }
   ::HomeButton:Activate     := { || ::BrowserControl:Home() }
   ::StopButton:Activate     := { || ::BrowserControl:Stop() }
   ::URLCombo:Keyboard       := { |mp1| ::URLKeyboard( mp1 ) }
   ::URLCombo:ItemSelected   := { || ::BrowserControl:Navigate( ;
                                     ::URLCombo:XbpSLE:GetData() ) }

   ::BrowserControl:DocumentComplete  := {|oI,cURL| ::DocumentComplete(cURL) }
   ::BrowserControl:StatusTextChange  := {|| ::StatusbarChange() }
   ::BrowserControl:BeforeNavigate2   := {|oI,cURL,nF,cF,cP| ::BeforeNavigate(cURL,cP) }
   ::BrowserControl:ProgressChange    := {|| ::StatusbarChange() }

   ::Statusbar:GetItem(1):AutoSize    := XBPSTATUSBAR_AUTOSIZE_SPRING
   oPanel := ::Statusbar:AddItem( "",,, XBPSTATUSBAR_PANEL_TIME )
   oPanel:Alignment                   := XBPALIGN_HCENTER

   ::Statusbar:SetCaption( "" )
   ::MinSize := { 310, 200 }

   * Display the form
   ::show()
   SetappFocus( ::URLCombo )

   * Navigate to the default page
   ::BrowserControl:Navigate( "http://www.alaska-software.com" )

RETURN SELF


******************************************************************************
* Overloaded keyboard handler that recognizes the ENTER and RETURN keys
******************************************************************************
METHOD XbpWebBrowser:URLKeyboard( nKey )

   IF nKey == xbeK_ENTER .OR. nKey == xbeK_RETURN
      ::BrowserControl:Navigate( ::URLCombo:XbpSLE:GetData() )
   ENDIF

RETURN SELF


******************************************************************************
* Document has been loaded. Add URL to the combo box and history.
******************************************************************************
METHOD XbpWebBrowser:DocumentComplete( cURL )

   LOCAL lFound := .F., i
   LOCAL cMethod := "GET"
   LOCAL lAddToHistory := .F.

   IF ::BrowserControl:IsBusy() == FALSE
      ::lIsNavigating := .F.
      lAddToHistory := .T.
   ENDIF

   IF Empty( cURL )
      RETURN SELF
   ENDIF

   IF lAddToHistory
      FOR i := 1 TO ::URLCombo:NumItems()
         IF cURL == ::URLCombo:GetItem( i )
            lFound := .T.
         ENDIF
      NEXT
      IF lFound == .F.
         ::URLCombo:AddItem( cURL )
      ENDIF
      ::URLCombo:XbpSLE:SetData( cURL )

      IF !Empty( ::cLastPost )
         cMethod := "POST"
      ENDIF
   ENDIF
RETURN SELF


******************************************************************************
* Browser status change: update the text displayed in the status bar.
******************************************************************************
METHOD XbpWebBrowser:StatusBarChange()

 LOCAL cStatus := ::BrowserControl:StatusText 

   IF ::BrowserControl:Loading == .T.
      cStatus += " (" + LTrim(Str(::BrowserControl:PercentLoaded)) + "%)"
   ENDIF

   ::Statusbar:GetItem(1):Caption := cStatus

RETURN SELF


******************************************************************************
* Callback for BeforeNavigateEvent: Store new URL and update history
******************************************************************************
METHOD XbpWebBrowser:BeforeNavigate( cURL, cPost )

 LOCAL cMethod := "GET"

   IF ::lIsNavigating == .T.
      RETURN self
   ENDIF

   ::lIsNavigating := .T.
   ::cLastURL  := cURL

   IF !Empty( cPost )
      ::cLastPost := cPost
   ELSE
      ::cLastPost := ""
   ENDIF

   IF !Empty( ::cLastPost )
      cMethod := "POST"
   ENDIF
   ::PushHistory( ::cLastURL, cMethod, ::cLastPost )

RETURN SELF


******************************************************************************
* Dialog's drawing area has been resized. Resize and realign child controls
******************************************************************************
METHOD XbpWebBrowser:DAResize( aOld, aNew );

 LOCAL nXDiff := aNew[1] - aOld[1]
 LOCAL nYDiff := aNew[2] - aOld[2]
 LOCAL aSize
 LOCAL aPos

   aPos  := ::BackButton:currentPos()
   ::BackButton:setPos( {aPos[1],aPos[2] + nYDiff} )

   aPos  := ::ForwardButton:currentPos()
   ::ForwardButton:setPos( {aPos[1],aPos[2] + nYDiff} )

   aPos  := ::HomeButton:currentPos()
   ::HomeButton:setPos( {aPos[1],aPos[2] + nYDiff} )

   aPos  := ::StopButton:currentPos()
   ::StopButton:setPos( {aPos[1],aPos[2] + nYDiff} )

   aPos  := ::URLCombo:currentPos()
   aSize := ::URLCombo:currentSize()
   ::URLCombo:setPosAndSize( {aPos[1],aPos[2] + nYDiff}, ;
                             {aSize[1] + nXDiff,aSize[2]} )

   aPos  := ::URLCaption:currentPos()
   ::URLCaption:setPos( {aPos[1],aPos[2] + nYDiff} )

   aPos  := ::Separator:currentPos()
   aSize := ::Separator:currentSize()
   ::Separator:setPos( {aPos[1],aPos[2] + nYDiff} )
   ::Separator:setSize( {aSize[1] + nXDiff,aSize[2]} )

   aSize := ::BrowserControl:currentSize()
   ::BrowserControl:setSize( {aSize[1] + nXDiff,aSize[2] + nYDiff} )

   aPos  := ::Statusbar:currentPos()
   aSize := ::Statusbar:currentSize()
   ::StatusBar:setPosAndSize( aPos, {aSize[1] + nXDiff,aSize[2]} )

RETURN 


******************************************************************************
* Add an item to the history
******************************************************************************
METHOD XbpWebBrowser:PushHistory( cURL, cMethod, cPost )

   DEFAULT cURL    TO ""
   DEFAULT cMethod TO "GET"
   DEFAULT cPost   TO ""

   AAdd( ::aHistory, { cURL, cMethod, cPost } )
   IF !Empty( ::HistoryViewer )
      ::HistoryViewer:AddItem( { cURL, cMethod, cPost } )
   ENDIF

RETURN

******************************************************************************
* Pop an item from the history. The item is removed from the history window.
******************************************************************************
METHOD XbpWebBrowser:PopHistory()

 LOCAL aRet

   IF Len( ::aHistory ) > 0
      aRet := ::aHistory[Len( ::aHistory )]
      ADel( ::aHistory, Len( ::aHistory ) )
      ASize( ::aHistory, Len( ::aHistory ) - 1 )
      IF !Empty( ::HistoryViewer )
         ::HistoryViewer:DelItem( Len( ::aHistory ) + 1 )
      ENDIF
   ENDIF

RETURN aRet


******************************************************************************
* Clear the History
******************************************************************************
METHOD XbpWebBrowser:ClearHistory()

   ::aHistory := {}
   IF !Empty( ::HistoryViewer )
      ::HistoryViewer:clear()
   ENDIF

RETURN SELF


******************************************************************************
* Display the History dialog
******************************************************************************
METHOD XbpWebBrowser:ViewHistory()
   LOCAL oDlg     := XbpDialog():New()

   IF !Empty( ::HistoryViewer )
      SetAppFocus( ::HistoryViewer )
      RETURN SELF
   ENDIF

   oDlg:TitleBar := .F.
   oDlg:SysMenu  := .F.
   oDlg:Title := "History"
   oDlg:drawingArea:ClipChildren := .T.
   oDlg:Border := XBPDLG_RAISEDBORDERTHICK_FIXED
   oDlg:Create( ::SetParent(), self, ;
                {::CurrentPos()[1],::CurrentPos()[2] - 150},;
                {::CurrentSize()[1],150} )
   oDlg:Close := {||oDlg:Destroy(),::HistoryViewer := NIL}
   ::HistoryViewer := XbpHistoryViewer():New( oDlg:DrawingArea,,,;
                                              oDlg:DrawingArea:CurrentSize() )
   ::HistoryViewer:horizScroll := .T.
   ::HistoryViewer:Create()

   oDlg:drawingArea:Resize := { |aOld,aNew| ::HistoryViewer:SetSize( aNew ) }
   AEval( ::aHistory, { |a| ::HistoryViewer:AddItem(a) } )
   ::HistoryViewer:ItemSelected := { || ::BrowserControl:Navigate( ;
                                        ::aHistory[::HistoryViewer:GetData()[1],1] ) }
RETURN oDlg


******************************************************************************
* Modified listbox class for managing the browser history
******************************************************************************
CLASS XbpHistoryViewer FROM xbpListBox

 EXPORTED:
   VAR    aHistory READONLY

   INLINE METHOD Init( oParent, oOwner, aPos, aSize, aPP, lVisible )
      ::aHistory := {}
   RETURN ::XbpListbox:Init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   INLINE METHOD Clear()
      ::aHistory := {}
   RETURN ::XbpListbox:Clear()

   INLINE METHOD DelItem( nItem )
      ADel( ::aHistory, Len( ::aHistory ) )
      ASize( ::aHistory, Len( ::aHistory ) - 1 )
   RETURN ::XbpListbox:DelItem( nItem )

   INLINE METHOD AddITem( aItem )
      AAdd( ::aHistory, aItem )
      IF Upper(aItem[2]) == "GET"
         ::XbpListbox:AddItem( "GET : " + aItem[1] )
      ELSE
         ::Xbplistbox:AddItem( "POST: " + aItem[1] + " <DATA>: " + aItem[3] )
      ENDIF
   RETURN SELF

   INLINE METHOD GetHistory( nItem )
      IF ValType( nItem ) == "N"
         RETURN AClone( ::aHistory[nItem] )
      ENDIF
   RETURN AClone( ::aHistory )

ENDCLASS

//EOF
/////
