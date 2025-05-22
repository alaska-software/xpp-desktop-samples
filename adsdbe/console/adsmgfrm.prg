//////////////////////////////////////////////////////////////////////
//
//  ADSMGFRM.PRG
//
//  Copyright:
//   Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//       Alaska Management Console for the Advantage Database Server
//   
//       This file contains the implementation of the main application dialog 
//       window.
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"
#include "AdsMg.ch"

// This class represents the application dialog.
CLASS ManageForm FROM XbpDialog
   EXPORTED:
      VAR aServers             // List of available database servers
      VAR SelectedServer       // The actual server
      VAR TreeView             // The "main menu"
      VAR Stage                // Area where information is displayed
      VAR Bitmap               // Welcome bitmap
      VAR ServerBox            // The combox that contains the list of servers
      VAR ScanButton           // Used to re-scan the network for available servers

      VAR oThread              // Thread that refreshes the displayed information
     

      METHOD init                 // Initialize dialog window
      METHOD create               // Requests resources for the dialog window
      METHOD ClearChildren        // Removes GUI elements from the "stage"
      METHOD AddTreeViewItems     // Adds the "menu entries" to the treeview
      METHOD KillThread           // Terminates the active thread
      METHOD ActivateItem         // Is executed on treeview-item selection
      METHOD SetActiveServer      // Sets the active server
      METHOD ScanServers          // Started on pushbutton activation
      METHOD CheckValidServers    // Scans network for available servers
      METHOD ServerType           // Returns the type of a database server
      METHOD AnimDlg              // Shows an animation when scanning the network
      METHOD Welcome              // Shows the welcome bitmap
      METHOD CenterPos            // Centers the position of the dialog
ENDCLASS



// Initialize main dialog window
METHOD ManageForm:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aSize    TO {800,600}, ;
           aPos     TO ::CenterPos( aSize, AppDesktop():currentSize() ), ;
           lVisible TO .F. , ;
           aPP      TO {}

   AAdd ( aPP, { XBP_PP_BGCLR, GRA_CLR_PALEGRAY } )
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Arial" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:drawingArea:ColorBG := GRA_CLR_PALEGRAY  
   ::XbpDialog:border := XBPDLG_RAISEDBORDERTHICK_FIXED
   ::XbpDialog:taskList := .T.
   ::XbpDialog:maxbutton := .F.
   ::XbpDialog:title := "Alaska ADS Management Console "+ADSMG_VERSION

   ::ServerBox := XbpCombobox():New(::drawingArea, , {8,356}, {140,200})
   ::Serverbox:clipsiblings := .T.
   ::ServerBox:itemSelected := {||::SetActiveServer() }

   ::ScanButton := XbpPushButton():New(::drawingArea, , {160,532}, {48,24} )
   ::ScanButton:caption := "Rescan"
   ::ScanButton:activate := {|| ::ScanServers()}


   // This treeview works as the main menu.
   ::TreeView := XbpTreeView():new( ::drawingArea, , {8,8}, {200,508} )
   ::TreeView:clipSiblings := .T.
   ::TreeView:hasButtons := .T.
   ::TreeView:hasLines := .T.
   ::TreeView:ItemMarked := {|oItem, aRect, uself| ::ActivateItem(oItem) }

   // On this "stage", all information is displayed.
   ::Stage := XbpStatic():New(::drawingArea,,{220,8}, {564,556} )
   ::Stage:type := XBPSTATIC_TYPE_GROUPBOX

   // This refers to the running thread, that automatically refreshs the
   // displayed data.
   ::oThread := nil
RETURN self


// REQUEST system resources for main dialog window
METHOD ManageForm:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   LOCAL cConnected, nI, nAmount

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::ServerBox:create()
   ::ScanButton:create()
   ::TreeView:create()
   ::Stage:create()

   // Show dialog window
   ::Show()

   // Show welcome bitmap
   ::Welcome()

   // Add the "menu items" to the treeview
   ::AddTreeViewItems()
RETURN self



// Destroys the children of an Xbase-part
// (Used to clear the "stage")
METHOD ManageForm:ClearChildren(oXbp)
   LOCAL nI
   LOCAL aChildren := oXbp:childlist() // Get the children
   LOCAL nAmount   := len(aChildren)

   // Destroy the children
   FOR nI := 1 TO nAmount
      aChildren[nI]:destroy()
   NEXT 

   // Repaint the groupbox.
   // (Groupboxes consist of a border only! So this must be done.)
   ::invalidaterect({220,8,800,600})
RETURN self



// Adds Items to the treeview; the treeview works as "menu".
// The codeblocks that shall be executed when a TreeViewItem is selected
// can be retrieved via TreeViewItem:getdata().
METHOD ManageForm:AddTreeViewItems()
   LOCAL oItem, oLastItem

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Installation Info"
   oItem:setdata({|| ShowServerInstallationInfo(::SelectedServer, ::Stage)})
   oItem:create()
   ::TreeView:rootitem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Memory Configuration Info"
   oItem:setdata({|| ShowServerMemConfigurationInfo(::SelectedServer, ::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Configuration Parameters Info"
   oItem:setdata({|| ShowServerConfigurationParamsInfo(::SelectedServer, ::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Activity Info"
   oItem:setdata({|| ShowActivityInfo(::SelectedServer, ::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)


   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Worker Thread Activity"
   oItem:setdata({|| ShowBrowser(ADSMG_THREADBROWSER, ::SelectedServer,::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Open Tables"
   oItem:setdata({||ShowBrowser(ADSMG_TABLEBROWSER,::SelectedServer,::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Communication Statistics"
   oItem:setdata({||ShowCommStats(::SelectedServer,::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)

   oLastItem:=XbpTreeViewItem():New()
   oLastItem:caption := "User Names"
   oLastItem:setdata({||ShowBrowser(ADSMG_USERBROWSER,::SelectedServer,::Stage)})
   oLastItem:create()

   ::TreeView:rootitem:additem(oLastItem)


   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Kill User"
   oItem:setdata({||KillUser(::SelectedServer,::Stage)})
   oItem:create()

   oLastItem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Open Indexes"
   oItem:setdata({||ShowBrowser(ADSMG_INDEXBROWSER,::SelectedServer,::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)

   oItem:=XbpTreeViewItem():New()
   oItem:caption := "Show locks and owners"
   oItem:setdata({||ShowBrowser(ADSMG_LOCKBROWSER,::SelectedServer,::Stage)})
   oItem:create()

   ::TreeView:rootitem:additem(oItem)
RETURN self


// Activates the TreeViewItem, if an ADS is available.
METHOD ManageForm:ActivateItem(oItem)
   IF ::SelectedServer == nil
      AlertBox(,"No Advantage Database Server available!", ;
               ,XBPSTATIC_SYSICON_ICONINFORMATION,"Operation impossible")
   ELSE 
      // Terminate the running thread.
      ::KillThread()

      // Create a new thread.
      ::oThread:=Eval(oItem:getdata())
   ENDIF 
RETURN self



// Kills the worker thread if available and removes GUI elements from
// the "stage"
METHOD ManageForm:KillThread
   // Check wether there is already a thread running.
   IF ::oThread != nil
      // Do not start it again if it finishes.
      ::oThread:setinterval( nil )

      // Wait for its termination.
      ::oThread:synchronize(0)

      // Support the garbage collector.
      ::oThread := nil
   ENDIF 

   // Clear the "stage".
   ::ClearChildren(::Stage)
RETURN self



// Sets the active server
METHOD ManageForm:SetActiveServer()
   LOCAL cConnected := "Alaska ADS Management Console "+ADSMG_VERSION
   LOCAL aItems := ::ServerBox:xbplistbox:getdata()
   LOCAL oTreeViewItem := ::TreeView:getdata()  
   LOCAL nServer := 0

   IF Len(aItems) > 0
      // There ARE available servers listed in the combobox.

      // Get the selected item of the listbox part of the combobox.
      ::SelectedServer := Left(::Serverbox:xbplistbox:getitem(aItems[1]),2)

      // Get the position of the selected server in the Server-Array.
      nServer := AScan(::aServers, {|a| a[1] == ::SelectedServer})

      IF nServer != 0
         // Created a new title for the dialog window.
         cConnected += " - Connected to "
         cConnected += ::aServers[nServer,1]+"  "
         cConnected += ::aServers[nServer,2]
      ENDIF 

      // Set the title of the dialog window.
      ::SetTitle( cConnected )

      IF oTreeViewItem != NIL
         // Refresh the data displayed on the "stage" after change of server.
         PostAppEvent(xbeTV_ItemMarked,oTreeViewItem,,::TreeView)

         // Re-activate the appropriate "menu"-item.
         SetAppFocus(::TreeView)
         ::TreeView:setdata(oTreeViewItem)
      ENDIF 
   ELSE
      // No servers in list. Show welcome bitmap and do a clean-up.
      ::SelectedServer := nil
      ::aServers := {}
      ::KillThread()
      ::Welcome()
   ENDIF 
RETURN self


// :AnimDlg() is for entertainment purposes only, its dialog is only 
// required to show an animation.
METHOD ManageForm:AnimDlg()
   LOCAL oBitmap
   LOCAL oDlg, oXbp, drawingArea
   LOCAL aSize := {178,174}
   LOCAL aPos  := ::CenterPos( aSize, AppDesktop():currentSize() )

   // Create animation dialog window.
   oDlg := XbpDialog():new( AppDeskTop(), SetAppWindow(), aPos, aSize, , .F.)
   oDlg:taskList := .F.
   oDlg:sysmenu := .F.
   oDlg:border := XBPDLG_RAISEDBORDERTHICK_FIXED
   oDlg:title := "Searching ADS"
   oDlg:create()
   oDlg:setmodalstate(XBP_DISP_APPMODAL)

   drawingArea := oDlg:drawingArea
   drawingArea:setColorBG( GRA_CLR_PALEGRAY   )
   drawingArea:setFontCompoundName( "8.Arial" )

   // oBitmap is used to display the animation bitmaps.
   oBitmap := BitmapRef():new( drawingArea, , {32,56}, {112,72} )
   oBitmap:clipSiblings := .T.
   oBitmap:create()

   // This static informs the user what the application is doing.
   oXbp := XbpStatic():new( drawingArea, , {8,8}, {160,32} )
   oXbp:caption := "Searching ADS.  Please wait..."
   oXbp:clipSiblings := .T.
   oXbp:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oXbp:create()

   // Show the dialog window.
   oDlg:show()
RETURN {oDlg, oBitmap}  // Return dialog and bitmap object



// Adds valid database servers to the combobox
METHOD ManageForm:ScanServers()
   LOCAL cConnected, nI, nAmount

   // Clear the listbox-part of the combobox.
   ::ServerBox:xbplistbox:clear()

   // Retrieve an array of available servers.
   ::aServers := ::CheckValidServers()
   IF Len(::aServers) == 0
      // Array is empty -> no server could be found.
      AlertBox(,"No Advantage Database Server could be found!", ;
               ,XBPSTATIC_SYSICON_ICONWARNING,"Warning")
      ::SelectedServer := nil
      ::KillThread()
      ::Welcome()
   ELSE 
      // Use the first server found as default selection.
      ::SelectedServer := ::aServers[1,1]
      AlertBox(,"First Advantage Database Server  was found on " ;
               +::SelectedServer+". ;"+ ;
               "This one will be used as default.",, ;
               XBPSTATIC_SYSICON_ICONINFORMATION,"Info")

      // Change the title of the dialog window.
      cConnected := "Alaska ADS Management Console "+ADSMG_VERSION+" - Connected to "
      cConnected += ::aServers[1,1]+"  "
      cConnected += ::aServers[1,2]

      ::settitle(cConnected)

      // Store the servers found in the listbox part of the combobox.
      nAmount := len(::aServers)
      FOR nI := 1 TO nAmount
         ::ServerBox:xbplistbox:additem(::aServers[nI,1]+"  "+::aServers[nI,2])
      NEXT 
      // Mark the first entry and post an appropriate event to fill the SLE part
      // of the combobox.
      ::ServerBox:xbplistbox:setdata({1})
      PostAppEvent(xbeLB_ItemSelected,,,::ServerBox)
   ENDIF 
RETURN self



// Checks the network for available ADS
METHOD ManageForm:CheckValidServers()
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL nHandle, nServerType, nRC
   LOCAL aServers := {}               // Empty server list.
   LOCAL nStart
   LOCAL cTest
   LOCAL oOldAppWindow := SetAppWindow()
   LOCAL aAnimXbps := ::AnimDlg()     // Show animation dialog
   LOCAL oProceed  := Signal():New()  // Used to ensure that the thread has started.
   LOCAL oAnimate := Animation():New({ID_WORLD1, ;  // Resources needed by the
                                      ID_WORLD2, ;  // animation.
                                      ID_WORLD3, ;
                                      ID_WORLD4, ;
                                      ID_WORLD5, ;
                                      ID_WORLD6, ;
                                      ID_WORLD7} )

   oAnimate:start(, aAnimXbps[2], 25, oProceed)   // Start animation thread
   oProceed:wait()                                // Wait for signal from thread

   SetAppWindow(aAnimXbps[1])
   SetAppFocus(aAnimXbps[1])
   FOR nStart := Asc("C") TO Asc("Z")             // Scan drives from C: to Z:
       cTest := Chr(nStart)+":"
 
       // Try to connect.
       nRC := AdsMgConnect(cTest,,,@nHandle)      

       IF nRC == AE_SUCCESS
          // There is an active ADS server on the current drive.
          // Get the type of the server.
          nRC := AdsMgGetServerType(nHandle, @nServerType)
          IF nRC != AE_SUCCESS
             // Operation was not successful. Set an invalid server type.
             nServerType := -1
          ENDIF 
             
          // Disconnect from the server. 
          nRC := AdsMgDisConnect(nHandle)
          IF nRC != AE_SUCCESS
             // Operation unsuccessful, show an error message.
             AdsError(nRC, ADSMG_DISCONNECT)
          ENDIF 

          // Add the found server and its type to the array that will be returned.
          AAdd(aServers, {cTest, ::ServerType(nServerType)})
       ENDIF 

       // Event-handling.
       nEvent := AppEvent( @mp1, @mp2, @oXbp, 10 )
       IF nEvent != xbe_None
          oXbp:handleEvent( nEvent, mp1, mp2 )
       ENDIF 
   NEXT 

   // Stop the animation.
   oAnimate:stop()

   // Destroy the dialog of the animation.
   aAnimXbps[1]:destroy()
   SetAppWindow(oOldAppWindow)
   SetAppFocus(oOldAppWindow)
RETURN aServers  // Return array of found servers.


// Return the type of an ADS
METHOD ManageForm:ServerType(nType)
   LOCAL cRet 

   // Determine the type of a server.
   DO CASE 
      CASE nType == ADS_MGMT_NETWARE_SERVER
           cRet := "Netware Server"
      CASE nType == ADS_MGMT_NT_SERVER
           cRet := "NT Server"
      CASE nType == ADS_MGMT_LOCAL_SERVER
           cRet := "Local Server"
      OTHERWISE
           cRet := "UNKNOWN SERVER"
   ENDCASE 
RETURN cRet // Return string with type of server.


// Show a welcome bitmap.
METHOD ManageForm:Welcome()
   ::Bitmap := BitmapRef():New(::Stage,::Stage,{20,70}, {522,424})
   ::Bitmap:create()
   ::Bitmap:load(ID_MG)
   ::Bitmap:display()
RETURN self



// Calculate a center position.
METHOD ManageForm:CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1] - aSize[1]) / 2 ) ;
       , Int( (aRefSize[2] - aSize[2]) / 2 ) }

















