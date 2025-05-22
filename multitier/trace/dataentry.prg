//////////////////////////////////////////////////////////////////////
//
//  DATAENTRY.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   A dialog to set the port for incoming connections. The URL can be
//   copied to the Clipboard. Whether the default Web Browser shall be
//   started can be set with a checkbox.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#define HTMLSTARTPAGE "trace.htm"
#define PORT          81

//
// This class is derived from the implementation-level class of the
// form. Instance variables are declared in the _DataEntry class.
//
CLASS DataEntry FROM _DataEntry
   EXPORTED:
             METHOD init
             METHOD create
             METHOD displayUrl
             METHOD copyUrl
             METHOD getPort
             METHOD getUrl
      ACCESS METHOD getConfiguration VAR Configuration
ENDCLASS

//
// Initialize form
//
METHOD DataEntry:init()
   // Execute method of the super class with
   // identical parameters
   SUPER

   ::Title                 := "Server Configuration"
   ::_Computername:Caption := GetEnv("COMPUTERNAME")
RETURN self


//
// Request system resources
//
METHOD DataEntry:create()

   // Execute method of the super class with
   // identical parameters
   SUPER

   // Codeblock for the button that copies the
   // URL to the Clipboard
   ::_CopyUrl:Activate := {|| ::copyUrl() }

   // The port is visualized in a spin button.
   // Initialize the spin button and assign the
   // codeblocks that update the dialog when the
   // value changes
   ::_Port:setData( PORT )
   ::_Port:EndSpin        := {|| ::displayUrl() }
   ::_Port:KillInputFocus := {|| ::displayUrl() }

   ::displayUrl()

   // Display the form
   ::show()

RETURN self

//
// Display the URL in a static on the dialog
//
METHOD DataEntry:displayUrl()
  LOCAL cUrl
  cUrl := ::getUrl()
  ::_UrlDisplay:setCaption( cUrl )
RETURN self

//
// Copy the URL to the clipboard
//
METHOD DataEntry:copyUrl()
  LOCAL cUrl, oClipBoard
  cUrl := ::getUrl()

  oClipBoard := XbpClipBoard():new():create()
  oClipBoard:open()
  oClipBoard:clear()
  oClipBoard:setBuffer( cUrl )
  oClipBoard:close()
  oClipBoard:destroy()
RETURN self

//
// Return the URL
//
METHOD DataEntry:getPort()
RETURN ::_Port:getData()

//
// Compose the URL from the computername,
// the port and the RESTful path that
// accesses the start page
//
METHOD DataEntry:getUrl()
  LOCAL cUrl
  cUrl := ""
  cUrl += "http://"
  cUrl += GetEnv("ComputerName")
  cUrl += ":"
  cUrl += AllTrim(Str(::getPort()))
  cUrl += "/trace/start"
RETURN cUrl

//
// The configuration is represented by a
// dataobject
//
METHOD DataEntry:getConfiguration()
  LOCAL oConfiguration
  oConfiguration := DataObject():new( AppName()+"Config" )
  // Port
  oConfiguration:Port := ::_Port:getData()
  // URL
  oConfiguration:Url  := ::getUrl()
  // The start page for a web browser
  oConfiguration:StartBrowser := ::_StartBrowser:getData()
RETURN oConfiguration

//EOF
/////
