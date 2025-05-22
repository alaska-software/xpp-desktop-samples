//////////////////////////////////////////////////////////////////////
//
//  PICTURESERVERUI.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The Class PictureServerUI implements the user interface of the server.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "font.ch"

#define DEFAULT_PORT 81

******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _PictureServer class.
******************************************************************************
CLASS PictureServerUI FROM _PictureServerUI
      VAR _HttpEndpoint
      VAR _ConnectedClientsNum

   EXPORTED:
      METHOD init
      METHOD create
      METHOD updateConnectedClients
      METHOD setUIStopped
      METHOD setUIRunning
      METHOD shutdown
      METHOD startup
      METHOD go
      METHOD copyUrl

      // methods invoked by the view model
      METHOD processPicture
      METHOD connected
      METHOD disconnected

      ACCESS METHOD getUrl()   VAR URL
      ACCESS METHOD getWSUrl() VAR WSURL
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD PictureServerUI:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_PictureServerUI:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Title := "Picture Server"

   ::_Shutdown:Activate := {|| ::shutdown() }
   ::_Startup:Activate  := {|| ::startup()  }
   ::_Go:Activate       := {|| ::go()       }
   ::_CopyUrl:Activate  := {|| ::copyUrl()  }

   ::_ConnectedClientsNum := 0
   ::_HttpEndpoint        := NIL

   ::updateConnectedClients()

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD PictureServerUI:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_PictureServerUI:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::_ComputerName:setCaption( GetEnv("Computername") )
   ::_Port:setData( DEFAULT_PORT )

   ::setUIStopped()

   * Display the form
   ::show()

RETURN self

METHOD PictureServerUI:updateConnectedClients()
  ::_ConnectedClients:setCaption( AllTrim(Str(::_ConnectedClientsNum)) )
RETURN self

METHOD PictureServerUI:setUIStopped()
  ::_BrowseUrl:setCaption( "" )
  ::_ConnectedClients:setColorFG(GRA_CLR_RED)
  ::_Go:disable()
  ::_CopyUrl:disable()
  ::_Port:enable()
  ::_Processing:setCaption( "" )
  ::_Shutdown:disable()
  ::_Startup:enable()
  ::_Status:setCaption( "Stopped" )
  ::_Status:setColorFG(GRA_CLR_RED)
RETURN self

METHOD PictureServerUI:setUIRunning()
  ::_BrowseUrl:setCaption( "http://"+::Url )
  ::_ConnectedClients:setColorFG(GRA_CLR_DARKGREEN)
  ::_Go:enable()
  ::_CopyUrl:enable()
  ::_Port:disable()
  ::_Processing:setCaption( "" )
  ::_Shutdown:enable()
  ::_Startup:disable()
  ::_Status:setCaption( "Running" )
  ::_Status:setColorFG(GRA_CLR_DARKGREEN)
RETURN self

METHOD PictureServerUI:shutdown()
  ::setUIStopped()
  IF .NOT. (NIL==::_HttpEndpoint)
    ::_HttpEndpoint:stop()
    ::_HttpEndpoint := NIL
  ENDIF
  WebSocketPictureServer():closeAll()
RETURN self

METHOD PictureServerUI:startup()
  LOCAL nPort

  ::setUIRunning()

  nPort := ::_Port:getData()

  ::_HttpEndpoint := HttpEndpoint():new(nPort)
  ::_HttpEndpoint:start()

RETURN self

METHOD PictureServerUI:go()
  LOCAL cUrl
  cUrl := "http://"+::Url
  RunShell( "/C explorer.exe "+cUrl,, .T., .T. )
RETURN self

METHOD PictureServerUI:copyUrl()
  LOCAL cUrl, oClipBoard
  cUrl := "http://"+::Url

  oClipBoard := XbpClipBoard():new():create()
  oClipBoard:open()
  oClipBoard:clear()
  oClipBoard:setBuffer( cUrl )
  oClipBoard:close()
  oClipBoard:destroy()
RETURN self

METHOD PictureServerUI:processPicture( cPicturePath )
  ::_Processing:setCaption( cPicturePath )
RETURN self

METHOD PictureServerUI:connected()
  ::_ConnectedClientsNum++
  ::updateConnectedClients()
RETURN self

METHOD PictureServerUI:disconnected()
  ::_ConnectedClientsNum--
  ::updateConnectedClients()
RETURN self

METHOD PictureServerUI:getUrl()
  LOCAL cUrl := ""
  cUrl += ::_ComputerName:caption+":"
  cUrl += AllTrim(Str(::_Port:getData()))
  cUrl += "/pictureserver/start"
RETURN cUrl


METHOD PictureServerUI:getWSUrl()
  LOCAL cUrl := ""
  cUrl += ::_ComputerName:caption+":"
  cUrl += AllTrim(Str(::_Port:getData()))
  cUrl += "/websocketpictureserver"
RETURN cUrl


******************************************************************************
* Main procedure to test a form
******************************************************************************
PROCEDURE Main
   LOCAL oPictureServerUI

   SET DEFAULT TO ..\..\data\northwind\dbf

   oPictureServerUI := SetAppWindow()

   PictureServer( oPictureServerUI )
   WebSocketPictureServer( oPictureServerUI )

   oPictureServerUI:showModal()

   oPictureServerUI:shutdown()

RETURN

PROCEDURE AppSys()
   LOCAL oPictureServerUI
   oPictureServerUI := PictureServerUI():New():Create()
   SetAppWindow( oPictureServerUI )
RETURN

PROCEDURE DbeSys()
  IF .NOT. DbeLoad( "FOXDBE", .F. )
    MsgBox( "Can not load FOXDBE" )
    quit
  ENDIF
RETURN

//EOF
/////
