//////////////////////////////////////////////////////////////////////
//
//  PICTURESERVER.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The class PictureServer delivers the start page for the web browser. A html
//   page is loaded and then feeded with an url suitable to connect to the server.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "web.ch"

CLASS PictureServer FROM WebHandler
  PROTECTED:
    CLASS VAR _UI
  EXPORTED:
    CLASS METHOD initClass
          METHOD start
ENDCLASS

CLASS METHOD PictureServer:initClass( oUI )
  ::_UI := oUI
RETURN self

//The URL with the RESTful path "http://<servername>:<port>/PictureServer/start"
//returns the start page.
METHOD PictureServer:start()
  LOCAL cHtml, cUrl
  cUrl  := "ws://"+::_UI:WSUrl
  cHtml := MemoRead("websocketpictureserver.htm")
  cHtml := StrTran( cHtml, "<!--[URL]-->", cUrl )
RETURN cHtml
