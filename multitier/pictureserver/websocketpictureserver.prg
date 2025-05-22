//////////////////////////////////////////////////////////////////////
//
//  WEBSOCKETPICTURESERVER.PRG
//
//  Copyright:
//   Alaska Software, (c) 2012-2025. All rights reserved.         
//  
//  Contents:
//   The class WebSocketPictureServer is derived from WebSocketHandler.
//   After a connection with ws://<servername>:<port>/WebSocketPictureServer
//   is established the client is connected with an instanz of this class.
//   
//  Remarks:
//   
//////////////////////////////////////////////////////////////////////


#include "Directry.ch"
#include "fileio.ch"
#include "assert.ch"

CLASS WebSocketPictureServer FROM WebSocketHandler
  PROTECTED:
    CLASS VAR _UI
    CLASS VAR _Instances
          VAR _Pictures
          VAR _PicturePath
          VAR _WA

    SYNC CLASS METHOD _addSelf
    SYNC CLASS METHOD _delSelf

               METHOD _next
               METHOD _prev
               METHOD _sendCurrentPicture
  EXPORTED:
         CLASS METHOD initClass
    SYNC CLASS METHOD closeAll

               METHOD init
               METHOD onConnect
               METHOD onDisconnect
               METHOD onText
               METHOD onBinary
ENDCLASS

CLASS METHOD WebSocketPictureServer:_addSelf( oSelf )
  AAdd( ::_Instances, oSelf )
RETURN self

CLASS METHOD WebSocketPictureServer:_delSelf( oSelf )
  LOCAL nPos
  nPos := AScan( ::_Instances, oSelf )
  ASSERT( nPos != 0 )
  ARemove( ::_Instances, nPos )
RETURN self

CLASS METHOD WebSocketPictureServer:initClass( oUI )
  ::_UI        := oUI
  ::_Instances := {}
RETURN self

CLASS METHOD WebSocketPictureServer:closeAll()
  LOCAL oInstance, nInstance, nInstances
  nInstances := Len( ::_Instances )
  FOR nInstance := 1 to nInstances
    oInstance := ::_Instances[nInstance]
    oInstance:close()
  NEXT
  ::_Instances := {}
RETURN self

METHOD WebSocketPictureServer:init()

  DbUseArea( .T., "FOXDBE", "employees" )
  ::_WA := Select()

  ::_addSelf(self )

RETURN self

METHOD WebSocketPictureServer:_next()
  DbSkip()
  IF Eof()
    DbGoTop()
  ENDIF
RETURN self

METHOD WebSocketPictureServer:_prev()
  DbSkip(-1)
  IF Bof()
    DbGoBottom()
  ENDIF
RETURN self

METHOD WebSocketPictureServer:_sendCurrentPicture()

  ::sendBinary( (::_WA)->photo )

  ::_UI:processPicture( AllTrim((::_WA)->firstname) + " " + ;
                        AllTrim((::_WA)->lastname) )
RETURN self

METHOD WebSocketPictureServer:onConnect()
  ::_UI:connected()
RETURN self

METHOD WebSocketPictureServer:onDisconnect()
  ::_UI:disconnected()
  (::_WA)->( DbCloseArea() )
  ::_WA := NIL
RETURN self

METHOD WebSocketPictureServer:onText( cText )
  cText := Lower( cText )
  DO CASE
    CASE cText == "current"
      // no navigation here
    CASE cText == "next"
      (::_WA)->( ::_next() )
    CASE cText == "prev"
      (::_WA)->( ::_prev() )
    OTHERWISE
      RETURN
  ENDCASE
  ::_sendCurrentPicture()
RETURN self

METHOD WebSocketPictureServer:onBinary( cBinary )
  Unused( cBinary )
RETURN self
