//////////////////////////////////////////////////////////////////////
//
//  FONTDB.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       Create a database that contains all fonts that are
//       installed on the system and displays it.
//   
//////////////////////////////////////////////////////////////////////

#include "Appevent.ch"
#include "Dmlb.ch"
#include "Gra.ch"
#include "Xbp.ch"

****************
PROCEDURE Main()
   LOCAL newVal, oFont, n, oFontView
   LOCAL aFontList := {}
   LOCAL aStruct   := { { "FONTNAME" , "C" , 20 , 0 }, ;
                        { "SIZE"     , "N" , 10 , 0 }, ;
                        { "HWCOMP"   , "N" , 16 , 8 }, ;
                        { "VECTOR"   , "L" , 1  , 0 }, ;
                        { "CODEPAGE", "N" , 5  , 0 }  }

   SetAppWindow():useShortcuts := .T.   
   SetAppWindow():close        := {|| PostAppEvent(xbeK_ESC)}
   SetColor( "N/W" )
   SetMouse( .T. )
   CLS

   ? "Generating database 'FONTS.DBF' ..."
   DbCreate( "FONTS.DBF" , aStruct , NIL )
   USE Fonts EXCLUSIVE

   /*
    * Create list of all fonts installed and write them into
    * the database
    */
   ? "Generating list of installed fonts ..."
   aFontList :=  XbpFont():New():List()

   DbGoTop()

   ? "Writing font records ..."
   FOR n := 1 TO LEN( aFontList )

      DbAppend()

      oFont := aFontList[n]

      /*  { "FONTNAME" , "C" , 20 , 0 }     */
      FieldPut( 1, oFont:compoundName )

      /*  { "SIZE"     , "N" , 10 , 0 }     */
      FieldPut( 2, oFont:nominalPointSize )

      /*  { "HWCOMP"   , "N" , 16 , 8 }     */
      newVal :=  oFont:height + ( oFont:width / 1000 )
      FieldPut( 3, newVal )

      /* { "VECTOR"   , "L" , 1  , 0 }      */
      FieldPut( 4, oFont:vector )

      /* { "CODEPAGE", "N" , 5  , 0 }       */
      FieldPut( 5, oFont:codePage )

      /*
       * Event processing to catch the close event
       */
      DO WHILE NextAppEvent() != xbe_None
         /* Quit program on xbeP_Close      */
         IF AppEvent() == xbeP_Close
            dbCloseArea()
            QUIT
         ENDIF
      ENDDO

   NEXT

   CLS

   oFontView := FontView():new( , , {0,0},                      ;
                                { SetAppWindow():xSize,         ;
                                4 * SetAppWindow():fontHeight } )

   oFontView:fontList := aFontList
   oFontView:create()

   SetColor( "W/N,W+/B" )
   DbGoTop()
   Browse(1, 1, MaxRow()-5, MaxCol()-1 )

   dbCloseArea()
   ? "Done!"

RETURN



****************
/*
 * Declaration of the "FontView" Class for viewing a font
 */
STATIC CLASS FontView FROM XbpStatic
   PROTECTED:
   VAR presSpace, font
   VAR oThread

   EXPORTED:
   VAR testString, fontList
   METHOD init, create, destroy, notify, paint
ENDCLASS



****************
/*
 * Method Init(): Initialise member variables
 */
METHOD FontView:init( oParent, oOwner, aPos, aSize )
   ::XbpStatic:init( oParent, oOwner, aPos, aSize, {{XBP_PP_BGCLR, GRA_CLR_PALEGRAY}} )
   ::XbpStatic:clipParent := .T.
   ::XbpStatic:type       := XBPSTATIC_TYPE_TEXT

   ::presSpace            := XbpPresSpace():new()

   ::testString           := "Alaska Xbase++ displays system fonts"

   ::oThread              := Thread():new()
   ::oThread:setInterval(50)

RETURN self



/*
 * Method Create(): Anfordern von Resourcen fuer das Objekt
 */
METHOD FontView:create( oParent, oOwner, aPos, aSize )
   LOCAL aAttr[ GRA_AA_COUNT ]
   ::xbpStatic:create( oParent, oOwner, aPos, aSize )

   ::presSpace:create( ::winDevice(), ::currentSize() )
   aAttr[ GRA_AA_COLOR ] := GRA_CLR_PALEGRAY
   ::presSpace:setAttrArea( aAttr )

   DbRegisterClient( self )

   ::oThread:start( {|| self:paint()} )

RETURN self



/*
 * Method Destroy(): Free resources allocated on Create()
 */
METHOD FontView:destroy
   ::oThread:setInterval( NIL )
   ::oThread:synchronize( 0 )

   DbDeregisterClient( self )
   ::XbpStatic:destroy()
RETURN self



/*
 * Method Notify(): Notifies a FontView object that a new
 * font has been selected. Note that the display of the font
 * that is currently selected is done via a separate thread.
 */
METHOD FontView:notify( nEvent, mp1 )
   IF mp1 == DBO_MOVE_DONE
      IF Recno() <= Len( ::fontList )
         ::font := ::fontList[ Recno() ]
         ::presSpace:setFont( ::font )
      ELSE
         ::font := NIL
      ENDIF
ENDIF
RETURN self



/*
 * Method Paint(): Overloaded Paint() method for rendering a
 *                 test string using the font currently selected
 */
METHOD FontView:paint( mp1, mp2 )
   IF ::font <> NIL
     ::XbpStatic:paint( mp1, mp2 )
     GraBox(  ::presSpace, {0,0}, ::currentSize(), GRA_FILL )
     GraStringAt( ::presSpace, {12,12}, ::testString )
     ::font := NIL
   ENDIF
RETURN self
