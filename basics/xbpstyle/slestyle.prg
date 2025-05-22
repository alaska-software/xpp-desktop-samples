//////////////////////////////////////////////////////////////////////
//
//  SLESTYLE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      Display of XbpSLE objects using different presentation
//      parameters (PP).
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Gra.ch"
#include "Xbp.ch"
#include "Font.ch"


/*
 * Display XbpSLE objects with different attributes
 */
PROCEDURE Main
   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp
   LOCAL aPos := {120,325}, aSize := {400,24}

   SetColor( "N/W" )
   CLS
   SetAppWindow():useShortCuts := .T.

   @ 0, 0 SAY "Examples of how XbpSle objects can appear"

   oXbp          := XbpStatic():new(,, {12,54}, {616,2} )
   oXbp:type     := XBPSTATIC_TYPE_RAISEDLINE
   oXbp:create()

   oXbp          := XbpPushbutton():new(,, {12,12}, {100,30} )
   oXbp:caption  := "Quit"
   oXbp:tabStop  := .T.
   oXbp:activate := {|| PostAppEvent( xbeP_Close ) }
   oXbp:create()
   oXbp:setFontCompoundName( "8.Helv" )

   DefaultSle ( aPos, aSize )
   ColorSle   ( aPos, aSize )
   NoframeSle ( aPos, aSize )
   ChgColorSle( aPos, aSize )
   CustomSle  ( aPos, aSize )

   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN


/*
 * Display default XbpSLE object.
 */
PROCEDURE DefaultSle( aPos, aSize )
   LOCAL oXbp

   oXbp              := XbpSLE():new(,, aPos, aSize )
   oXbp:tabStop      := .T.
   oXbp:bufferLength := 64
   oXbp:create()

   oXbp:setData( "Default SLE" )
   aPos[2] -= 50
RETURN


/*
 * SLE with fore/background colors and font defined
 */
PROCEDURE ColorSle( aPos, aSize )
   LOCAL oXbp
   LOCAL aPP := { { XBP_PP_FGCLR       , GRA_CLR_RED         } , ;
                  { XBP_PP_BGCLR       , GRA_CLR_YELLOW      } , ;
                  { XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL } }

   oXbp              := XbpSLE():new(,, aPos, aSize, aPP )
   oXbp:tabStop      := .T.
   oXbp:bufferLength := 64
   oXbp:create()

   oXbp:setData( "Colors and font defined" )
   aPos[2] -= 50
RETURN



/*
 * SLE has no frame but a line underneath
 */
PROCEDURE NoframeSle( aPos, aSize )
   LOCAL oXbp
   LOCAL aPP := { { XBP_PP_BGCLR       , GRA_CLR_RED         } , ;
                  { XBP_PP_COMPOUNDNAME, FONT_HELV_MEDIUM    } }

   oXbp              := XbpSLE():new(,, aPos, aSize, aPP )
   oXbp:tabStop      := .T.
   oXbp:bufferLength := 64
   oXbp:border       := .F.
   oXbp:create()

   oXbp:setData( "No frame but extra line" )

  /*
   * Create the line underneath the SLE
   */
   aPos[2]   -= 3
   oXbp      := XbpStatic():new(,, aPos, {aSize[1],2} )
   oXbp:type := XBPSTATIC_TYPE_RAISEDLINE
   oXbp:create()

   aPos[2] -= 50
RETURN


/*
 * Change color when focus changes. This is done within callback codeblocks
 */
PROCEDURE ChgColorSle( aPos, aSize )
   LOCAL oXbp
   LOCAL aPP := { { XBP_PP_FGCLR       , GRA_CLR_BLUE        } , ;
                  { XBP_PP_BGCLR       , GRA_CLR_PALEGRAY    } , ;
                  { XBP_PP_COMPOUNDNAME, FONT_TIMES_MEDIUM + FONT_STYLE_BOLD }}

   oXbp                := XbpSLE():new(,, aPos, aSize, aPP )
   oXbp:tabStop        := .T.
   oXbp:bufferLength   := 64
   oXbp:border         := .F.
   oXbp:setInputFocus  := {|mp1,mp2,obj| HiliteSle( obj ) }
   oXbp:killInputFocus := {|mp1,mp2,obj| DeHiliteSle( obj ) }
   oXbp:create()

   oXbp:setData( "No frame, color changes with focus change" )
   aPos[2] -= 50
RETURN


PROCEDURE HiliteSle( oXbpSle )
   LOCAL aPP := { { XBP_PP_FGCLR       , GRA_CLR_WHITE       } , ;
                  { XBP_PP_BGCLR       , GRA_CLR_BLUE        } , ;
                  { XBP_PP_COMPOUNDNAME, FONT_TIMES_MEDIUM + FONT_STYLE_BOLD } }
   oXbpSle:setPresParam( aPP )
RETURN


PROCEDURE DeHiliteSle( oXbpSle )
   LOCAL aPP := { { XBP_PP_FGCLR       , GRA_CLR_BLUE        } , ;
                  { XBP_PP_BGCLR       , GRA_CLR_PALEGRAY    } , ;
                  { XBP_PP_COMPOUNDNAME, FONT_TIMES_MEDIUM + FONT_STYLE_BOLD } }
   oXbpSle:setPresParam( aPP )
RETURN


/*
 * Display a customized frame for SLE
 */
PROCEDURE CustomSle( aPos, aSize )
   LOCAL oXbp, oXbp1
   LOCAL cFont1 := FONT_HELV_MEDIUM + FONT_STYLE_BOLD
   LOCAL cFont2 := FONT_HELV_MEDIUM + FONT_STYLE_BOLD + FONT_STYLE_ITALIC
   LOCAL aPP := { { XBP_PP_BGCLR       , GRA_CLR_PALEGRAY    }, ;
                  { XBP_PP_COMPOUNDNAME, cFont1              } }

   oXbp      := XbpStatic():new(,, aPos, aSize )
   oXbp:type := XBPSTATIC_TYPE_RAISEDBOX
   oXbp:create()

   aSize[1] -= 2
   aSize[2] -= 2

  /*
   * SLE is child of raised box
   */
   oXbp1                := XbpSLE():new( oXbp,, {1,1}, aSize, aPP )
   oXbp1:tabStop        := .T.
   oXbp1:border         := .F.
   oXbp1:bufferLength   := 64
   oXbp1:create()
   oXbp1:setInputFocus  := {|mp1,mp2,obj| obj:setFontCompoundName( cFont2 ) }
   oXbp1:killInputFocus := {|mp1,mp2,obj| obj:setFontCompoundName( cFont1 ) }

   oXbp1:setData( "Customized frame and font for SLE" )
   aPos[2] -= 50
RETURN
