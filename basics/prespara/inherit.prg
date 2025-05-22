//////////////////////////////////////////////////////////////////////
//
//  INHERIT.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      This program demonstrates the inheritance of presentation parameters.
//      All Xbase Parts that have no default value for a particular presentation
//      parameter (or have no value defined for it) inherit the presentation
//      parameter from the parent.
//   
//  Remarks:
//      An XbpStatic and XbpSLE object is transported from one parent to another
//      just by clicking a pushbutton.
//   
//////////////////////////////////////////////////////////////////////



#include "Gra.ch"
#include "Xbp.ch"
#include "Appevent.ch"
#include "Font.ch"


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL oParent1, oParent2, oParent3, oParent4
   LOCAL oTxt1, oTxt2
   LOCAL oSle1, oSle2
   LOCAL aPP1 , aPP2


   ? "Foreground color left:  GRA_CLR_DARKPINK   right:  GRA_CLR_RED"
   ? "Background color left:  GRA_CLR_DARKGRAY   right:  GRA_CLR_DARKGREEN"
   ? "Font             left:  FONT_HELV_SMALL    right:  FONT_TIMES_SMALL"
   ?
   ? "Click the pushbuttons to change the parent."


   /*
    * Presentation parameters for upper and lower rectangle
    */
   aPP1      := { { XBP_PP_FGCLR       , GRA_CLR_DARKPINK  }, ;
                  { XBP_PP_BGCLR       , GRA_CLR_DARKGRAY  }, ;
                  { XBP_PP_COMPOUNDNAME, FONT_HELV_SMALL   }  }

   aPP2      := { { XBP_PP_FGCLR       , GRA_CLR_RED       }, ;
                  { XBP_PP_BGCLR       , GRA_CLR_DARKGREEN }, ;
                  { XBP_PP_COMPOUNDNAME, FONT_TIMES_SMALL  }  }

   /*
    * XbpStatic objects display the rectangles
    */
   oParent1  := XbpStatic():new( , , { 12,168}, {204,120}, aPP1 ):create()
   oParent2  := XbpStatic():new( , , {332,168}, {204,120}, aPP2 ):create()
   oParent3  := XbpStatic():new( , , { 12, 20}, {204,120}, aPP1 ):create()
   oParent4  := XbpStatic():new( , , {332, 20}, {204,120}, aPP2 ):create()


   /*
    * XbpStatic and XbpSLE which inherit presentation parameters from
    * the parent because no presentation parameters are passed to the
    * :new() method. Note that XbpSLE has a default background color that
    * is inherited from the operating system's control panel.
    */
   oTxt1         := XbpStatic():new( oParent1, , {32,56}, {148,48} )
   oTxt1:caption := "We inherit our presentation parameters from the parent"
   oTxt1:options := XBPSTATIC_TEXT_WORDBREAK
   oTxt1:create()

   oSle1         := XbpSLE():new( oParent1, , {32,16}, {148,24} )
   oSle1:create()
   oSle1:setData( "System background color" )

   /*
    * XbpStatic and XbpSLE having their own presentation parameters,
    * since they are specified for :new().
    */
   aPP1          := { { XBP_PP_FGCLR       , GRA_CLR_BLUE        }, ;
                      { XBP_PP_BGCLR       , GRA_CLR_PALEGRAY    }, ;
                      { XBP_PP_COMPOUNDNAME, FONT_DEFFIXED_SMALL }  }

   oTxt2         := XbpStatic():new( oParent3, , {16,56}, {170,48}, aPP1 )
   oTxt2:caption := "We use our own presentation parameters"
   oTxt2:options := XBPSTATIC_TEXT_WORDBREAK
   oTxt2:create()


   aPP2          := { { XBP_PP_FGCLR       , GRA_CLR_DARKRED     }, ;
                      { XBP_PP_BGCLR       , GRA_CLR_CYAN        }, ;
                      { XBP_PP_COMPOUNDNAME, FONT_DEFPROP_SMALL  }  }

   oSle2         := XbpSLE():new( oParent3, , {32,16}, {148,24}, aPP2 )
   oSle2:create()
   oSle2:setData( "Own background color" )


   /*
    * The following two pushbuttons simply change the parent with :setParent()
    */
   oXbp          := XbpPushButton():new( , , {224,216}, {100,24} )
   oXbp:caption  := "Parent -->>"
   oXbp:cargo    := oParent1
   oXbp:create()
   oXbp:activate := {|x,y,o|                                                            ;
                      x       := IIf( o:cargo == oParent1, oParent2, oParent1 )       , ;
                      o:cargo := x                                                    , ;
                      oTxt1:setParent ( x )                                           , ;
                      oSle1:setParent ( x )                                           , ;
                      o:setCaption( IIf( x==oParent1, "Parent -->>", "<<-- Parent" ) )  ;
                    }


   oXbp          := XbpPushButton():new( , , {224,68}, {100,24} )
   oXbp:caption  := "Parent -->>"
   oXbp:cargo    := oParent3
   oXbp:create()
   oXbp:activate := {|x,y,o|                                                            ;
                      x       := IIf( o:cargo == oParent3, oParent4, oParent3 )       , ;
                      o:cargo := x                                                    , ;
                      oTxt2:setParent ( x )                                           , ;
                      oSle2:setParent ( x )                                           , ;
                      o:setCaption( IIf( x==oParent3, "Parent -->>", "<<-- Parent" ) )  ;
                     }

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN
