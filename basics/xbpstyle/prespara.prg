//////////////////////////////////////////////////////////////////////
//
//  PRESPARA.PRG
//
//  Copyright:
//      Alaska Software, (c) 1999-2025. All rights reserved.         
//  
//  Contents:
//      This program demonstrates the inheritance of presentation parameters
//      in the Parent-Child hierarchy of Xbase Parts, or windows. All Xbase Parts
//      that have no default value for a particular presentation parameter (or
//      have no value defined for it) inherit the presentation parameter from
//      the parent.
//   
//      The value for all presentation parameters that default to NIL is taken
//      from the system control panel when an Xbase Part is created.
//   
//  Remarks:
//      You should run the program and play with it before you read the source
//      code. This gives you a better idea what "inheritance of presentation
//      parameters" means.
//   
//      Note that it is sufficient to define a particular presentation parameter
//      for the parent so that all children take the same settings, unless a
//      child has its own presentation parameters.
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Gra.ch"
#include "Font.ch"
#include "Xbp.ch"


PROCEDURE AppSys
/*
 * Empty AppSys: The application window is created in Main()
 */
RETURN


PROCEDURE Main
   LOCAL nEvent, mp1 := NIL, mp2 := NIL, aPP
   LOCAL oDlg, oXbp, drawingArea, oXbp1
   LOCAL oTxtFont, oTxtBgClr, oTxtFgClr

   oDlg              := XbpDialog():new( AppDesktop(), , {15,89}, {600,400}, , .F.)
   oDlg:taskList     := .T.
   oDlg:title        := "Presentation Parameter"
   oDlg:create()
   SetAppWindow( oDlg )

   drawingArea       := oDlg:drawingArea
   drawingArea:setColorFG( GRA_CLR_BLACK      )
   drawingArea:setColorBG( GRA_CLR_PALEGRAY   )
   drawingArea:setFontCompoundName( FONT_HELV_SMALL )

   oXbp              := XbpSLE():new( drawingArea, , {32,332}, {540,20} )
   oXbp:create()
   oXbp:setData( "I use the system color for background. Foreground color and font are taken from the parent" )


   aPP               := { { XBP_PP_BGCLR       , GRA_CLR_GREEN    }, ;
                          { XBP_PP_COMPOUNDNAME, FONT_HELV_MEDIUM }  }
   oXbp              := XbpCheckbox():new( drawingArea, , {32,292}, {540,20}, aPP )
   oXbp:caption      := "I use only the foreground color of the parent"
   oXbp:create()


   aPP               := { { XBP_PP_FGCLR       , GRA_CLR_RED      }, ;
                          { XBP_PP_COMPOUNDNAME, FONT_HELV_MEDIUM } }
   oXbp              := XbpRadiobutton():new( drawingArea, , {32,252}, {540,20}, aPP )

   oXbp:caption      := "I use only the background color of the parent"
   oXbp:create()


   aPP               := { { XBP_PP_FGCLR       , GRA_CLR_YELLOW  }, ;
                          { XBP_PP_BGCLR       , GRA_CLR_BLUE    }  }
   oXbp              := Xbp3state():new( drawingArea, , {32,208}, {540,20}, aPP )
   oXbp:caption      := "I use my own colors but inherit the parent font"
   oXbp:create()


   aPP               := { { XBP_PP_FGCLR       , GRA_CLR_WHITE     }, ;
                          { XBP_PP_BGCLR       , GRA_CLR_DARKGRAY  }, ;
                          { XBP_PP_COMPOUNDNAME, FONT_TIMES_MEDIUM }  }
   oXbp              := XbpSLE():new( drawingArea, , {32,164}, {540,20}, aPP )
   oXbp:create()
   oXbp:setData( "My colors and font remain unchanged" )


   aPP               := { { XBP_PP_FGCLR       , GRA_CLR_BLACK        }, ;
                          { XBP_PP_BGCLR       , XBPSYSCLR_ENTRYFIELD }, ;
                          { XBP_PP_COMPOUNDNAME, FONT_DEFPROP_LARGE   }  }
   oXbp              := XbpStatic():new( drawingArea, , {32,96}, {540,52}, aPP )
   oXbp:caption      := "Change the background color for 'WINDOW'"  + Chr(13) + ;
                        "in the system control panel"
   oXbp:options      := XBPSTATIC_TEXT_WORDBREAK
   oXbp:create()

   aPP               := { { XBP_PP_FGCLR       , GRA_CLR_BLUE       }, ;
                          { XBP_PP_COMPOUNDNAME, FONT_DEFPROP_SMALL }  }

   oXbp1             := XbpStatic():new( drawingArea, , {16,12}, {564,80}, aPP )
   oXbp1:caption     := "Change presentation parameters for the parent => oXbpDialog:drawingArea"
   oXbp1:type        := XBPSTATIC_TYPE_GROUPBOX
   oXbp1:create()


   /*
    * XbpStatics displaying the selected presentation parameters
    */
   oTxtFont          := XbpStatic():new( oXbp1, , {16,8}, {100,20}, aPP )
   oTxtFont:caption  := FONT_HELV_SMALL
   oTxtFont:options  := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oTxtFont:create()


   oTxtFgClr         := XbpStatic():new( oXbp1, , {136,8}, {100,20}, aPP )
   oTxtFgClr:caption := "Black"
   oTxtFgClr:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oTxtFgClr:create()


   oTxtBgClr         := XbpStatic():new( oXbp1, , {260,8}, {100,20}, aPP )
   oTxtBgClr:caption := "Pale gray"
   oTxtBgClr:options := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   oTxtBgClr:create()


   /*
    * XbpPushbuttons change a presentation parameter for the parent
    */
   oXbp              := XbpPushButton():new( oXbp1, , {16,36}, {100,24}, aPP )
   oXbp:caption      := "Font"
   oXbp:cargo        := FONT_HELV_SMALL
   oXbp:create()
   oXbp:activate     := {|x,y,obj| drawingArea:setFontCompoundName(             ;
                                      IIf(  obj:cargo == FONT_HELV_SMALL      , ;
                                           (obj:cargo := FONT_COURIER_SMALL)  , ;
                                           (obj:cargo := FONT_HELV_SMALL)       ;
                                         )                                      ;
                                   )                                          , ;
                                   oTxtFont:setCaption( obj:cargo )             ;
                        }


   oXbp              := XbpPushButton():new( oXbp1, , {136,36}, {100,24}, aPP )
   oXbp:caption      := "Foreground color"
   oXbp:cargo        := GRA_CLR_BLACK
   oXbp:create()
   oXbp:activate     := {|x,y,obj| drawingArea:setColorFG(                      ;
                                      IIf(  obj:cargo == GRA_CLR_BLACK        , ;
                                           (obj:cargo := GRA_CLR_YELLOW)     , ;
                                           (obj:cargo := GRA_CLR_BLACK)         ;
                                         )                                      ;
                                   )                                          , ;
                                   oTxtFgClr:setCaption(                        ;
                                         IIf( obj:cargo == GRA_CLR_BLACK, "Black", "Yellow" ) ;
                                   )                                            ;
                        }


   oXbp              := XbpPushButton():new( oXbp1, , {260,36}, {100,24}, aPP )
   oXbp:caption      := "Background color"
   oXbp:cargo        := GRA_CLR_PALEGRAY
   oXbp:create()
   oXbp:activate     := {|x,y,obj| drawingArea:setColorBG(                      ;
                                      IIf(  obj:cargo == GRA_CLR_PALEGRAY     , ;
                                           (obj:cargo := GRA_CLR_PINK)        , ;
                                           (obj:cargo := GRA_CLR_PALEGRAY)      ;
                                         )                                      ;
                                   )                                          , ;
                                   oTxtBgClr:setCaption(                        ;
                                         IIf( obj:cargo == GRA_CLR_PALEGRAY,"Palegray","Pink" ) ;
                                   )                                            ;
                        }


   oXbp              := XbpPushButton():new( oXbp1, , {452,36}, {100,24}, aPP )
   oXbp:caption      := "Quit"
   oXbp:create()
   oXbp:activate     := {|| PostAppEvent( xbeP_Close ) }

   oDlg:show()

   nEvent := xbe_None
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN
