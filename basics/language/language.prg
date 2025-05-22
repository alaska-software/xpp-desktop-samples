//////////////////////////////////////////////////////////////////////
//
//  LANGUAGE.PRG
//
//  Copyright:
//           Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//           Demonstrates the usage of several languages
//           as resources with Xbase++
//   
//  Remarks:
//           The resources are defined in LANGUAGE.ARC
//   
//////////////////////////////////////////////////////////////////////


#include "dll.ch"
#include "appEvent.ch"
#include "xbp.ch"

#include "language.ch"

STATIC cLang
STATIC nEvent
STATIC oMsg

PROCEDURE AppSys
RETURN

PROCEDURE Main()

    LOCAL mp1, mp2, oXbp, oDraw, oMenu, oMenuBar

    cLang  := "de"
    nEvent := 0

    oXbp := XbpDialog():new( , , {100,100} , {300,300} )
    oXbp:title := "MultiLanguageTestDialog"
    oXbp:create()

    oDraw := oXbp:drawingArea

    SetAppWindow( oXbp )

    oMenuBar := oXbp:menuBar()

   /*
    * As the menu items are defined later in relation to the selected
    * language, we create dummys to initialize the items
    */
    oMenu := XbpMenu():new( oMenuBar )
    oMenu:create()
    oMenu:AddItem( {"1",{||NIL}} )

    oMenuBar:addItem( { oMenu,NIL } )

    oMenu := XbpMenu():new( oMenuBar )
    oMenu:create()
    oMenu:AddItem({"1",{||NIL}})
    oMenu:AddItem({"2",{||NIL}})
    oMenu:AddItem({"3",{||NIL}})

    oMenuBar:addItem( { oMenu,NIL } )

    ChkLang(oMenuBar)

    oXbp := XbpStatic():new( oDraw,, {10,200},, {{XBP_PP_COMPOUNDNAME, "14.Arial"}})
    oXbp:autoSize := .T.
    oXbp:Caption := "MultiLanguage Message Sample"
    oXbp:create()

    oMsg := XbpStatic():new( oDraw, , {10,150}, {240,30}, {{XBP_PP_COMPOUNDNAME, "12.Arial"}})
    oMsg:autoSize := .T.
    oMsg:caption := Loadresource( STAT1,, RES_STRING, cLang)
    oMsg:create()

    oXbp := XbpPushButton():new( oDraw,, {25,20}, {50,50})
    oXbp:caption := "MSG1"
    oXbp:Activate := {|| oMsg:setCaption( Loadresource(  MSG1,, RES_STRING, cLang))}
    oXbp:create()

    oXbp := XbpPushButton():new( oDraw,, {85,20}, {50,50})
    oXbp:caption := "MSG2"
    oXbp:Activate := {|| oMsg:setCaption( Loadresource( MSG2,, RES_STRING, cLang))}
    oXbp:create()

    oXbp := XbpPushButton():new( oDraw,, {145,20}, {50,50} )
    oXbp:caption := "MSG3"
    oXbp:Activate := {|| oMsg:setCaption( Loadresource( MSG3,, RES_STRING, cLang))}
    oXbp:create()

    oXbp := XbpPushButton():new(oDraw,,{205,20},{50,50})
    oXbp:caption := "MSG4"
    oXbp:Activate := {|| oMsg:setCaption( Loadresource( MSG4,, RES_STRING, cLang))}
    oXbp:create()

    SetAppFocus(oXbp)
    DO WHILE nEvent <> xbeP_Close
       nEvent := AppEvent( @mp1, @mp2, @oXbp )
       oXbp:HandleEvent( nEvent, mp1, mp2 )
    ENDDO

RETURN

PROCEDURE ChkLang( oMenuBar )

    LOCAL oMen1 := oMenuBar:childList()[1]
    LOCAL oMen2 := oMenuBar:childList()[2]

   /*
    * Change the menu caption to the chosen language
    */
    oMen1:setTitle("~"+Loadresource( MEN1,,RES_STRING, cLang ))

    oMen1:setItem(1, { "~"+Loadresource( MEN11,,RES_STRING, cLang ) , ;
                     { || nEvent := xbeP_Close } } )

    oMen2:setTitle("~"+Loadresource( MEN2,,RES_STRING, cLang ))

    oMen2:setItem(1, { "~"+Loadresource( MEN21,,RES_STRING, cLang ) , ;
                   { |nItem,lCheck,o| cLang := "de",ChkLang(oMenuBar) , ;
                     oMsg:setCaption( Loadresource( STAT1,,RES_STRING, cLang ) ) } } )

    oMen2:setItem(2, { "~"+Loadresource( MEN22,,RES_STRING, cLang ) , ;
                   { |nItem,lCheck,o| cLang := "en",ChkLang(oMenuBar) , ;
                     oMsg:setCaption( Loadresource( STAT1,,RES_STRING, cLang ) ) } } )

    oMen2:setItem(3, { "~"+Loadresource( MEN23,,RES_STRING, cLang ) , ;
                   { |nItem,lCheck,o| cLang := "fr",ChkLang(oMenuBar) , ;
                     oMsg:setCaption( Loadresource( STAT1,,RES_STRING, cLang ) ) } } )

   /*
    * Provide the chosen menu item with a check mark
    */
    DO CASE
      CASE cLang == "de"
        oMen2:checkItem(1,.t.)
        oMen2:checkItem(2,.f.)
        oMen2:checkItem(3,.f.)
       CASE cLang == "en"
        oMen2:checkItem(1,.f.)
        oMen2:checkItem(2,.t.)
        oMen2:checkItem(3,.f.)
       CASE cLang == "fr"
        oMen2:checkItem(1,.f.)
        oMen2:checkItem(2,.f.)
        oMen2:checkItem(3,.t.)
    ENDCASE

RETURN
