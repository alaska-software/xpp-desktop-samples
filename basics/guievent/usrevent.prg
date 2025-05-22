//////////////////////////////////////////////////////////////////////
//
//  USREVENT.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      The program demonstrates two important techniques for event driven
//      programs: Usage of callback code blocks and user defined events.
//      A listbox displays file names and updates an Sle and an Mle.
//      
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Xbp.ch"

#define   xbeUser_FileMarked       xbeP_User + 1
#define   xbeUser_FileSelected     xbeP_User + 2


PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp, oSle, oMle

   SetColor( "N/W" )
   SetAppWindow():useShortCuts := .T.
   CLS
   @ 0,0 SAY "Listbox updates SLE, Return or left double click updates MLE"


  /*
   * Create listbox and fill it with names of files in the current directory
   */
   oXbp         := XbpListBox():new(,, {12,12}, {256,360} )
   oXbp:tabStop := .T.
   oXbp:create()
   
   oXbp:cargo := Directory( "*.*" )
   AEval( oXbp:cargo, {|a| oXbp:addItem( a[1] ) } )


  /*
   * Callback code blocks are evaluated automatically in :handleEvent()
   * which is executed in the event loop. The callback code blocks of the 
   * listbox post user defined events. The user defined events carry 
   * additional information in the first message parameter (-> file name).
   */
   oXbp:itemMarked   := {|mp1,mp2,obj| ;
                          mp1 := obj:cargo[ obj:getData()[1], 1 ], ;
                          PostAppEvent( xbeUser_FileMarked, mp1 ) }

   oXbp:itemSelected := {|mp1,mp2,obj| ;
                          mp1 := obj:cargo[ obj:getData()[1], 1 ], ;
                          PostAppEvent( xbeUser_FileSelected, mp1 ) }

  /*
   * Mark first item in the listbox and set focus to it
   */
   oXbp:setData( {1}, .T. )
   SetAppFocus( oXbp )


  /*
   * Create Sle and Mle. They are updated when an item is
   * marked or selected in the listbox
   */
   oSle := XbpSLE():new(,, { 280, 348 }, {344,  24} )
   oSle:tabStop := .T.
   oSle:editable  := .F.
   oSle:create()

   oMle := XbpMLE():new(,, { 280,  12 }, {344, 312} )
   oMle:wordWrap  := .F.
   oMle:tabStop   := .T.
   oMle:ignoreTab := .T.
   oMle:editable  := .F.
   oMle:create()


  /*
   * The event loop controls the program
   */
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )

      DO CASE
      CASE nEvent == xbeUser_FileMarked
        /*
         * This user defined event arrives when an item in the listbox is marked
         * The first message parameter of the event is a file name
         */

         oSle:setData( mp1 )

      CASE nEvent == xbeUser_FileSelected
        /*
         * This user defined event arrives when an item in the listbox is double clicked
         * The first message parameter of the event is a file name
         */

         mp2 := Upper( SubStr( mp1, At( ".", mp1 ) ) ) 

         IF mp2 $ ".PRG.TXT.XPJ"
            oMle:setData( Memoread( mp1 ) )
         ELSE
            mp2 := SetAppFocus()
            MsgBox( "File must have extension .PRG or .TXT or .XPJ", "Illegal File" )
            SetAppFocus( mp2 )
         ENDIF

      OTHERWISE
         oXbp:handleEvent( nEvent, mp1, mp2 )

      ENDCASE
   ENDDO

RETURN
