//////////////////////////////////////////////////////////////////////
//
//  RTFDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program demonstrates the usage of an XbpHelp object and    
//      a RTF-based online help.
//   
//////////////////////////////////////////////////////////////////////


#include "xbp.ch"     
#include "appevent.ch"

#define B_BOTTOM 15

STATIC  scRtfFile, scHelpTopic, scHelpID, snHelpID

#define ID_HLP100    100 
#define ID_HLP200    200 
#define ID_HLP300    300 
#define ID_HLP400    400 
#define ID_HLP500    500 
#define ID_HLP600    600 
#define ID_HLP700    700 
#define ID_HLP800    800 
#define ID_HLP900    900 
#define ID_HLP1000  1000

                              
STATIC  saTopics     := { ;
   { "What is RTF ?"            , "RTFBASIC.RTF" , "HLP100" , ID_HLP100}, ;
   { "Characteristics"          , "RTFBASIC.RTF" , "HLP200" , ID_HLP200}, ;
   { "Document definitions"     , "RTFBASIC.RTF" , "HLP300" , ID_HLP300}, ;
   { "Characters"               , "RTFTOKEN.RTF" , "HLP400" , ID_HLP400}, ;
   { "Paragraph"                , "RTFTOKEN.RTF" , "HLP500" , ID_HLP500}, ;
   { "Page"                     , "RTFTOKEN.RTF" , "HLP600" , ID_HLP600}, ;
   { "\footnote"                , "RTFTOKEN.RTF" , "HLP700" , ID_HLP700}, ;
   { "Hypertext Links"          , "RTFTOKEN.RTF" , "HLP800" , ID_HLP800}  }


PROCEDURE Main
   LOCAL  nEvent, mp1, mp2, oXbp
   LOCAL  oOk, oQuit, oHelp, oXbpHelp, oMLE, oCombo, oStatic
   LOCAL  cText, nXsize, nYsize, i, imax

   SetColor ( "N/W" )
   CLS
   SetMouse(.T.)
   SetAppWindow():useShortCuts := .T.
   nXsize := SetAppWindow():xSize
   nYsize := SetAppWindow():ySize
   

   oXbpHelp := XbpHelp():New( , "rtfdemoe.hlp",  "Windows RTF Help Demo" )
   oXbpHelp:Create()

   oStatic := XbpStatic():New ( , , {10, 60}, {nXsize-20, nYsize-70} )
   oStatic:Type := XBPSTATIC_TYPE_RAISEDBOX
   oStatic:Caption := ""
   oStatic:Create()

  /*
   * Combobox is a child of oStatic to assure a correct repaint
   */
   oCombo := XbpCombobox():New ( oStatic, oStatic, ;
                                 {10, nYsize-85-200}, {nXsize-40, 200} )
   oCombo:Type := XBPCOMBO_DROPDOWNLIST
   oCombo:Create()
   oCombo:helpLink := XbpHelpLabel():new():create( ID_HLP900 )
   oCombo:helpLink:helpObject := oXbpHelp

   cText := "Demo program for online help" +Chr(13)+Chr(10) + ;
            "The major part of the RTF source code" +Chr(13)+Chr(10) + ;
            "is displayed in this window once the online help is activated"

  /*
   * MLE is a child of oStatic since the MLE
   * is located within oStatic
   */
   oMLE := XbpMLE():new( oStatic,oStatic, {10,10}, {nXsize-40,nYSize-140} )
   oMLE:create()                    // Create multi line edit and
   oMLE:setData( cText )            // assign initial text
   oMLE:helpLink := XbpHelpLabel():new():create( ID_HLP1000 )
   oMLE:helpLink:helpObject := oXbpHelp

   imax := Len( saTopics )          // Transfer help topics 
   FOR i := 1 TO imax               // to combo box. 
      oCombo:AddItem(saTopics[i,1]) 
   NEXT

   oCombo:ItemSelected := ;
       {|mp1, mp2, obj| ShowMarked( obj, oMLE, oXbpHelp ) }

   oXbp := XbpPushButton():New (,, {nXsize-190, B_BOTTOM}, {80, 30} )
   oXbp:pointerFocus := .F.
   oXbp:Caption  := "Ok"   
   oXbp:Create()           
   oXbp:Activate := ;      
   {| mp1, mp2, obj | PostAppEvent( xbeP_Close, mp1, mp2, SetAppWindow() ) }

   oXbp := XbpPushButton():New (,, {nXSize-100, B_BOTTOM} , {80, 30} )
   oXbp:pointerFocus :=.F.
   oXbp:Caption  := "Help"
   oXbp:Create()
   oXbp:Activate := { || oXbpHelp:ShowHelpIndex( ) }

   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
     nEvent := AppEvent( @mp1, @mp2, @oXbp, 0 )
      IF nEvent == xbeK_ESC
         EXIT
      ELSEIF oXbp <> NIL
         oXbp:handleEvent( nEvent, mp1, mp2, oXbp )
      ENDIF
   ENDDO

   oXbpHelp:hide()
RETURN


/*
 * Show marked element from combobox
 */
STATIC PROCEDURE ShowMarked( oCombo, oMle, oXbpHelp )

  LOCAL nItem

  IF Empty( oCombo:XbpListbox:GetData() )
     RETURN
  ENDIF

  nItem := oCombo:XbpListbox:GetData()[1]
  scHelpTopic := saTopics[ nItem,1 ]
  scRtfFile   := saTopics[ nItem,2 ]
  scHelpID    := saTopics[ nItem,3 ]
  snHelpID    := saTopics[ nItem,4 ]

  oXbpHelp:showHelp( snHelpID )
  DisplayRtfFile( oMLE ) 

RETURN


/*
 * Display RTF file in MLE and position buffer to the current Help ID
 * Only the RTF source for the current help window is displayed
 */
STATIC PROCEDURE DisplayRtfFile( oMLE )
   LOCAL nPos, cText := memoread( scRtfFile ) 

   nPos := At( scHelpID, cText )

   DO WHILE SubStr( cText, nPos, 1 ) <> Chr(10)
      nPos --
   ENDDO

   cText := SubStr( cText, nPos + 1 )

   IF ( nPos := At( "\page" , cText ) ) > 0
      cText := Left( cText, nPos + 4 )
   ENDIF

   oMLE:setData( cText )
RETURN
