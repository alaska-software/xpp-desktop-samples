//////////////////////////////////////////////////////////////////////
//
//  HTMDEMO.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program demonstrates the usage of an XbpHelp object and    
//      a HTML-based online help.
//   
//      Note: To compile the CHM help file used by this example, the Microsoft
//            HTML Help Workshop must be downloaded from the Microsoft website 
//            (www.microsoft.com, see the download center), and installed on
//            the development machine.
//////////////////////////////////////////////////////////////////////


#include "xbp.ch"     
#include "appevent.ch"

#define B_BOTTOM 15

STATIC  scHtmlFile

STATIC  saTopics     := { ;
        { "Using sub-sections"       , "html\contents.htm" }, ;
        { "First entry"              , "html\entry1.htm"   }, ;
        { "Second entry"             , "html\entry2.htm"   }, ;
        { "Third entry"              , "html\entry3.htm"   }, ;
        { "Formatting text"          , "html\tag_char.htm" }, ;
        { "Paragraphs"               , "html\tag_p.htm"    }, ;
        { "Headings"                 , "html\tag_h.htm"    }, ;
        { "Unordered list"           , "html\tag_ul.htm"   }, ;
        { "Ordered list"             , "html\tag_ol.htm"   }, ;
        { "Definition list"          , "html\tag_dl.htm"   }, ;
        { "Table"                    , "html\tag_tbl.htm"  }, ;
        { "Define margins"           , "html\tag_mgn.htm"  }, ;
        { "Combobox"                 , "html\xbp_help.htm#combobox"  }, ;
        { "Using colors"             , "html\tag_clr.htm"  }, ;
        { "Display image"            , "html\tag_img.htm"  }, ;
        { "Hypertext links"          , "html\tag_lnk.htm"  }, ;
        { "Jumps"                    , "html\tag_jmp.htm"  }  };


PROCEDURE Main
   LOCAL  nEvent, mp1, mp2, oXbp
   LOCAL  oOk, oQuit, oHelp, oXbpHelp, oMLE, oCombo, oStatic
   LOCAL  cText, nXsize, nYsize, i, imax

   SetColor ( "N/W" )
   CLS
   SetMouse(.T.)

   nXsize := SetAppWindow():xSize
   nYsize := SetAppWindow():ySize

   oXbpHelp := XbpHelp():New( , "htmdemoe.chm",  "Windows HTML Help Demo" )
   oXbpHelp:Create()

   oStatic := XbpStatic():New ( , , {10, 60}, {nXsize-20, nYsize-70} )
   oStatic:Type := XBPSTATIC_TYPE_RAISEDBOX
   oStatic:Caption := ""
   oStatic:Create()

  /*
   * Create Combobox as a child of oStatic, which
   * is used as a visual container
   */
   oCombo := XbpCombobox():New ( oStatic, oStatic, ;
                                 {10, nYsize-85-200}, {nXsize-40, 200} )
   oCombo:Type := XBPCOMBO_DROPDOWNLIST
   oCombo:Create()
   oCombo:helpLink := XbpHelpLabel():new( "html\xbp_help.htm#Combobox" ):create()
   oCombo:helpLink:helpObject := oXbpHelp

   cText := "Demo program for online help" +Chr(13)+Chr(10) + ;
            "The major part of the HTML source code" +Chr(13)+Chr(10) + ;
            "is displayed in this window once the online help is activated"

  /*
   * Create MLE as a child of oStatic 
   */
   oMLE := XbpMLE():new( oStatic,oStatic, {10,10}, {nXsize-40,nYSize-140} )
   oMLE:create()                    // Create multi line edit and
   oMLE:setData( cText )            // assign initial text
   oMLE:helpLink := XbpHelpLabel():new( "html\xbp_help.htm#Mle" ):create()
   oMLE:helpLink:helpObject := oXbpHelp


   imax := Len( saTopics )          // Transfer help topics 
   FOR i := 1 TO imax               // to combo box. 
      oCombo:AddItem(saTopics[i,1]) 
   NEXT

   oCombo:ItemSelected := ;
       {|mp1, mp2, obj| ShowMarked( obj, oMle, oXbpHelp ) }

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
   oXbp:Activate := { || IF ( oXbpHelp:ShowHelp( scHtmlFile ),, HtmlError() ) }

   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
     nEvent := AppEvent( @mp1, @mp2, @oXbp, 0 )
      IF nEvent == xbeK_ESC
         EXIT
      ELSEIF oXbp <> NIL
         oXbp:handleEvent( nEvent, mp1, mp2, oXbp )
      ENDIF
   ENDDO

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
  scHtmlFile := saTopics[ nItem, 2 ]
  IF oXbpHelp:ShowHelp( scHtmlFile ) == .F.
     HtmlError()
  ENDIF
  oMLE:setData( memoread( scHtmlFile ) )

RETURN


/*
 * Error when HTML help is not installed
 */
STATIC PROCEDURE HtmlError()

   MsgBox ( "No HTML Help installed", "Error" )
   QUIT

RETURN
