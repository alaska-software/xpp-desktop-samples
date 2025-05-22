//////////////////////////////////////////////////////////////////////
//
//  PLAIN.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2023. All rights reserved.         
//  
//  Contents:
//      Sample for HRF
//   
//  Remarks:
//      We will show some simple operations how to produce HTML
//      documents from scratch or from template strings containing 
//      markup.
//   
//  Syntax:
//      PLAIN
//   
//  Return:
//      returns 0 always
//   
//////////////////////////////////////////////////////////////////////


 /* **********************************************************************
  * Force the library to be added to the program
  * */
 #pragma Library( "HRFCLASS.LIB" )

 /* **********************************************************************
  * Our main program
  * */
 PROCEDURE Main

    LOCAL oDocument, oTag

    // A simple hello world HTML document
    ? "A simple hello world HTML document"
    ?
    oDocument := HTMLDocument():New()
    oTag      := HTMLHTMLElement():New( oDocument )
    oTag      := HTMLBodyElement():New( oTag )
    oTag      := HTMLParagraphElement():New( oTag,,, "Hello World" )
    ? oDocument:tohtml() + CHR(13)+CHR(10)
    WAIT
    CLS

    // Now replace the content of the hello world tag
    ? "We replace the content of the hello world tag"
    ?
    oTag:SetContent( "Aeppelwoi schmeckt gut!" )
    ? oDocument:tohtml() + CHR(13)+CHR(10)
    WAIT
    CLS

    // We may also create a document with content only from markup
    ? "We may also create a document with content only from markup"
    ?
    oDocument := HTMLDocument():New(,,, "<P>Hello &nbsp; World</P>" )
    ? oDocument:tohtml() + CHR(13)+CHR(10)
    WAIT
    CLS

    // The document also may be created with the LoadFile() method of the
    // HTMLDocument class
    ? "The document also may be created with the LoadFile() method of the"
    ? "HTMLDocument class"
    ?
    oDocument := HTMLDocument():LoadFile( "plain.htm" )
    ? oDocument:tohtml() + CHR(13)+CHR(10)
    WAIT
    CLS

    // Now we show how to create elements with attributes in a document
    ? "Now we show how to create elements with attributes in a document"
    ?
    oDocument := HTMLDocument():New()
    HTMLAnchorElement():New( oDocument, "a", "name=test1", "My anchor" )
    HRFText():New( oDocument,,, "&nbsp;" )
    HTMLAnchorelement():New( oDocument, "a", "name=test2", "Another anchor" )
    ? oDocument:tohtml() + CHR(13)+CHR(10)
    WAIT
    CLS

 RETURN

 // EOF
