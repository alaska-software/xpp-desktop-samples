//////////////////////////////////////////////////////////////////////
//
//  FEED.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   This sample demonstrates how to load and modify a Microsoft
//   Word template, and then write it to the hard disc an MS Word 
//   document.
//   
//  Remarks:
//   The template contains bookmarks that are replaced with 
//   textual content.
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////


#include "activex.ch"

//////////////////////////////////////////////////////////////////////
// Searches for a bookmark cBM by name in a given bookmark 
// collection oBM, and assigns a new value cValue.
//////////////////////////////////////////////////////////////////////
FUNCTION ReplaceBookmark(oBM,cBM,cValue)
  LOCAL lRet := oBM:Exists(cBM)
  LOCAL oF
  IF(lRet)
    ? "Replace", cBM, "with", cValue
    oF := oBM:Item(cBM)
    oF:Range:Text := cValue
    oF:Destroy()
  ENDIF
RETURN(lRet)


//////////////////////////////////////////////////////////////////////
// Open a MS Word document and replace the bookmarks with the values 
// passed in the array aData. The resulting document is then saved 
// back to the hard drive.
//////////////////////////////////////////////////////////////////////
FUNCTION WordFillDocument(cFile,aData,cSaveAs,lPrint)
  LOCAL oWord,oBM,oDoc

  // Create a Word ActiveX component
  oWord := CreateObject("Word.Application")
  IF Empty( oWord )
    MsgBox( "Microsoft Word is not installed" )
  ENDIF

  oWord:visible := .T.

  // Open a Word document and retrieve the bookmarks 
  // collection.
  oWord:documents:open( cFile )
  oDoc := oWord:ActiveDocument
  oBM  := oDoc:Bookmarks

  // Replace the Bookmark with a new value
  ReplaceBookmark(oBM , "COMPANY"     , aData[1] )
  ReplaceBookmark(oBM , "TO"          , aData[2] )
  ReplaceBookmark(oBM , "FAX"         , aData[3] )
  ReplaceBookmark(oBM , "FROM"        , aData[4] )
  ReplaceBookmark(oBM , "TOTAL_PAGES" , "1" )
  ReplaceBookmark(oBM , "CARBON_COPY" , "" )
  ReplaceBookmark(oBM , "SUBJECT"     , aData[5] )
  ReplaceBookmark(oBM , "SALUTATION"  , aData[6] )
  ReplaceBookmark(oBM , "TEXT"        , aData[7] )
  ReplaceBookmark(oBM , "DATE"        , DToC(Date()) )

  // Save the resulting Word document
  IF(ValType(cSaveAs)=="C")
    oDoc:saveas(cSaveAs)
  ENDIF

  // Optional print out of document to standard 
  // printer
  IF(ValType(lPrint)=="L" .AND. lPrint)
    oDoc:PrintOut()
  ENDIF

  // Close the document and destroy the ActiveX 
  // object
  oDoc:close()
  oWord:Quit()
  oWord:destroy()
RETURN NIL


//////////////////////////////////////////////////////////////////////
// Main program entry point of the application
//////////////////////////////////////////////////////////////////////
PROCEDURE main
  LOCAL cDir,cFile, cSave
  LOCAL aData := {}

  AAdd( aData, "Alaska Software Inc." )
  AAdd( aData, "Technical Support" )
  AAdd( aData, "06196 779999-23" )
  AAdd( aData, "A customer" )
  AAdd( aData, "ActiveX with MS-Word" )
  AAdd( aData, "Dear Sirs," )
  AAdd( aData, "We need urgently a solution NOW, not later than yesterday!" )

  // Determine fully-qualified path for 
  // loading the word template document
  cDir := CurDrive()+":\"+CurDir()
  cFile := cDir +"\Fax.dot"
  cSave := cDir+"\Saved.doc"

  // Helper function:
  // - loads existing word document
  // - uses the array aData to feed in data 
  //   into bookmarks
  // - saves the word document under a new 
  //   name
  WordFillDocument(cFile,aData,cSave,.F.)

  ? "Document", cSave, "created"
  WAIT

RETURN
