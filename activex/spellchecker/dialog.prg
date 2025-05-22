//////////////////////////////////////////////////////////////////////
//
//  DIALOG.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   
//  Remarks:
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////

#include "AppEvent.ch"
#include "xbp.ch"

#define BUTTON_HEIGHT 20
#define BUTTON_WIDTH  150

CLASS SpellCheckDialog FROM XbpDialog
PROTECTED
  VAR oSpellChecker
  VAR aSuggestion
  VAR nOffset

  VAR oCheckButton
  VAR oNextButton
  VAR oReplaceButton
  VAR oSuggestions
  VAR oMle
  VAR oHint

  METHOD fillListBox
  METHOD markCurrentWrong

EXPORTED
  METHOD init
  METHOD create
  METHOD destroy
  METHOD showModal

  METHOD resizeXbps

  METHOD checkSpelling
  METHOD nextSuggestion
  METHOD replaceMarked
ENDCLASS

/// <summary>
/// <para>
///   Execute Spellchecking for text that is displayed
///   in the MLE
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:checkSpelling()
  LOCAL cString

  // This is a time consuming operation, thus all buttons
  // are disabled
  ::oCheckButton:disable()
  ::oNextButton:disable()
  ::oReplaceButton:disable()
  ::oSuggestions:disable()

  // if not done yet, instantiate XbpSpellchecker class, or
  // prepare for next usage
  IF NIL == ::oSpellChecker
    ::oSpellChecker := XbSpellChecker():new()
  ELSE
    ::oSpellChecker:reset()
  ENDIF

  cString := ::oMle:getData()

  // When replacing words the word positons handled by
  // Spellchecker class becomes out of sync. self:nOffset
  // is used to correct insert and mark operations
  ::nOffset := 0

  // check the string for syntactical correctness
  ::oSpellChecker:checkSpelling( cString )

  // enable buttons again
  ::oCheckButton:enable()
  ::oNextButton:enable()
  ::oReplaceButton:enable()
  ::oSuggestions:enable()

  // display first error
  ::nextSuggestion()

RETURN self

/// <summary>
/// <para>
///   Show next misspelled word
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:nextSuggestion()

  // gain next suggestion
  ::aSuggestion := ::oSpellChecker:getSuggestion()

  // if there is no suggestion, disable buttons
  // that are not needed
  IF NIL == ::aSuggestion
    ::oNextButton:disable()
    ::oReplaceButton:disable()
    ::oSuggestions:disable()
  ENDIF

  // syncronize the suggestions listbox
  ::fillListBox()

  ::markCurrentWrong()

RETURN self

/// <summary>
/// <para>
///   Replace the marked word with the suggested word
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:replaceMarked()

  LOCAL cWord, nPos, nOrig, nNew, aMarked

  IF NIL == ::aSuggestion
    // nothing to replace
    RETURN self
  ENDIF

  // position of word to mark
  nPos  := ::aSuggestion[2]

  // array of suggested words. ( Allways one element in aMarked )
  aMarked := ::oSuggestions:getData()

  IF Empty( aMarked )
    RETURN self
  ENDIF

  // cWord is word for replacement
  cWord := ::oSuggestions:getItem( aMarked[1] )

  IF Empty( cWord )
    RETURN self
  ENDIF

  // remove marked text and insert the replacement
  ::oMle:cutMarked()
  ::oMle:insert( nPos + ::nOffset, cWord )

  // Correct the offset for next replacement operations.
  // This is necessary because each replacement might change
  // the length of text in the mle
  nOrig := Len( ::aSuggestion[1] )
  nNew := Len( cWord )
  ::nOffset += ( nNew - nOrig )

  // show next misspelled word
  ::nextSuggestion()

RETURN self


/// <summary>
/// <para>
///   Fill listbox wit suggestions.
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:fillListBox()

  LOCAL n, nLen, aSugg, cSugg

  // remove all items in the listbox
  nLen := ::oSuggestions:numItems()
  FOR n := 1 TO nLen
    ::oSuggestions:delItem( 1 )
  NEXT

  IF Empty( ::aSuggestion )
    // if there are no suggestions, we are done
    RETURN self
  ENDIF

  aSugg := ::aSuggestion[3]

  // add new suggestions to the listbox
  nLen := Len( aSugg )
  FOR n := 1 TO nLen
    cSugg :=  aSugg[ n ]
    ::oSuggestions:addItem( cSugg )
  NEXT

  // Mark first item if any.
  nLen := ::oSuggestions:numItems()
  IF 0 < nLen
    ::oSuggestions:setData( 1 )
  ENDIF

RETURN self

/// <summary>
/// <para>
///   Mark word of current suggestion
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:markCurrentWrong()

  LOCAL cWord, nPos, nFrom, nTo

  IF Empty( ::aSuggestion )
    RETURN self
  ENDIF

  // get the word an the position of the word
  cWord := ::aSuggestion[1]
  nPos  := ::aSuggestion[2]

  // set focus to mle to have the words marked
  SetAppFocus( ::oMle )

  // use the current offset to calc where the word
  // currently is
  nFrom := nPos + ::nOffset
  nTo   := nPos + ::nOffset + Len( cWord )

  // After the position is known, mark the word
  ::oMle:setMarked( { nFrom, nTo } )

RETURN self

/// <summary>
/// <para>
///   Initialize the instance
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:init( p, o, pos, size, pp, visible )

  LOCAL oDArea


  ::XbpDialog:init( p, o, pos, size, pp, visible )

  oDArea := ::XbpDialog:drawingArea

  ::oCheckButton := XbpPushButton():new( oDArea )
  ::oCheckButton:caption := "Check Spelling"
  ::oCheckButton:activate := {|| ::checkSpelling() }

  ::oNextButton := XbpPushButton():new( oDArea )
  ::oNextButton:caption  := "Go Next"
  ::oNextButton:activate := {|| ::nextSuggestion() }

  ::oReplaceButton := XbpPushButton():new( oDArea )
  ::oReplaceButton:caption := "Replace"
  ::oReplaceButton:activate := {|| ::replaceMarked() }

  ::oSuggestions := XbpListBox():new( oDArea )

  ::oMle := XbpMle():new( oDArea )

  ::oHint := XbpStatic():new( oDArea )
  ::oHint:setColorBg( GraMakeRGBColor( { 255, 255, 255 } ) )
  ::oHint:options := XBPSTATIC_TEXT_WORDBREAK
  ::oHint:caption := "Enter some text into the edit field. Then push " + ;
                     "'Check Spelling' TO search for misspelled words. " + ;
                     "Use the 'Go Next' and 'Replace' buttons for inserting " +;
                     "alternative spellings that are shown in the listbox"

  ::resize := {| aOld, aNew, obj | ::resizeXbps( self:drawingArea ) }

RETURN self

/// <summary>
/// <para>
///   Repositioning of the contained XbaseParts according
///   the dialog size
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:resizeXbps( oArea )
  LOCAL nTotX, nTotY, nMleX, nTmp

  IF NIL == oArea
    RETURN self
  ENDIF

  nTotX := oArea:currentSize()[1]
  nTotY := oArea:currentSize()[2]
  nMleX := nTotx - BUTTON_WIDTH

  ::oMle:setPos( {0, 0} )
  ::oMle:setSize( {nMleX, nTotY} )

  nTmp := nTotY - BUTTON_HEIGHT
  nTmp -= Int( BUTTON_HEIGHT / 2 )
  ::oCheckButton:setPos( { nMleX, nTmp } )
  ::oCheckButton:setSize( { BUTTON_WIDTH, BUTTON_HEIGHT } )

  nTmp := nTmp - BUTTON_HEIGHT - Int( BUTTON_HEIGHT / 2 )
  ::oNextButton:setPos( { nMleX, nTmp } )
  ::oNextButton:setSize( { BUTTON_WIDTH, BUTTON_HEIGHT } )

  nTmp := nTmp - ( 7 * BUTTON_HEIGHT ) - Int( BUTTON_HEIGHT / 2 )
  ::oSuggestions:setPos( { nMleX, nTmp } )
  ::oSuggestions:setSize( { BUTTON_WIDTH, 7 * BUTTON_HEIGHT } )

  nTmp := nTmp - BUTTON_HEIGHT - Int( BUTTON_HEIGHT / 2 )
  ::oReplaceButton:setPos( { nMleX, nTmp } )
  ::oReplaceButton:setSize( { BUTTON_WIDTH, BUTTON_HEIGHT } )

  nTmp := nTmp - 8 * BUTTON_HEIGHT - Int( BUTTON_HEIGHT / 2 )
  ::oHint:setPos( { nMleX, nTmp } )
  ::oHint:setSize( { BUTTON_WIDTH, 8 * BUTTON_HEIGHT } )

RETURN self

/// <summary>
/// <para>
///   Create the dialog resources
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:create( p, o, pos, size, pp, visible )

  ::XbpDialog:create( p, o, pos, size, pp, visible )

  ::XbpDialog:minSize := { BUTTON_WIDTH, BUTTON_HEIGHT }

  ::oMle:create()
  ::oCheckButton:create()
  ::oNextButton:create()
  ::oNextButton:disable()
  ::oReplaceButton:create()
  ::oReplaceButton:disable()
  ::oSuggestions:create()
  ::oHint:create()

  ::resizeXbps()

RETURN self

/// <summary>
/// <para>
///   destroy the dialog resources
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:destroy()
  ::XbpDialog:destroy()

  IF NIL != ::oSpellChecker
    ::oSpellChecker:destroy()
  ENDIF

RETURN self

/// <summary>
/// <para>
///   Show dialog in modal mode
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
METHOD SpellCheckDialog:showModal()

  LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL

  nEvent := 0
  DO WHILE nEvent <> xbeP_Close
     nEvent := AppEvent( @mp1, @mp2, @oXbp )
     oXbp:handleEvent( nEvent, mp1, mp2 )
  ENDDO

RETURN self

/// <summary>
/// <para>
///   Overload AppSys. An own dialog is used
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
PROCEDURE AppSys()
  Set Charset TO Ansi
RETURN

/// <summary>
/// <para>
///   The main procedure
/// </para>
/// </summary>
/// <returns>
///   self
/// </returns>
PROCEDURE main()

  LOCAL oDlg

  // Instantiate the dialog
  oDlg := SpellCheckDialog():new( AppDesktop(),, { 100, 100 }, { 400, 600 } )
  oDlg:create()

  // Set it as application window
  SetAppWindow( oDlg )

  // Show it
  oDlg:showModal()

  // and kill it.
  oDlg:destroy()

RETURN
