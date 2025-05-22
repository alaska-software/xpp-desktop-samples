//////////////////////////////////////////////////////////////////////
//
//  SLIDINGLIST.PRG
//
//  Copyright:
//      Alaska Software, (c) 2015-2025. All rights reserved.         
//  
//  Contents:
//      This example program demonstrates using the XbpHTMLStyle class
//      for developing custom controls. To do this, HTML and CSS are used
//      to define the visualization of the control, which includes using 
//      transition effects during state changes. In addition, the behavior 
//      of the control is defined, for example, for when certain HTML elements
//      are clicked using the mouse. For this, event handlers are bound to
//      individual HTML elements using the interfaces of the XbpHTMLWindow
//      class. Parts of the code in this file is required for creating an
//      easy-to-use interface for the users of the class, which is comparable
//      to other Xbase Parts.
//   
//////////////////////////////////////////////////////////////////////

#include "appevent.ch"
#include "gra.ch"
#include "xbp.ch"

#pragma library( "xppwui.lib" )

#define xbeP_ItemExpanded  (xbeP_User+1)
#define xbeP_ItemCollapsed (xbeP_User+2)

/// <summary> Class implementing a custom Xbase 
/// Part based on HTML and CSS. The class inherits 
/// its display and management capabilities from 
/// the XbpHTMLWindow class.</summary>
CLASS XbpSlidingList FROM XbpHTMLWindow
 EXPORTED:
   METHOD create()

   /*
    * Members and code blocks for reading 
    * in item information from the data 
    * source
    */
   VAR    workArea

   VAR    itemId
   VAR    itemCaption
   VAR    itemImage
   VAR    itemTexts

   /*
    * Callback code blocks and methods
    * for the events supported by the
    * class
    */
   VAR    itemExpanded
   VAR    itemCollapsed
   METHOD itemExpanded()
   METHOD itemCollapsed()

   METHOD handleEvent()
ENDCLASS


/// <summary> Create (allocate resources for) 
/// the custom control</summary>
/// <remarks>See XbpWindow:create() in the
/// documentation for a description of the
/// parameters of the method</remarks>
METHOD XbpSlidingList:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
 LOCAL _lVisible := ::visible
 LOCAL cHTML
 LOCAL i
 LOCAL j
 LOCAL cItems
 LOCAL cItem
 LOCAL cTexts
 LOCAL cDetails
 LOCAL oElem

   IF ValType(lVisible) == "L"
      _lVisible := lVisible
   ENDIF

   SUPER:create( oParent, oOwner, aPos, aSize, aPP, .F. )

   /*
    * Load the HTML markup for displaying
    * the control from the application
    * resource and successively add the 
    * items with information from the 
    * database. To do this, the general
    * HTML markup is loaded along with
    * the style sheet definition 
    * ("ITEMLIST"). The markup is then
    * complemented with the HTML markup
    * of the individual items as these
    * are read in. For this, HTML 
    * templates ("ITEMTEMPLATE" and
    * "ITEMTEXTTEMPLATE") are loaded 
    * from the resource for generating
    * the item markup via text 
    * substitution.
    *
    * The images in the parts table are
    * first reduced in size (thumbnails)
    * before inserting them into the 
    * HTML markup. This is done for 
    * performance reasons because 
    * otherwise, the images would slow
    * down the transition effects due
    * to their size. If the images 
    * already were in a format suitable
    * for display, these could also be
    * loaded from the application 
    * resource or from disk by specifying
    * a suitable URI.
    */
   cHTML := LoadResource( "ITEMLIST",, "HTML", "neutral" )
   cItems:= ""

   (::workArea)->(DbGoTop())
   i:=1
   DO WHILE (::workArea)->(EOF()) == .F.
      cItem := LoadResource( "ITEMTEMPLATE",, "HTML", "neutral" )
      IF (::workArea)->(RecNo()) == 1
         cItem := StrTran( cItem, "%DEFAULT%", "default" )
      ELSE
         cItem := StrTran( cItem, "%DEFAULT%", "" )
      ENDIF

      cItem := StrTran( cItem, "%ID%", Var2Char(i) )
      cItem := StrTran( cItem, "%ITEMID%",  Eval(::itemID) )
      cItem := StrTran( cItem, "%CAPTION%", Eval(::itemCaption) )
      cItem := StrTran( cItem, "%IMAGE%", ImageToThumbnailBase64(Eval(::itemImage),30) )
      cTexts:= ""
      FOR j:=1 TO Len(::itemTexts)
         cDetails := LoadResource( "ITEMTEXTTEMPLATE",, "HTML", "neutral" )
         cDetails := StrTran( cDetails, "%TEXT1%", ::itemTexts[j][1] )
         cDetails := StrTran( cDetails, "%TEXT2%", Var2Char(Eval(::itemTexts[j][2])) )
         cTexts   += cDetails
      NEXT

      cItem  := StrTran( cItem, "%ITEMTEXTS%", cTexts )
      cItems += cItem
      (::workArea)->(DbSkip())
      i++
   ENDDO

   cHTML := StrTran( cHTML, "%ITEMLIST%", cItems )
   ::HTML := cHTML

   IF _lVisible == .T.
      ::show()
   ENDIF

   /*
    * Register event handlers for items
    * being expanded or collapsed in 
    * the control. This is done by 
    * assigning code blocks to dedicated
    * callback slots provided by the
    * corresponding HTML element. An
    * object reference to the element is 
    * obtained using the element's id.
    *
    * The event handlers map the events 
    * to class-specific event codes and 
    * insert them into the event queue 
    * for standard processing via 
    * AppEvent(), :handleEvent() and
    * dedicated event callbacks defined
    * by the custom class.
    */
   i := 1
   DO WHILE .T.
      oElem := ::childFromId( "item" + Var2Char(i) )
      IF oElem == NIL
         EXIT
      ENDIF

      oElem:expanded  := {|uNil1,uNil2,oItem| ;
                           PostAppEvent(xbeP_ItemExpanded,  oItem, NIL, self)}
      oElem:collapsed := {|uNil1,uNil2,oItem| ;
                           PostAppEvent(xbeP_ItemCollapsed, oItem, NIL, self)}
      i++
   ENDDO
RETURN self


/// <summary> Callback method for the "item
/// expanded" event. The class defines no 
/// standard behavior for this event</summary>
METHOD XbpSlidingList:itemExpanded()
RETURN self


/// <summary> Callback method for the "item
/// collapsed" event. The class defines no 
/// standard behavior for this event</summary>
METHOD XbpSlidingList:itemCollapsed()
RETURN self


/// <summary> Generic event handler of the
/// control. This method calls the callback 
/// methods and callback slots associated 
/// with the events supported by the class.
/// </summary>
/// <param name="nEvent">Numeric identifier
/// of the event</param>
/// <param name="mp1">First event parameter
/// </param>
/// <param name="mp2">Second event parameter
/// </param>
METHOD XbpSlidingList:handleEvent( nEvent, mp1, mp2 )
 LOCAL xRet := self

   DO CASE
      CASE nEvent == xbeP_ItemExpanded
         ::itemExpanded()

         IF ValType(::itemExpanded) == "B"
            ::Eval( ::itemExpanded, mp1, mp2, self )
         ENDIF

      CASE nEvent == xbeP_ItemCollapsed
         ::itemCollapsed()

         IF ValType(::itemCollapsed) == "B"
            ::eval( ::itemCollapsed, mp1, mp2, self )
         ENDIF

      OTHERWISE
         xRet := ::XbpHTMLWindow:handleEvent( nEvent, mp1, mp2 )
   ENDCASE

RETURN xRet


//
/// <summary>Function for creating a smaller
/// version of an image (thumbnail)</summary>
/// <param name="cData">The image data</param>
/// <param name="nPercent">The scale factor, 
/// specified as a percentage value</param>
/// <returns>The thumbnail image as a Base64-
/// encoded data stream</returns>
FUNCTION ImageToThumbnailBase64( cData, nPercent )
 LOCAL oBmpDst
 LOCAL oBmpSrc
 LOCAL oPS
 LOCAL aSize

   oBmpSrc:= XbpBitmap():new():create()
   oBmpSrc:setBuffer( cData )
   IF oBmpSrc:xSize == 0
      RETURN ""
   ENDIF

   aSize  := {oBmpSrc:xSize * (nPercent/100), oBmpSrc:ySize * (nPercent/100)}
   oPS    := XbpPresSpace():new():create()
   oBmpDst:= XbpBitmap():new():create()
   oBmpDst:make( aSize[1], aSize[2], 1, 24 )
   oBmpDst:presSpace( oPS )

   oBmpSrc:draw( oPS, {0,0,aSize[1]-1,aSize[2]-1},,, GRA_BLT_BBO_IGNORE )
RETURN Bin2Base64(oBmpDst:setBuffer(,XBPBMP_FORMAT_PNG,0))

// EOF
