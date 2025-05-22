//////////////////////////////////////////////////////////////////////
//
//  GROUPMGR.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Class GroupManager for managing the position of grouped Xbase Parts in
//      sizeable dialog windows. All XBPs of one group have the same size
//   
//  Remarks:
//      A GroupManager object calculates a centered position for the
//      XBP group across a given distance, applies an offset and provides
//      for a variable space between each XBP in the group.
//      Positioning can be done in horizontal or vertical direction.
//   
//      Example for horizontal arrangement:
//   
//      #    ->  distance for centering
//      o    ->  offset in x direction
//      |PB| ->  Pushbuttons
//      -    ->  Variable space between pushbuttons
//   
//      A) Small distance available
//   
//                  ################
//            o      |PB|-|PB|-|PB|
//   
//      B) Large distance available
//   
//                  ####################################
//            o             |PB|----|PB|----|PB|
//   
//////////////////////////////////////////////////////////////////////



#include "Appevent.ch"
#include "Common.ch"
#include "Gra.ch"
#include "Xbp.ch"


CLASS GroupManager FROM XbpPartHandler
   PROTECTED:
   VAR aXbpSize         // size of single XbasePart
   VAR aMinSize         // min size of all XbaseParts
   VAR aMaxSize         // max size of all XbaseParts
   VAR aXbps            // array with XbaseParts
   VAR aOldPos          // position before :setPos()
   VAR aOldSize         // size before :setSize() 
   METHOD centerH       // center XbaseParts horizontally
   METHOD centerV       // center XbaseParts vertically

   EXPORTED:
   VAR maxDistance      // max. distance between XbaseParts
   VAR currentDistance  // current distance between XbaseParts
   VAR horizontal       // .T. => XbaseParts are displayed horizontally
   
   METHOD init
   METHOD addPart       // adds an XbasePart to :aXbps
   METHOD createButton  // creates an XbpPushbutton
   METHOD createSLE     // creates an XbpSLE
   METHOD createText    // creates an XbpStatic Type Text
   METHOD part          // returns an Xbase Part by ordinal position
   METHOD numParts      // returns the number of XbaseParts in a group
   METHOD setPos        // repositions XbaseParts group to given position
   METHOD setSize       // resizes each XbasePart in group to same size
   METHOD center        // centers all XbaseParts within the group
   METHOD minSize       // returns minimum size of the Part group
   METHOD maxSize       // returns maximum size of the Part group
   METHOD currentPos    // returns current pos of the Part group
   METHOD currentSize   // returns current size of the Part group
ENDCLASS



/*
 * Initialize object 
 */
METHOD GroupManager:init( oParent, oOwner, aSize )

   DEFAULT oParent TO SetAppWindow(), ;
           oOwner  TO oParent

   ::xbpPartHandler:init( oParent, oOwner )
   ::aXbps           := {}
   ::aXbpSize        := AClone( aSize )

   ::currentDistance := 0
   ::maxDistance     := 24
   ::horizontal      := .T.
   ::aOldPos         := {0,0}
   ::aOldSize        := AClone( aSize )
RETURN self




/*
 * Adds a created Xbase Part to the group
 */
METHOD GroupManager:addPart( oXbp )

   AAdd( ::aXbps, oXbp )

   IF ::aMinSize == NIL
      ::aMinSize := Aclone( ::aXbpSize )
      ::aMaxSize := Aclone( ::aXbpSize )
   ELSEIF ::horizontal
      ::aMinSize[1] += ::aXbpSize[1]
      ::aMaxSize[1] += ( ::aXbpSize[1] + ::maxDistance )
   ELSE
      ::aMinSize[2] += ::aXbpSize[2]
      ::aMaxSize[2] += ( ::aXbpSize[2] + ::maxDistance )
   ENDIF

RETURN self



/*
 * Creates a new pushbutton and adds it to the group
 */
METHOD GroupManager:createButton( xCaption, bActivate, lTabstop, lPointerFocus, nGroup )
   LOCAL oButton

   DEFAULT lTabStop      TO .T. , ;
           lPointerFocus TO .T. , ;
           nGroup        TO XBP_NO_GROUP

   oButton              := XbpPushbutton():new( ::setParent(), ::setOwner(), ;
                                                NIL          , ::aXbpSize )
   oButton:activate     := bActivate
   oButton:caption      := xCaption
   oButton:group        := nGroup
   oButton:pointerFocus := lPointerFocus
   oButton:tabStop      := lTabStop
   oButton:clipSiblings := .T.
   oButton:create()

   ::addPart( oButton )

RETURN oButton



/*
 * Creates a new Sle and adds it to the group
 */
METHOD GroupManager:createSLE( xData, nBufferLength, lTabstop, nGroup )
   LOCAL oSle

   DEFAULT nBufferLength TO 32  , ;
           lTabStop      TO .T. , ;
           nGroup        TO XBP_NO_GROUP

   oSle              := XbpSLE():new( ::setParent(), ::setOwner(), ;
                                      NIL          , ::aXbpSize )
   oSle:bufferLength := nBufferLength
   oSle:tabStop      := lTabStop
   oSle:group        := nGroup
   oSle:clipSiblings := .T.
   oSle:create()

   IF Valtype( xData ) == "B"
      oSle:dataLink := xData
      oSle:setData()
   ELSE
      oSle:setData( xData )
   ENDIF  

   ::addPart( oSle )

RETURN oSle



/*
 * Creates a new XbpStatic type text
 */
METHOD GroupManager:createText( cText, nHorizAlign, nVertAlign )
   LOCAL oStatic

   DEFAULT nHorizAlign TO XBPSTATIC_TEXT_LEFT , ;
           nVertAlign  TO XBPSTATIC_TEXT_VCENTER

   oStatic         := XbpStatic():new( ::setParent(), , , ::aXbpSize )
   oStatic:caption := cText
   oStatic:type    := XBPSTATIC_TYPE_TEXT
   oStatic:options := nHorizAlign + nVertAlign
   oStatic:clipSiblings := .T.
   oStatic:create()

   ::addPart( oStatic )

RETURN oStatic
 


/*
 * Returns an XbasePart by ordinal posintion in the group
 */
METHOD GroupManager:part( nIndex )
RETURN ::aXbps[ nIndex ]



/*
 * Returns the number of XbaseParts
 */
METHOD GroupManager:numParts()
RETURN Len( ::aXbps )



/*
 * Sets the position for the "anchor" Part and repositions all other 
 * XbaseParts from there 
 */
METHOD GroupManager:setPos( aPos, lPaint )
   LOCAL aXbpPos, i, nCount := Len( ::aXbps )

   DEFAULT lPaint TO .T.

   IF ::horizontal                // horizontal positioning left->right
      i := 1
   ELSE                           // vertical positioning bottom->top
      i := nCount
   ENDIF
   aXbpPos   := AClone( aPos )
   ::aOldPos := ::currentPos()
   ::aXbps[i]:setPos( aXbpPos, lPaint )

   IF ::horizontal
      FOR i := 2 TO nCount
         aXbpPos[1] += ::currentDistance + ::aXbpSize[1]
         ::aXbps[i]:setPos( aXbpPos, lPaint )
      NEXT 
   ELSE
      nCount --
      FOR i := nCount TO 1 STEP -1
         aXbpPos[2] += ::currentDistance + ::aXbpSize[2]
         ::aXbps[i]:setPos( aXbpPos, lPaint )
      NEXT 
   ENDIF

RETURN self



/*
 * Changes the size of all XbaseParts in the group.
 * After this, the method :center() must be called
 */
METHOD GroupManager:setSize( aSize , lPaint )
   LOCAL aXbps[ ::numParts() ]

   DEFAULT lPaint TO .T.

   ACopy( ::aXbps, aXbps )

   ::aOldSize := ::currentSize()
   ::aXbpSize := aSize
   ::aMinSize := NIL
   ::aXbps    := {}

   AEval( aXbps, {|o| o:setSize( aSize, lPaint ), ::addPart(o) } )

RETURN self



/*
 * Centers the Part group across a given distance and adds an offset 
 */
METHOD GroupManager:center( nWidth, nOffset )

   IF ::horizontal
      ::centerH( nWidth, nOffset )
   ELSE
      ::centerV( nWidth, nOffset )
   ENDIF
RETURN self



/*
 * Centers the group horizontally with variable distance between XbaseParts
 */
METHOD GroupManager:centerH( nWidth, nLeftOffset )
   LOCAL i
   LOCAL aPos  := ::aXbps[1]:currentPos()
   LOCAL aSize := ::currentSize()
   LOCAL nCount:= Len( ::aXbps )
   LOCAL nDist := nWidth - ( nCount * ::aXbpSize[1] )
   LOCAL aRect := { ::aOldPos[1], ;
                    ::aOldPos[2], ;
                    ::aOldPos[1]+::aOldSize[1], ;
                    ::aOldPos[2]+::aOldSize[2]  } 

   DEFAULT nLeftOffset TO  0
   nDist -= nLeftOffSet

  /*
   * Variable distance between XbaseParts from 0 to :maxDistance
   */
   IF nCount > 1
      ::currentDistance := Min( ::maxDistance, Max( 0, nDist / nCount ) )
   ELSE
      ::currentDistance := 0
   ENDIF

   aPos[1] := ( nWidth - ( nCount * ::aXbpSize[1] ) - ;
                         ( ( nCount - 1 ) * ::currentDistance ) ) / 2
   aPos[1] += nLeftOffset
   aPos[1] := Max( aPos[1], nLeftOffset )

   ::aXbps[1]:setPos( aPos,.F. )
   FOR i:=2 TO nCount
      aPos[1] += ::currentDistance + ::aXbpSize[1]
      ::aXbps[i]:setPos( aPos, .F. )
   NEXT   

   aPos     := ::currentPos()
   aRect[1] := Min( aRect[1], aPos[1] )
   aRect[2] := Min( aRect[2], aPos[2] )
   aRect[3] := Max( aRect[3], aPos[1]+aSize[1] )
   aRect[4] := Max( aRect[4], aPos[2]+aSize[2] )

   ::setParent():invalidateRect( aRect )
RETURN self


/*
 * Centers the group vertically with variable distance between XbaseParts
 */
METHOD GroupManager:centerV( nHeight, nBottomOffset )
   LOCAL i
   LOCAL aPos  := ::aXbps[1]:currentPos()
   LOCAL aSize := ::currentSize()
   LOCAL nCount:= Len( ::aXbps )
   LOCAL nDist := nHeight - ( nCount * ::aXbpSize[2] )
   LOCAL aRect := { ::aOldPos[1], ;
                    ::aOldPos[2], ;
                    ::aOldPos[1]+::aOldSize[1], ;
                    ::aOldPos[2]+::aOldSize[2]  } 

   DEFAULT nBottomOffset TO 0
   nDist -= nBottomOffSet

  /*
   * Variable distance between XbaseParts from 0 to :maxDistance
   */
   IF nCount > 1
      ::currentDistance := Min( ::maxDistance, Max( 0, nDist / nCount ) )
   ELSE
      ::currentDistance := 0
   ENDIF

   aPos[2] := ( nHeight - ( nCount * ::aXbpSize[2] ) - ;
                          ( ( nCount - 1 ) * ::currentDistance ) ) / 2
   aPos[2] += nBottomOffset
   aPos[2] := Max( aPos[2], nBottomOffset )

   ATail(::aXbps):setPos( aPos,.F. )
   nCount --

   FOR i:= nCount TO 1 STEP -1
      aPos[2] += ::currentDistance + ::aXbpSize[2]
      ::aXbps[i]:setPos( aPos,.F. )
   NEXT   

   aPos     := ::currentPos()
   aRect[1] := Min( aRect[1], aPos[1] )
   aRect[2] := Min( aRect[2], aPos[2] )
   aRect[3] := Max( aRect[3], aPos[1]+aSize[1] )
   aRect[4] := Max( aRect[4], aPos[2]+aSize[2] )

   ::setParent():invalidateRect( aRect )
RETURN self


/*
 * Position of lower left XbasePart
 */
METHOD GroupManager:currentPos
   LOCAL aPos

   IF ::horizontal
      aPos := ::aXbps[1]:currentPos()
   ELSE
      aPos := Atail(::aXbps):currentPos()
   ENDIF

RETURN aPos



/*
 * Returns the area that is occupied by all XbaseParts 
 */
METHOD GroupManager:currentSize
   LOCAL aPos, aSize
   
   IF Len( ::aXbps ) == 1
      RETURN AClone( ::aXbpSize )      // ** RETURN **

   ELSEIF ::horizontal
      aPos  := ::aXbps[1]:currentPos()
      aSize := ATail( ::aXbps ):currentPos()
   ELSE
      aPos  := ATail( ::aXbps ):currentPos()
      aSize := ::aXbps[1]:currentPos()
   ENDIF    

   aSize[1] += ::aXbpSize[1]
   aSize[2] += ::aXbpSize[2]
   aSize[1] -= aPos[1]
   aSize[2] -= aPos[2]

RETURN aSize



/*
 * The minimum area required to display all XbaseParts of the group
 */
METHOD GroupManager:minSize
RETURN AClone( ::aMinSize )



/*
 * The maximum area required to display all XbaseParts of the group
 */
METHOD GroupManager:maxSize
RETURN AClone( ::aMaxSize )
