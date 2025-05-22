//////////////////////////////////////////////////////////////////////
//
//  PREVIEW.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This program contains the XbpPreview class. It displays printer
//      output WYSIWYG as a preview inside a window. Display can be
//      zoomed and scrolled.
//   
//  Remarks:
//      An XbpDialog window is created in the Main procedure. It is used
//      as parent for an XbpPreview object. Pushbuttons for zooming and
//      printing are added. Display of the printer output is done in
//      the DrawDB() function which creates a simple listing of a database.
//   
//////////////////////////////////////////////////////////////////////


#include "Appevent.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"

PROCEDURE AppSys()
// Desktop remains application window
RETURN

PROCEDURE Main
   LOCAL nEvent, mp1, mp2
   LOCAL oDlg, oXbp, drawingArea, oView

    // Quit if there are no installed printers.
   IF XbpPrinter():list() == NIL
      MsgBox( "Error - There are no printers installed!", "Preview Sample" )
      QUIT
   ENDIF

   SET DEFAULT TO "..\..\data\misc"
   USE Cars NEW

   oDlg := XbpDialog():new( SetAppWindow(), , {10,10}, {400,410}, , .F.)
   oDlg:border    := XBPDLG_RAISEDBORDERTHICK_FIXED
   oDlg:title     := "Preview Dialog"
   oDlg:minButton := .F.
   oDlg:maxButton := .F.
   oDlg:taskList  := .T.
   oDlg:create()

   drawingArea := oDlg:drawingArea
   drawingArea:setFontCompoundName( "8.Helv" )

   oView := XbpPreview():new( drawingArea, , {0,0}, {270,380} )
   oView:drawBlock := {|oPS, nPageNo, nNumRows| FormatPage(oPS, nPageNo, @nNumRows) }
   oView:create()

   oXbp := XbpPushButton():new( drawingArea, , {283,325}, {100,30} )
   oXbp:caption := "Zoom ++"
   oXbp:create()
   oXbp:activate := {|| oView:zoom( oView:zoomFactor + 0.25 ) }

   oXbp := XbpPushButton():new( drawingArea, , {283,275}, {100,30} )
   oXbp:caption := "Zoom - -"
   oXbp:create()
   oXbp:activate := {|| oView:zoom( oView:zoomFactor - 0.25 ) }

   oXbp := XbpPushButton():new( drawingArea, , {283,223}, {100,30} )
   oXbp:caption := "Print"
   oXbp:create()
   oXbp:activate := {|| oView:print() }

   oXbp := XbpPushButton():new( drawingArea, , {283,171}, {100,30} )
   oXbp:caption := "OK"
   oXbp:create()
   oXbp:activate := {|| PostAppEvent( xbeP_Close ) }

   oDlg:show()
   SetAppFocus( oXbp )

   nEvent := 0
   DO WHILE nEvent <> xbeP_Close
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO
RETURN



/*
 * Function for formatting a page for the preview. It gets 
 * called via the ":drawBlock" codeblock and gets passed three 
 * parameters. "oPS" is the Presentation Space that is to be 
 * used for rendering. Parameter "nPageNo" specifies the number 
 * of the page that needs to be formatted for previewing. 
 * "nNumRows" specifies the number of rows per page and is used 
 * for positioning the database cursor for pages other than the 
 * first page. For the first page ("nPageNo" == 1), this must be 
 * computed and returned by this function. The record number where 
 * database retrieval is to start from for page numbers greater 
 * than 1 is computed as follows:
 *
 *   nRecNo := ((nPageNo -1) * nNumRows) +1
 *
 * Return is a graphical segment with the graphical primitives 
 * required to render the page.
 *
 * Note on Page Calculation;
 *   The graphics primitives required to render a certain
 *   page are stored in a graphical segment. These are requested
 *   on a per-page basis via evaluation of the ":drawBlock"
 *   codeblock. Later, the segments will be played back in either
 *   the Preview window or on the printer to display the respective
 *   page
 */
FUNCTION FormatPage( oPS, nPageNo, nNumRows )
   LOCAL i, imax   := FCount()
   LOCAL aFields   := DbStruct()
   LOCAL aPageSize := oPS:setPageSize()[1]
   LOCAL aPosX[imax], nY, xValue
   LOCAL nFontWidth, nFontHeight
   LOCAL nSegment

   // Get font metrics of current font
   nFontWidth  := oPS:setFont():width
   nFontHeight := oPS:setFont():height

   aPosX[1]    := nFontWidth/2
   FOR i:=2 TO imax
      aPosX[i] := aPosX[i-1] + nFontWidth/2 + ;
                  Max( Len(aFields[i-1,1]),aFields[i-1,3] ) * nFontWidth
   NEXT

   IF nPageNo == 1
      GO TOP
   ELSE
      GO ((nPageNo -1) * nNumRows) +1
   ENDIF

   // Suppress display of graphics primitives during page
   // calculations
   GraSegDrawMode( oPS, GRA_DM_RETAIN )

   //
   // Format Page
   // 
   // Open new segment 
   nSegment := GraSegOpen( oPS )

   // Draw field names and column separator lines
   nY := aPageSize[2] - 2 * nFontHeight
   FOR i:=1 TO imax
      GraStringAt( oPS, { aPosX[i], nY }, FieldName(i) )
      GraLine( oPS, {aPosX[i]-nFontWidth/4, 0}, {aPosX[i]-nFontWidth/4, aPageSize[2]} )
   NEXT

   // Draw heading separator line
   nY -= nFontHeight/2
   GraLine( oPS, {0, nY}, {aPageSize[1], nY} )

   // Fill page with data from DBF
   DO WHILE nY >= ( nFontHeight + 2 )
      nY -= ( nFontHeight + 2 )

      FOR i:=1 TO imax
         xValue := FieldGet(i)
         DO CASE
            CASE aFields[i,2] == "D"
               xValue := DtoC( xValue )
            CASE aFields[i,2] == "L"
               xValue := IIf( xValue, "T", "F" )
            CASE aFields[i,2] == "M"
               xValue := Left( xValue, 10 )
            CASE aFields[i,2] == "N"
               xValue := Str( xValue )
         ENDCASE

         GraStringAt( oPS, { aPosX[i], nY }, xValue )
      NEXT

      SKIP      
      IF Eof()
         EXIT
      ENDIF
   ENDDO

   IF nPageNo == 1
      nNumRows := RecNo() -1
   ENDIF

   // Close segment and reset segment drawing mode
   GraSegClose( oPS )
   
   GraSegDrawMode( oPS, GRA_DM_DRAWANDRETAIN )

RETURN nSegment



/*
 * The preview class declaration
 */
CLASS XbpPreview FROM XbpStatic
   PROTECTED:
   VAR oView                      // XbpDialog used as display area
   VAR oSquare                    // XbpStatic to fill the gap between scroll bars
   VAR oPresSpace                 // XbpPresSpace
   VAR aViewPort                  // View port for zoom
   VAR aPageSize                  // Printer page size
   VAR aCharAttr                  // Attributes for strings (GRA_AS_BOX)
   VAR aAreaAttr                  // Attributes for areas   (GRA_AA_COLOR)
   VAR nPagePos                   // Current page position
   VAR nRows                      // Number of rows per page

   METHOD setOrigin               // Set origin for the view port
   METHOD calcScrollBox           // Calculate size of scroll box inside scroll bars
   
   METHOD startDoc                // Open spooler
   METHOD endDoc                  // Close spooler
   
   EXPORTED:
   VAR hScroll    READONLY        // Horizontal scroll bar
   VAR vScroll    READONLY        // Vertical   scroll bar
   VAR oPagePos                   // Static for displaying current page position                               
   VAR oNextPage                  // Button for skipping to next page   
   VAR oPrevPage                  // Button for skipping to previous page   
                                  ** Configuration
   VAR font                       // XbpFont
   VAR drawBlock                  // Code block calls function that draws
   VAR printer                    // XbpPrinter
   VAR zoomFactor                 // Zoom factor from 1 to n
   VAR segments                   // Array containing segment IDs
                                  ** Life cycle
   METHOD init                    //
   METHOD create                  //
   METHOD destroy                 //
                                  ** Display
   METHOD setSize                 // Define size
   METHOD hScroll                 // Scroll horizontally
   METHOD vScroll                 // Scroll vertically
   METHOD zoom                    // Zoom
   METHOD presSpace               // Retrieve pesentation space
   METHOD setViewPort             // Define size of view port
   METHOD drawPage                // Render a single page
                                  ** Printing
   METHOD print                   // Output pages to printer

   METHOD numPages                // Determine number of pages
   METHOD pageNo                  // Determine number of current page
   METHOD drawNextPage            // Render next page
   METHOD drawPrevPage            // Render previous page
ENDCLASS



/*
 * Initialize object
 */
METHOD XbpPreview:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpStatic:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::xbpStatic:type  := XBPSTATIC_TYPE_RAISEDRECT

   ::oView           := XbpDialog():new( self )

   ::vScroll         := XbpScrollbar():new( self )
   ::vScroll:type    := XBPSCROLL_VERTICAL
   ::vScroll:cargo   := 0
   ::vScroll:autoTrack := .F.

   ::oPagePos        := XbpPushButton():new( self )
   ::oPagePos:activate := { || MsgBox("Skip to Page n (not implemented)") }

   ::oNextPage       := XbpPushButton():new( self )
   ::oNextPage:caption := "+"
   ::oNextPage:activate := { || ::drawNextPage() }
   ::oPrevPage       := XbpPushButton():new( self )
   ::oPrevPage:caption := "-"
   ::oPrevPage:activate := { || ::drawPrevPage() }

   ::hScroll         := XbpScrollbar():new( self )
   ::hScroll:type    := XBPSCROLL_HORIZONTAL
   ::hScroll:cargo   := 0
   ::hScroll:autoTrack := .F.

   ::oSquare         := XbpStatic():new( self )
   ::oSquare:type    := XBPSTATIC_TYPE_RECESSEDBOX

   // Initialize Presentation Space for the preview.
   // To ensure proper text display, mode "high text
   // precision" is explicitly set
   ::oPresSpace      := XbpPresSpace():new()
   ::oPresSpace:mode := XBPPS_MODE_HIGH_PRECISION
   
   ::zoomFactor      := 1
   ::segments        := {}
   ::nPagePos        := 1
   ::nRows           := 0

   // Use default printer
   ::printer         := XbpPrinter():new()

RETURN self



/*
 * Request system resources
 */
METHOD XbpPreview:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   // XbpStatic is parent for all other XBPs
   ::xbpStatic:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   // Create the printer
   IF ::printer:status() == XBP_STAT_INIT
      ::printer:create()
   ENDIF

   // XbpDialog is displayed without title bar
   ::oView:minButton := .F.
   ::oView:maxButton := .F.
   ::oView:sysMenu   := .F.
   ::oView:titleBar  := .F.
   ::oView:border    := XBPDLG_THINBORDER
   ::oView:create()

   ::oPagePos:create()
   ::oPagePos:setFontCompoundName( "7.Arial" )
   ::oPagePos:paint  := { || ::oPagePos:setCaption( AllTrim(Str(::pageNo())) +;
                             "/" + AllTrim(Str(::numPages())) ) }

   ::oNextPage:create()
   ::oNextPage:setFontCompoundName( "7.Arial" )
   ::oPrevPage:create()
   ::oPrevPage:setFontCompoundName( "7.Arial" )

   // Create scroll bars and XbpStatic to fill the gap
   ::oSquare:create()
   ::hScroll:create()
   ::vScroll:create()

   ::hScroll:scroll := {|mp1| ::hScroll( mp1 ) }
   ::vScroll:scroll := {|mp1| ::vScroll( mp1 ) }

   // Size of XBPs is calculated in the :setSize() method
   ::setSize( ::currentSize() )

   // Calculate page size and deduct margins that can not be printed on
   // Unit is 1/10 mm
   aSize       := ::printer:paperSize()
   ::aPageSize := { aSize[5]-aSize[3], aSize[6]-aSize[4] }

   // Link presentation space with printer device context
   // The PS is created with the metric unit 1/10 mm
   ::oPresSpace:create( ::printer, ::aPageSize, GRA_PU_LOMETRIC )

   IF ::font == NIL
      // Use default font -> small fixed font
      ::font := XbpFont():new( ::oPresSpace )
      ::font:familyName := "Courier New"
      ::font:nominalPointSize := 8
      ::font:generic := .T.
      ::font:create()
   ENDIF

   // Associate Presentation Space with Window Device Context
   ::oPresSpace:configure( ::oView:drawingArea:winDevice() )

   // Erase drawing using a filled white box on repaint
   ::aCharAttr := Array( GRA_AS_COUNT )
   ::aAreaAttr := Array( GRA_AA_COUNT )
   ::aAreaAttr [ GRA_AA_COLOR ] := GRA_CLR_WHITE
   ::oPresSpace:setAttrArea( ::aAreaAttr )

   ::oPresSpace:setFont( ::font )

   // Terminate application if no codeblock was assigned
   IF ValType( ::drawBlock ) != "B"
      MsgBox( ":drawBlock is invalid!" )
      QUIT
   ENDIF

   // Evaluate codeblock in ":drawBlock" to have it format 
   // the first page. This must call a routine that does the 
   // drawing, see notes on "FormatPage()"
  AAdd( ::segments, Eval(::drawBlock, ::oPresSpace, 1, @::nRows) )
  ASize( ::segments, ::numPages() )

   // Initialize view port
   aSize       := ::oView:drawingArea:currentSize()
   ::aViewPort := { 0, 0, aSize[1], aSize[2] }
   ::oView:drawingArea:paint := {|| ::drawPage() }
   ::zoom()

RETURN self



/*
 * Release system resources
 */
METHOD XbpPreview:destroy
   ::xbpStatic:destroy()
   ::aViewPort := ;
   ::aPageSize := ;
   ::font      := NIL
RETURN self



/*
 * Change size of XbpStatic and contained XBPs
 */
METHOD XbpPreview:setSize( aSize )

   ::xbpStatic:setSize( aSize )
   aSize := ::currentSize()

   ::oPagePos:setPos ( { 0, 0 } )
   ::oPagePos:setSize( { 40, 16 } )
   ::oNextPage:setPos ( { 40, 0 } )
   ::oNextPage:setSize( { 20, 16 } )
   ::oPrevPage:setPos ( { 60, 0 } )
   ::oPrevPage:setSize( { 20, 16 } )

   ::hScroll:setPos ( { 80, 0 } )
   ::hScroll:setSize( { aSize[1]-16-80, 16 } )
   ::vScroll:setPos ( { aSize[1]-16, 16 } )
   ::vScroll:setSize( { 16, aSize[2]-16 } )
   ::oSquare:setPos ( { aSize[1]-16,  0 } )
   ::oSquare:setSize( { 16, 16 } )
   ::oView  :setPos ( { 0 , 16 } )
   ::oView  :setSize( { aSize[1]-16, aSize[2]-16 } )

RETURN self



/*
 * Scroll horizontally
 */
METHOD XbpPreview:hScroll( mp1 )
   LOCAL nScrollPos := mp1[1]
   LOCAL nCommand   := mp1[2]
   LOCAL nScroll

   IF (nScrollPos == 0                               .AND. ;
       ::hScroll:getData() == 0)                     .OR.  ;
      (nScrollPos == ::hScroll:setRange()[2]         .AND. ;
       ::hScroll:getData() == ::hScroll:setRange()[2] ) 
       RETURN self
   ENDIF

   DO CASE
   CASE nCommand == XBPSB_PREVPOS
      nScroll = ::hScroll:getData() - ::hScroll:setScrollBoxSize()
      ::hScroll:setData( Max(nScroll, 0) )     
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_NEXTPOS
      nScroll = ::hScroll:getData() + ::hScroll:setScrollBoxSize()
      ::hScroll:setData( Min(nScroll, ::hScroll:setRange()[2]) )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_PREVPAGE
      ::hScroll:setData( nScrollPos )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_NEXTPAGE
      ::hScroll:setData( nScrollPos )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_ENDTRACK
      ::hScroll:setData( nScrollPos )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   ENDCASE

RETURN self



/*
 * Scroll vertically
 */
METHOD XbpPreview:vScroll( mp1 )
   LOCAL nScrollPos := mp1[1]
   LOCAL nCommand   := mp1[2]
   LOCAL nScroll   

   IF (nScrollPos == 0                               .AND. ;
       ::vScroll:getData() == 0)                     .OR.  ;
      (nScrollPos == ::vScroll:setRange()[2]         .AND. ;
       ::vScroll:getData() == ::vScroll:setRange()[2] ) 
       RETURN self
   ENDIF

   DO CASE
   CASE nCommand == XBPSB_PREVPOS
      nScroll = ::vScroll:getData() - ::vScroll:setScrollBoxSize()
      ::vScroll:setData( Max(nScroll, 0) )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_NEXTPOS
      nScroll = ::vScroll:getData() + ::vScroll:setScrollBoxSize()
      ::vScroll:setData( Min(nScroll, ::vScroll:setRange()[2]) )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_PREVPAGE
      ::vScroll:setData( nScrollPos )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_NEXTPAGE
      ::vScroll:setData( nScrollPos )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   CASE nCommand == XBPSB_ENDTRACK
      ::vScroll:setData( nScrollPos )
      ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )
   ENDCASE

RETURN self



/*
 * Set origin of the view port after scrolling
 */
METHOD XbpPreview:setOrigin( nX, nY )
   LOCAL aSize       := ::oView:drawingArea:currentSize()
   LOCAL nViewWidth  := aSize[1]
   LOCAL nViewHeight := aSize[2]
   LOCAL nZoomWidth  := nViewWidth  * ::zoomFactor
   LOCAL nZoomHeight := nViewHeight * ::zoomFactor

   // X origin is always <= 0
   nX := - Abs( nX )

   IF nZoomWidth <= nViewWidth
      nX := 0
   ELSEIF nViewWidth - nX >= nZoomWidth
      nX := nViewWidth - nZoomWidth
   ENDIF

   ::aViewPort[1] := nX
   ::aViewPort[3] := nX + nZoomWidth

   // Y origin is always <= 0
   // Consider the difference between visible and total height
   nY := Abs( nY ) - ( nZoomHeight - nViewHeight )

   IF nY > 0 .OR. nZoomHeight <= nViewHeight
      nY := 0
   ELSEIF nViewHeight - nY >= nZoomHeight
      nY := nViewHeight - nZoomHeight
   ENDIF

   ::aViewPort[2] := nY
   ::aViewPort[4] := nY + nZoomHeight

   ::setViewPort( ::aViewPort )
   ::drawPage()

RETURN self



/*
 * Calculate size of the scroll box and range for both scroll bars
 */
METHOD XbpPreview:calcScrollBox()
   LOCAL aSize       := ::oView:drawingArea:currentSize()
   LOCAL nViewWidth  := aSize[1]
   LOCAL nViewHeight := aSize[2]
   LOCAL nZoomWidth  := aSize[1] * ::zoomFactor
   LOCAL nZoomHeight := aSize[2] * ::zoomFactor

   // Calculation for horizontal scroll bar
   ::hScroll:setRange( { 0, Int(nZoomWidth - nViewWidth) } )
   IF nZoomWidth > nViewWidth
      ::hScroll:setScrollBoxSize( (nZoomWidth-nViewWidth) * (nViewWidth/nZoomWidth) )
   ELSE
      ::hScroll:setScrollBoxSize( nViewWidth )
   ENDIF

   // Calculation for vertical scroll bar
   ::vScroll:setRange( { 0, Int(nZoomHeight - nViewHeight) } )
   IF nZoomHeight > nViewHeight
      ::vScroll:setScrollBoxSize( (nZoomHeight-nViewHeight) * (nViewHeight/nZoomHeight) )
   ELSE
      ::vScroll:setScrollBoxSize( nViewHeight )
   ENDIF

RETURN self



/*
 * Zoom view port
 */
METHOD XbpPreview:zoom( nZoomFactor )
   LOCAL aSize  := ::oView:drawingArea:currentSize()

   DEFAULT nZoomFactor TO ::zoomFactor

   ::zoomFactor := Max( 1, nZoomFactor )

   // Re-calculate view port
   aSize[1] := Int( aSize[1] * nZoomFactor )
   aSize[2] := Int( aSize[2] * nZoomFactor )

   ::aViewPort  := {0,0,aSize[1],aSize[2]}
   ::calcScrollBox()

   // Set origin for view port
   ::setOrigin( ::hScroll:getData(), ::vScroll:getData() )

RETURN self



/*
 * Get presentation space
 */
METHOD XbpPreview:presSpace
RETURN ::oPresSpace



/*
 * Define view port
 */
METHOD XbpPreview:setViewPort( aViewPort )
   LOCAL aOldViewPort := ::oPresSpace:setViewPort()

   IF Valtype( aViewPort ) == "A"
      ::aViewPort := aViewPort
      ::oPresSpace:setViewPort( aViewPort )
   ENDIF
RETURN aOldViewPort



/*
 * Display page specified
 */
METHOD XbpPreview:drawPage( nPageNo )
   
   DEFAULT nPageNo TO ::nPagePos 
   
   IF Len(::segments) == 0
      RETURN self
   ENDIF

   // Drawing is erased by a box filled with white color
   GraBox( ::oPresSpace, {0,0}, ::aPageSize, GRA_FILL )

   IF nPageNo != 0 .AND. nPageNo <= ::numPages()
      IF ::segments[nPageNo] == NIL
         ::segments[nPageNo] := Eval( ::drawBlock,  ;
                                      ::oPresSpace, ;
                                      nPageNo,      ;
                                      @::nRows )
      ENDIF
      
      IF ::segments[nPageNo] != 0      
         GraSegDraw( ::presSpace(), ::segments[nPageNo] )
      ENDIF
   ENDIF

RETURN self



/*
 * Start printing
 * - The presentation space gets associated with a printer device context
 */
METHOD XbpPreview:startDoc

   ::oPresSpace:configure( ::printer )

   // From now on output in :oPresSpace is redirected to the spooler
   ::printer:startDoc()

   // Font and attributes are lost due to :configure() -> reset both
   ::oPresSpace:setFont( ::font )
   ::oPresSpace:setAttrArea( ::aAreaAttr )
   ::oPresSpace:setAttrString( ::aCharAttr )

RETURN self



/*
 * End printing
 * - The presentation space gets associated with the window device context again
 */
METHOD XbpPreview:endDoc

   // Close spooler
   ::printer:endDoc()

   // Re-link PS to window device
   ::oPresSpace:configure( ::oView:drawingArea:winDevice() )

   // Font and attributes are lost due to :configure() -> reset both
   ::oPresSpace:setFont( ::font )
   ::oPresSpace:setAttrArea( ::aAreaAttr )
   ::oPresSpace:setAttrString( ::aCharAttr )

   // Set view port for window
   ::zoom()

RETURN self



/*
 * Output all pages to the printer
 */
METHOD XbpPreview:print()
//  LOCAL i
   
   IF Len(::segments) == 0   
      RETURN self
   ENDIF

   ::startDoc()

   // Uncomment the following code to enable 
   // printing of the whole database. In this
   // sample, only the current page is printed
   ::drawPage( ::nPagePos )

   // FOR i:=1 TO ::numPages()
   //   ::drawPage( i )
   //
   //   IF i +1 <= ::numPages()
   //      ::printer:newPage()
   //   ENDIF
   // NEXT

   ::endDoc()

RETURN self



/*
 * Determine number of pages
 */
METHOD XbpPreview:numPages()
   LOCAL nPages
   
   IF ::nRows == 0
      nPages := 1
   ELSE
      nPages := Int( RecCount() / ::nRows ) + ;
                IIF( RecCount() % ::nRows != 0, 1, 0 )
   ENDIF
RETURN nPages



/*
 * Determine number of current page
 */
METHOD XbpPreview:pageNo()
RETURN ::nPagePos
/*
 * Display next page 
 */
METHOD XbpPreview:drawNextPage()
   IF ::nPagePos +1 <= ::numPages()
      ::nPagePos++
      ::drawPage()
      ::oPagePos:invalidateRect()
   ENDIF
RETURN self



/*
 * Display previous page 
 */
METHOD XbpPreview:drawPrevPage()
   IF ::nPagePos -1 > 0
      ::nPagePos--
      ::drawPage()
      ::oPagePos:invalidateRect()
   ENDIF
RETURN self


