//////////////////////////////////////////////////////////////////////
//
//  PRINTOUT.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This sample application demonstrates working with XbpPrinter
//      objects in Xbase++. It opens a dialog with a list of the
//      printers currently installed on the system. From this list,
//      a printer can be selected and configured via the setup
//      dialog. Finally, a test page can be sent to the printer
//      selected.
//   
//      A preview window allows to preview the effects some of the
//      options configurable via the setup dialog have on the printed
//      page prior to printing.
//   
//////////////////////////////////////////////////////////////////////

#include "Xbp.ch"
#include "AppEvent.ch"
#include "Gra.ch"

#include "PrintOut.ch"

#pragma Library( "XppUI2.lib" )

************
/*
 * Main() procedure; standard entry point
 */
PROCEDURE Main()

   LOCAL nEvent := NIL, mp1 := NIL, mp2 := NIL, oXbp := NIL
   LOCAL oPreviewDlg
   LOCAL oPrinter


   /*
    * Return failure if there are no installed printers.
    */
   IF XbpPrinter():list() == NIL
      MsgBox( "Error - There are no printers installed!", "Printers Sample" )

      RETURN
   ENDIF

   /*
    * Create the printer object for the systems
    * default printer.
    */
   oPrinter := XbpPrinter():new()
   oPrinter:create()

   /*
    * Create an instance of the PreviewWindow class
    * and have it display a preview of the test page.
    */
   oPreviewDlg          := PreviewWindow():new():create( AppDeskTop() )
   oPreviewDlg:setPrinter( oPrinter )
   oPreviewDlg:show()

   /*
    * Event loop
    */

   nEvent               := xbe_None

   WHILE nEvent <> xbeP_Quit
      nEvent := AppEvent( @mp1, @mp2, @oXbp )

      IF nEvent == xbeP_Close
          PostAppEvent(xbeP_Quit)
      ENDIF

      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   oPrinter:destroy()

RETURN


************
/*
 * Print the test page on the printer passed in
 * "oPrinter". Note that we will use the drawing
 * routine implemented in the "PreviewWindow"
 * class to do the rendering.
 */
PROCEDURE PrintPreview( oPrinter )

   LOCAL oPreviewDlg


   /*
    * Create an instance of the PreviewWindow class
    * and have it print the test page
    */
   oPreviewDlg          := PreviewWindow():new():create()
   oPreviewDlg:setPrinter( oPrinter )
   oPreviewDlg:printPreview()
   oPreviewDlg:destroy()

RETURN


************
/*
 * Open a preview window to show the test page
 * we would print if the user selected the
 * "Print Testpage" button
 */
PROCEDURE PreviewPage( oPrinter )

   LOCAL oPreviewDlg

   /*
    * Create an instance of the PreviewWindow class
    * and have it display a preview of the test page
    */
   oPreviewDlg          := PreviewWindow():new():create( AppDesktop() )
   oPreviewDlg:setPrinter( oPrinter )
   oPreviewDlg:show()

RETURN


/*
 * Definition of the interface of the "PreviewWindow"
 * class
 */
CLASS PreviewWindow FROM XbpDialog

 EXPORTED:

   METHOD Init
   METHOD Create
   METHOD Destroy

   METHOD PaintPreview
   METHOD Show
   METHOD HandleEvent

   METHOD SetPrinter
   METHOD DrawPreview
   METHOD PrintPreview
   METHOD ShowSetup

   METHOD Layout

   VAR   lPrintIt

 PROTECTED:

   VAR    oPS
   VAR    oPageStatic
   VAR    oPushOptions
   VAR    oPushClose
   VAR    oPushPrint
   VAR    oPrinter

   VAR   aPreviewRect

ENDCLASS



************
/*
 * Init() method; Initializes object members
 */
METHOD PreviewWindow:Init()

   /*
    * Initialize members
    */
   ::aPreviewRect             := NIL
   ::oPrinter                 := NIL

   ::oPageStatic              := XbpStatic():new()
   ::oPageStatic:type         := XBPSTATIC_TYPE_RECESSEDRECT

   ::oPushPrint               := XbpPushButton():new()
   ::oPushPrint:caption       := PRINTPUSH_CAPTION
   ::oPushPrint:activate      := { || ::PrintPreview() }

   ::oPushOptions             := XbpPushButton():new()
   ::oPushOptions:caption     := OPTIONSPUSH_CAPTION
   ::oPushOptions:activate    := { || ::ShowSetup() }

   ::oPushClose               := XbpPushButton():new()
   ::oPushClose:caption       := CLOSEPUSH_CAPTION
   ::oPushClose:activate      := { || PostAppEvent(xbeP_Close,,,self)}

   /* Call base class' Init() method         */
   ::XbpDialog:Init()
   ::title                    := PREVIEW_CAPTION
   ::visible                  := .T.
   ::TaskList                 := .T.
   ::clipChildren             := .T.

   ::oPS                      := XbpPresSpace():new()

RETURN self


************
/*
 * Create() method; allocates resources used by an
 *                  object
 */
METHOD PreviewWindow:Create( oOwner )

   LOCAL aPos, aSize
   LOCAL aSizeDesktop         := AppDesktop():currentSize()


   /*
    * Set defaults for window position and size
    */
    /* Position window at the center of the  */
    /* display                               */
    aPos                     := {(aSizeDesktop[1] - PREVIEWWINDOW_SIZE_X*2) /2,   ;
                                 (aSizeDesktop[2] - PREVIEWWINDOW_SIZE_Y*2) /2}
    aSize                    := {PREVIEWWINDOW_SIZE_X*2, PREVIEWWINDOW_SIZE_Y*2}

    ::minSize := {aSize[1]/2,aSize[2]/2}


   /* Call base class' Create() method       */
   ::XbpDialog:Create( oOwner, , aPos, aSize,, .F. )

   /* Set our own PaintPreview() method into */
   /* the Paint() callback slot of the       */
   /* preview area to automatically refresh  */
   /* the preview graphics                   */
   ::oPageStatic:Paint        := {|aRect| ::PaintPreview(aRect) }

   /* Allocate the preview area rectangle    */
   ::aPreviewRect             := Array( 4 )

   /* Have the resize() codeblock do an auto-*/
   /* matic repositioning of the dialog's    */
   /* child windows                          */
   ::XbpDialog:resize         := { || ::Layout() }

   /*
    * Create the dialog window's child windows
    */
   ::oPageStatic:create( self:drawingArea )
   ::oPushPrint:create( self:drawingArea )
   ::oPushOptions:create( self:drawingArea )
   ::oPushClose:create( self:drawingArea )

   /* Position the child windows             */
   ::Layout()

   /* Create the Presentation Space used for */
   /* rendering the preview image.           */
   ::oPS:create()

RETURN self


************
/*
 * Destroy() method; frees resources used by an
 *                   object
 */
METHOD PreviewWindow:Destroy()

   LOCAL oOwner := ::setOwner()

   /* Destroy Presentation Space created for */
   /* rendering the preview                  */
   ::oPS:destroy()

   /* Set the focus back on the parent window*/
   oOwner:toFront()
   SetAppFocus( oOwner )

   /* Call base class' Destroy() method      */
   ::XbpDialog:destroy()

RETURN self


************
/*
 * Layout() method; Positioning and sizing the child
 *                  windows of the preview dialog.
 */
METHOD PreviewWindow:Layout()

   LOCAL aParentRect          := self:drawingArea:currentSize()
   LOCAL aCurrPos             := Array( 2 )

   aCurrPos                   := {aParentRect[1] - PUSHBUTTON_SIZE_X -  ;
                                  STD_SPACING_X, STD_SPACING_Y}
   ::oPushOptions:setPos( aCurrPos )
   ::oPushOptions:setSize( {PUSHBUTTON_SIZE_X, PUSHBUTTON_SIZE_Y} )

   ::oPushPrint:setPos( {aCurrPos[1],aCurrPos[2]+PUSHBUTTON_SIZE_Y+10} )
   ::oPushPrint:setSize( {PUSHBUTTON_SIZE_X, PUSHBUTTON_SIZE_Y} )

   ::oPushClose:setPos( {aCurrPos[1],aCurrPos[2]+PUSHBUTTON_SIZE_Y*3} )
   ::oPushClose:setSize( {PUSHBUTTON_SIZE_X, PUSHBUTTON_SIZE_Y} )

   ::oPageStatic:setPos( {STD_SPACING_X, STD_SPACING_Y} )
   ::oPageStatic:setSize( {aCurrPos[1]    - STD_SPACING_X *2,           ;
                           aParentRect[2] - STD_SPACING_Y *2} )

   ::oPageStatic:setPos( {STD_SPACING_X, STD_SPACING_Y} )
   ::oPageStatic:setSize( {aCurrPos[1]    - STD_SPACING_X *2,           ;
                           aParentRect[2] - STD_SPACING_Y *2} )

   /* Initialize the preview area rectangle  */
   /* used during invalidation and rendering */
   ::aPreviewRect[1]          := STD_SPACING_X
   ::aPreviewRect[2]          := STD_SPACING_Y 
   ::aPreviewRect[3]          := ::oPageStatic:currentSize()[1] - STD_SPACING_X
   ::aPreviewRect[4]          := ::oPageStatic:currentSize()[2] - STD_SPACING_Y

RETURN



************
/*
 * PaintPreview() method; Painting and refreshing invalidated areas,
 *                        including the preview image
 */
METHOD PreviewWindow:PaintPreview( aRect )

   LOCAL aAreaAttrs           := Array( GRA_AA_COUNT )
   LOCAL aLineAttrs           := Array( GRA_AL_COUNT )
   LOCAL aPageSize
   LOCAL aRectScaled          := Array( 4 )
   LOCAL nMin

   UNUSED( aRect )

   /*
    * Refresh preview image in the preview
    * area
    */
   /* Setup Presentation Space so that       */
   /* rendering is restricted to the preview */
   /* area and we are using printer units    */
   IF ::oPS == NIL .OR. ::oPS:status() <> XBP_STAT_CREATE
      RETURN NIL
   ENDIF
   IF ::oPrinter == NIL .OR. ::oPrinter:status() <> XBP_STAT_CREATE
      RETURN NIL
   ENDIF

   aPageSize            := ::oPrinter:paperSize()
   ::oPS:configure( ::oPageStatic:winDevice(), aPageSize, GRA_PU_LOMETRIC )
   ::oPS:setViewport( {::aPreviewRect[1],  ::aPreviewRect[2], ;
                       ::aPreviewRect[3] + ::aPreviewRect[1], ;
                       ::aPreviewRect[4] + ::aPreviewRect[2]} )
   /* Paint background                       */
   aAreaAttrs[GRA_AA_COLOR]   := GRA_CLR_BLUE
   GraSetAttrArea( ::oPS, aAreaAttrs )
   GraBox( ::oPS, {0, 0}, {aPageSize[1], aPageSize[2]}, GRA_FILL )

   /* Compute rectangle for the page image   */
   /* and reset the viewport. After that, we */
   /* can render to the abstract page as if  */
   /* it was a real printer page             */
   nMin := Min( ::aPreviewRect[4], ::aPreviewRect[3] )

   IF aPageSize[1] > aPageSize[2]
      aRectScaled[4]          := aPageSize[2] * nMin / aPageSize[1]
      aRectScaled[3]          := nMin
   ELSE
      aRectScaled[4]          := nMin
      aRectScaled[3]          := aPageSize[1] * nMin / aPageSize[2]
   ENDIF
   aRectScaled[1]             := ((::aPreviewRect[3] - aRectScaled[3]) / 2) + ;
                                   ::aPreviewRect[1] + STD_SPACING_X   / 2
   aRectScaled[2]             := ((::aPreviewRect[4] - aRectScaled[4]) / 2) + ;
                                   ::aPreviewRect[2] + STD_SPACING_Y   / 2
   aRectScaled[3] += aRectScaled[1] - STD_SPACING_X
   aRectScaled[4] += aRectScaled[2] - STD_SPACING_Y

   ::oPS:setViewport( aRectScaled )

   /* Render the page image                  */
   aAreaAttrs[GRA_AA_COLOR]   := GRA_CLR_WHITE
   aAreaAttrs[GRA_AA_BACKCOLOR] := GRA_CLR_BLACK
   GraSetAttrArea( ::oPS, aAreaAttrs )
   GraBox( ::oPS, {0, 0}, {aPageSize[1], aPageSize[2]}, GRA_OUTLINEFILL )

   /* Draw the page margins                  */
   aLineAttrs[GRA_AL_COLOR]   := GRA_CLR_DARKGRAY
   aLineAttrs[GRA_AL_TYPE]    := GRA_LINETYPE_SHORTDASH
   GraSetAttrLine( ::oPS, aLineAttrs )

   GraLine( ::oPS, {aPageSize[3], 0}, {aPageSize[3], aPageSize[2]} )
   GraLine( ::oPS, {aPageSize[5], 0}, {aPageSize[5], aPageSize[2]} )
   GraLine( ::oPS, {0, aPageSize[4]}, {aPageSize[1], aPageSize[4]} )
   GraLine( ::oPS, {0, aPageSize[6]}, {aPageSize[1], aPageSize[6]} )

   /* Render the image to be printed on the  */
   /* test page into the preview area        */
   ::DrawPreview( ::oPS, ::oPrinter )

RETURN self


************
/*
 * Show() method; displays the preview dialog which was
 *                created so that it is not initially visible
 */
METHOD PreviewWindow:Show()

   /* Set the focus on the preview window    */
   SetAppFocus( self )

RETURN ::XbpDialog:show()



************
/*
 * HandleEvent() method; Handles events for a preview
 *                       dialog
 */
METHOD PreviewWindow:HandleEvent( nEvent, mp1, mp2 )

   DO CASE
      CASE nEvent == xbeP_Close
         ::Destroy()

      OTHERWISE
         /* Have base class process this     */
         /* event                            */
         ::XbpDialog:handleEvent( nEvent, mp1, mp2 )
   ENDCASE

RETURN self



************
/*
 * SetPrinter() method; Set the printer the object should
 *                      display a preview for
 */
METHOD PreviewWindow:SetPrinter( oPrinter )

   ::oPrinter := oPrinter

RETURN



************
/*
 * DrawPreview() method; Renders the image to be printed on the
 *                       test page. All rendering goes to the
 *                       to the Presentation Space passed in "oPS".
 *                       Metrics are determined from the printer object
 *                       in "oPrinter"
 */
METHOD PreviewWindow:DrawPreview( oPS, oPrinter )

#define NUM_BOXES       6
#define LINE_STEPX      5
#define LINE_STEPY      7
#define RGB_INCREMENT   40

   LOCAL aBoxRect             := Array( 4 )
   LOCAL aBoxSize             := Array( 2 )
   LOCAL aLineAttrs           := Array( GRA_AL_COUNT )
   LOCAL aRgb                 := { 20, 20, 20 }
   LOCAL nColorIndex          := oPS:maxColorIndex()
   LOCAL i
   LOCAL nOriginX             := 0
   LOCAL nOriginY             := 0
   LOCAL nLineStepX
   LOCAL nLineStepY
   LOCAL aOutRect             := oPrinter:paperSize()
   LOCAL nRadius              := Min( aOutRect[5] - aOutRect[3], ;
                                      aOutRect[6] - aOutRect[4] ) /3

   /* Check wether the output device is a    */
   /* printer or the preview window; in the  */
   /* window, we need to offset manually in  */
   /* order to dispay the print margins pro- */
   /* perly. For the printer, we have to     */
   /* make sure we won't print ourside the   */
   /* printer's margins, eg. due to rounding */
   /* errors in our calculations             */
   IF oPS:device():className() == "XbpPrinter"
      aOutRect[5] -= 2
      aOutRect[6] -= 2
   ELSE
      nOriginX    := aOutRect[3]
      nOriginY    := aOutRect[4]
   ENDIF

   /* Draw some lines in the background      */
   aLineAttrs[GRA_AL_COLOR]   := GRA_CLR_BLACK
   aLineAttrs[GRA_AL_TYPE]    := GRA_LINETYPE_SOLID
   GraSetAttrLine( oPS, aLineAttrs )

   nLineStepX := (aOutRect[5]-aOutRect[3]) / LINE_STEPX
   nLineStepY := (aOutRect[6]-aOutRect[4]) / LINE_STEPY

   FOR i := nOriginX                           ;
       TO nOriginX + (aOutRect[5]-aOutRect[3]) ;
       STEP nLineStepX
     GraLine( oPS, {i, nOriginX}, {i, nOriginX + (aOutRect[6]-aOutRect[4])} )
   NEXT

   FOR i := nOriginY                           ;
      TO nOriginY + (aOutRect[6]-aOutRect[4])  ;
      STEP nLineStepY
     GraLine( oPS, {nOriginX, i}, {nOriginX + (aOutRect[5]-aOutRect[3]), i} )
   NEXT

   /* Compute the rectangle for the gradient */
   /* boxes                                  */
   aBoxSize[1]          := (aOutRect[5] - aOutRect[3]) / NUM_BOXES
   aBoxSize[2]          := (aOutRect[6] - aOutRect[4]) / 5

   aBoxRect[1]           := nOriginX
   aBoxRect[2]           := nOriginY + aBoxSize[2] *2
   aBoxRect[3]           := aBoxRect[1] + aBoxSize[1]
   aBoxRect[4]           := aBoxRect[2] + aBoxSize[2]

   /* Initialize the color range for the     */
   /* boxes                                  */
   FOR i := 0 TO NUM_BOXES -1
      aRgb[1]           += RGB_INCREMENT
      aRgb[2]           += RGB_INCREMENT
      aRgb[3]           += RGB_INCREMENT

      oPS:setColorIndex( nColorIndex-i, {aRgb[1], aRgb[2], aRgb[3]} )
   NEXT

   FOR i := 1 TO NUM_BOXES
      GraSetColor( oPS, nColorIndex )
      GraBox( oPS, {aBoxRect[1], aBoxRect[2]}, ;
                   {aBoxRect[3], aBoxRect[4]}, ;
              GRA_FILL )

      aBoxRect[1]        += aBoxSize[1]
      aBoxRect[3]        += aBoxSize[1]

      /* Set next color to render with      */
      nColorIndex--
  NEXT

   /* Draw an arc around the page's center   */
   GraSetAttrLine( oPS, aLineAttrs )
   GraArc( oPS, {nOriginX + (aOutRect[5]-aOutRect[3]) /2, ;
                 nOriginY + (aOutRect[6]-aOutRect[4]) /2}, ;
           nRadius )

RETURN



************
/*
 * ShowSetup() method; Open setup dialog for the printer
 *                     object set via the SetPrinter() method
 */
METHOD PreviewWindow:ShowSetup()

   /*
    * Check wether member "::oPrinter" contains
    * a valid XbpPrinter object. Display the
    * object's setup dialog if it does.
    */
   IF ::oPrinter <> NIL .AND. ::oPrinter:status() == XBP_STAT_CREATE
      ::oPrinter:setupDialog()

      /* Refresh the preview area to make    */
      /* changes made to the page's          */
      /* orientation visible, for example    */
      ::invalidateRect( ::aPreviewRect )
   ENDIF

RETURN



************
/*
 * PrintPreview() method; Print the test page to the
 *                        printer set via SetPrinter()
 */
METHOD PreviewWindow:PrintPreview()

   /* Return if the printer is not ready     */
   IF ::oPS == NIL .OR. ::oPS:status() <> XBP_STAT_CREATE
      RETURN NIL
   ENDIF
   IF ::oPrinter == NIL .OR. ::oPrinter:status() <> XBP_STAT_CREATE
      RETURN NIL
   ENDIF

   /*
    * Select the printer and its properties and determine, if you
    * finally want to print or cancel it
    */
   IF XbpPrintDialog():new():create(::drawingArea):display(::oPrinter) == NIL
      ::lPrintIt := .F.
   ELSE
      ::lPrintIt := .T.
   ENDIF

   /* Re-configure the Presentation Space for*/
   /* the printer device in "::oPrinter"     */
   ::oPS:configure( ::oPrinter,                                             ;
                    {::oPrinter:paperSize()[5] - ::oPrinter:paperSize()[3], ;
                     ::oPrinter:paperSize()[6] - ::oPrinter:paperSize()[4]},;
                    GRA_PU_LOMETRIC )

    /*
     * If the XbpPrinterDialog wasn't be canceled, then print the page
     */
    If ::lPrintIt
       /* Output the preview graphics to the     */
       /* printer device                         */
       ::oPrinter:startDoc()
       ::DrawPreview( ::oPS, ::oPrinter )
       ::oPrinter:endDoc()
    ENDIF
RETURN


************
/*
 * Overloaded AppSys() procedure to prevent creation
 * of the default Crt
 */
PROCEDURE AppSys()
RETURN
