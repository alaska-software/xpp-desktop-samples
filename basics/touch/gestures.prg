//////////////////////////////////////////////////////////////////////
//
//  GESTURES.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This example demonstrates how touch gestures can be processed
//      in an application. To do this, an image which is displayed in
//      the main window is changed with respect to the input of the
//      user. This allows the image be zoomed with the finger, for 
//      example.  
//   
//      The sample only has limited functionality without a touch-
//      capable digitizer. In this case, the image which is displayed
//      can only be moved using the mouse.
//   
//////////////////////////////////////////////////////////////////////

#include "Xbp.ch"
#include "Gra.ch"
#include "Appevent.ch"

#PRAGMA LIBRARY( "xppui2.lib" )

#define SURFACE_BGCOLOR    XBPSYSCLR_3DLIGHT

#define xbeP_NewImage      (xbeP_User+1)

/*
 * Last position of the mouse pointer;
 * used to compute rate of of change 
 * for mouse pointer movement
 */
STATIC aMousePos  := {0,0}
STATIC lMouseDown := .F.

PROCEDURE AppSys
/*
 * Desktop remains application window
 */
RETURN



PROCEDURE Main
   LOCAL nEvent, mp1, mp2, oXbp
   LOCAL oDlg, oPS
   LOCAL nSegId := GRA_SEG_NIL
   LOCAL cFile
   LOCAL oBmp

  /*
   * Create dialog window so that touch gestures
   * made in the drawing area of the dialog can
   * be processed via the xbeP_Gesture event.
   * Pen input and pen button feedback is 
   * disabled because pen input is ignored in 
   * this example.
   */
   oDlg := XbpDialog():new( ,, {10,30}, {640,400} )
   oDlg:taskList   := .T.
   oDlg:title      := "Touch Demo"
   oDlg:drawingArea:clipChildren := .T.
   oDlg:drawingArea:inputMode := XBP_INPUTMODE_TOUCHGESTURES +;
                                 XBP_INPUTMODE_NOPENFLICKS   +;
                                 XBP_INPUTMODE_NOPENBUTTONFEEDBACK
   oDlg:drawingArea:colorBG   := SURFACE_BGCOLOR
   oDlg:create()
   CenterControl( oDlg )

   /*
    * Add a message to the window title bar
    * if no touch-capable input device is 
    * found
    */
   IF IsTouchEnabled() == .F.
      oDlg:SetTitle( oDlg:getTitle() + " [No touch-capable input device found]" )
   ENDIF

  /*
   * Create presentation space for drawing
   * into the application window
   */
   oPS  := XbpPresSpace():new()
   oPS:create( oDlg:drawingArea:winDevice() )

  /*
   * Assign a :paint code block for automatic
   * display of the image. This is done via
   * a graphic segment which contains all the 
   * required primitives. The segment is created
   * implicitly during the first call of the
   * DrawImage(), and the segment just played 
   * back afterwards. The id of the segment is
   * stored in nSegId. However, no output occurs
   * in DrawImage() as long as no image was
   * selected.
   */
   oDlg:drawingArea:paint := {|| DrawImage(oDlg, oPS, oBmp, @nSegId) } 

  /*
   * Assign code block for centering the
   * image after resizing the window 
   */
   oDlg:drawingArea:resize:= {|aOld,aNew| CenterImage(oDlg, oPS, aOld, aNew)}

  /*
   * Assign a :gesture code block for processing
   * touch input. Gestures such as zoom or pan
   * change the position or size of the image 
   * displayed by the example.
   */
   oDlg:drawingArea:gesture:= {|mp1,mp2| HandleGesture(oDlg, oPS, mp1, mp2)}

  /*
   * Assign codeblocks for tracking mouse 
   * pointer movement and button presses. 
   * These are required for simulating pan 
   * operations with the left mouse button.
   */
   oDlg:drawingArea:motion := {|mp1,mp2| HandleMotion(oDlg, oPS, mp1, mp2)}
   oDlg:drawingArea:lbDown := {|mp1,mp2| aMousePos:=AClone(mp1), lMouseDown:=.T.}
   oDlg:drawingArea:lbUp   := {|mp1,mp2| lMouseDown:=.F.}

   /*
    * Add a message bar with information
    * about the sample
    */ 
   CreateMsgBar( oDlg )

   oDlg:show()
   SetAppFocus( oDlg )

   /*
    * Post an event for loading the 
    * initial image into the event 
    * queue
    */ 
   PostAppEvent( xbeP_NewImage,,, oDlg )

   DO WHILE nEvent <> xbeP_Close
      nEvent := Appevent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )

     /*
      * Check if the event is our user-
      * defined event for loading a 
      * new image. The event is posted
      * into the event queue if the "New
      * Image..." button is selected, 
      * and once more at program startup.
      * Free the old image and graphical
      * segment, if any, and load the new
      * image. Afterwards, invalidate the 
      * window to issue a repaint with 
      * the new image.
      */
      IF nEvent == xbeP_NewImage
         IF oBmp != NIL
           oBmp:destroy()
            oBmp := NIL
         ENDIF
         IF nSegId != GRA_SEG_NIL
            GraSegDestroy( oPS, nSegId )
            nsegId := GRA_SEG_NIL
         ENDIF

         oBmp := LoadImage()
         IF oBmp != NIL
            oDlg:drawingArea:invalidateRect()
         ENDIF
      ENDIF
   ENDDO
RETURN



/*
 * Procedure for processing touch gestures 
 * reported to the application via xbeP_Gesture 
 * events. In this example, the touch gestures 
 * performed by the user affect the position and 
 * dimension of n image displayed in application 
 * indow. To do this, the change or rate of 
 * change reported for each of the manipulational 
 * gestures is applied to the graphic tranform
 * matrix defined for for the presentation space. 
 * Whenever graphical output is performed in a 
 * presentation space, the output is implicitly 
 * subjected to the transform defined in the 
 * matrix. This way, graphical transforms such as 
 * rotation or scale can be easily implemented. 
 * The following gestures are recognized in the 
 * example:
 *
 *   Pan   :  => Translation, see GraTranslate()
 *   Zoom  :  => Scale, see GraScale()
 *   Rotate:  => Rotation, see GraRotate()
 *   2-Finger 
 *   Tap   :  => Resets the image
 */
PROCEDURE HandleGesture( oDlg, oPS, nType, aInfo )
 LOCAL aMat
 LOCAL nTmp

   /*
    * Get active transform matrix 
    */
   aMat := oPS:setGraTransform()

   DO CASE
      /*
       * Zoom gesture: add change which occurred
       * since the last event to the scale factor
       * already defined in the transform matrix
       */
      CASE nType == XBP_GESTURE_ZOOM
         nTmp := (aInfo[XBP_GESTUREINFO_ARGS]-         ;
                  aInfo[XBP_GESTUREINFO_LASTARGS]) /   ;
                 aInfo[XBP_GESTUREINFO_STARTARGS] +1
         GraScale( oPS, aMat,                          ;
                   {nTmp,nTmp},                        ;
                   {oPS:SetPageSize()[1][1]/2,         ;
                    oPS:SetPageSize()[1][2]/2},        ;
                    GRA_TRANSFORM_ADD )
      /* 
       * Rotation gesture: add change which occurred
       * since the last event to the rotation angle
       * already defined in the transform matrix
       */
      CASE nType == XBP_GESTURE_ROTATE
         nTmp := aInfo[XBP_GESTUREINFO_ARGS] - ;
                 aInfo[XBP_GESTUREINFO_LASTARGS]
         GraRotate( oPS, aMat,                 ;
                    nTmp,                      ;
                    {oPS:SetPageSize()[1][1]/2,;
                     oPS:SetPageSize()[1][2]/2},;
                    GRA_TRANSFORM_ADD )
      /* 
       * Pan gesture: add change which occurred
       * since the last event to the translation 
       * already defined in the transform matrix
       */
      CASE nType == XBP_GESTURE_PAN
         GraTranslate( oPS, aMat, ;
                       aInfo[XBP_GESTUREINFO_POS][1]-;
                       aInfo[XBP_GESTUREINFO_LASTPOS][1],;
                       aInfo[XBP_GESTUREINFO_POS][2]-;
                       aInfo[XBP_GESTUREINFO_LASTPOS][2],;
                       GRA_TRANSFORM_ADD )
      /* 
       * Two-finger tap: reset the transform matrix 
       * back to the identity matrix. This resets
       * the position and dimension of the image.
       */
      CASE nType == XBP_GESTURE_TWOFINGERTAP
         aMat := GraInitMatrix()
   ENDCASE

   /*
    * Activate the matrix just computed and
    * redraw the image using the new tranform.
    */
   oPS:setGraTransform( aMat )
   oDlg:drawingArea:handleEvent( xbeP_Paint, NIL, NIL )

RETURN


/*
 * Procedure for processing mouse movement
 * reported to the application via xbeM_Motion
 * events. In this example, moving the mouse
 * with the left button pressed simulates a
 * pan operation. 
 */
PROCEDURE HandleMotion( oDlg, oPS, aPos )

   UNUSED( oDlg )

   IF lMouseDown == .F.
      RETURN 
   ENDIF

   /*
    * Get active transform matrix 
    */
   aMat := oPS:setGraTransform()

   /* 
    * Mouse was moved while the left button
    * was pressed; simulate a pan gesture.
    * Add change which occurred since the 
    * last mouse move to the translation 
    * already defined in the transform matrix
    */
   GraTranslate( oPS, aMat, ;
                 aPos[1]-aMousePos[1],aPos[2]-aMousePos[2],;
                 GRA_TRANSFORM_ADD )
   aMousePos := AClone( aPos )

   /*
    * Activate the matrix just computed and
    * redraw the image using the new tranform.
    */
   oPS:setGraTransform( aMat )
   oDlg:drawingArea:handleEvent( xbeP_Paint, NIL, NIL )

RETURN


/*
 * Procedure for displaying the image passed
 * in <oBmp>. The graphic primitives required
 * for drawing the image are stored in a graphic
 * segment, which can subsequently be played 
 * back as needed. A new segment is created if
 * no valid segment is passed in parameter
 * <nSegId>. The id of the new segment is 
 * returned in <nSegId>, which must be passed
 * per reference. 
 */
PROCEDURE DrawImage( oDlg, oPS, oBmp, nSegId )

 /*
  * Do nothing if the image passed 
  * is empty or invalid
  */
  IF oBmp == NIL .OR. oBmp:xSize == 0
     RETURN
  ENDIF

 /*
  * Check if a valid segment is passed
  * in <nSegId>. Just play the segment
  * and return, if so.
  */
  IF nSegId != GRA_SEG_NIL
     GraSegDraw( oPS, nSegId,, GRA_DRAW_BUFFERED )
     RETURN
  ENDIF

 /*
  * Create a new graphic segment and 
  * assign its id to the nSegId 
  * parameter. Also reset the transform
  * matrix in case another image had
  * been displayed previously. 
  */
  oPS:setGraTransform( GraInitMatrix() )
  nSegId := GraSegOpen( oPS )

 /*
  * Compute the initial size of the 
  * image and draw it into the open
  * segment.
  */
  aSize := oDlg:drawingArea:currentSize()
  aRect := {0,0,oBmp:xSize,oBmp:ySize}

  IF oBmp:xSize > aSize[1]
     aRect[3] := aSize[1]
     aRect[4] := oBmp:ySize * (aSize[1] / oBmp:xSize)
  ENDIF
  IF aRect[4] > aSize[2]
     aRect[3] := aRect[3] * (aSize[2] / aRect[4])
     aRect[4] := aSize[2]
  ENDIF

  aRect[1] := (aSize[1] - aRect[3]) / 2 + 10
  aRect[2] := (aSize[2] - aRect[4]) / 2 + 10
  aRect[3] += aRect[1] - 20
  aRect[4] += aRect[2] - 20

  GraSetColor( oPS, SURFACE_BGCOLOR )
  GraBox( oPS, {-700,-700}, {1400,1400}, GRA_FILL )
  oBmp:draw( oPS, aRect,,, GRA_BLT_BBO_IGNORE )

 /*
  * Close the segment
  */
  GraSegClose( oPS )

RETURN


/*
 * Procedure for centering the image on the
 * screen. To do this, the dimensions passed
 * in <aOldSize> and <aNewSize> are used to 
 * compute the offset that must be applied 
 * to the current position. The translation
 * is done implicitly in the transform
 * matrix used when drawing the image.   
 */
PROCEDURE CenterImage( oDlg, oPS, aOldSize, aNewSize )
 LOCAL aMat

   /*
    * Get active transform matrix 
    */
   aMat := oPS:setGraTransform()

   GraTranslate( oPS, aMat, ;
                 (aNewSize[1]-aOldSize[1]) /2,;
                 (aNewSize[2]-aOldSize[2]) /2,;
                 GRA_TRANSFORM_ADD )

   /*
    * Activate the matrix just computed and
    * redraw the image using the new tranform.
    */
   oPS:setGraTransform( aMat )
   oDlg:drawingArea:handleEvent( xbeP_Paint, NIL, NIL )

RETURN


/*
 * Load a new image from disk 
 */
FUNCTION LoadImage()
 LOCAL oBmp
 LOCAL oXbp
 LOCAL cFile

  /*
   * Display a file dialog for selecting
   * the image to be displayed in the
   * window. 
   */
   oXbp := XbpFileDialog():New():Create()
   oXbp:fileFilters := { {"Image Files", "*.JPG;*.JPEG;*.BMP;*.GIF"},;
                         {"All Files",   "*.*"}}
   cFile := oXbp:Open()

   IF Empty(cFile) == .T.
      RETURN NIL
   ENDIF

   oBmp := Xbpbitmap():New():Create()
   oBmp:loadFile( cFile )
   IF oBmp:xSize == 0
      MsgBox( "Error opening image file." )
      QUIT
   ENDIF

RETURN oBmp


/*
 * Create a message bar at the top of the window
 * with information about the example. The bar
 * also contains a push button for loading a  
 * new image
 */
PROCEDURE CreateMsgBar( oDlg )
 LOCAL oBar
 LOCAL aSize := oDlg:drawingArea:currentSize()
 LOCAL aPP   := {{XBP_PP_BGCLR,XBPSYSCLR_3DHIGHLIGHT}}
 LOCAL oBtn

   oBar := XbpStatic():new( oDlg:drawingArea,,,, aPP )
   oBar:caption := "Supported Gestures: Rotate, Zoom, Pan, Two-Finger Tap"
   oBar:options := XBPALIGN_HCENTER + XBPALIGN_VCENTER
   oBar:layoutAlign := XBPLAYOUT_LEFT + XBPLAYOUT_TOP + XBPLAYOUT_RIGHT
   oBar:create( ,, {0,aSize[2]-30}, {aSize[1],30} )

   oBtn := XbpPushButton():new( oBar )
   oBtn:caption := "New Image..."
   oBtn:layoutAlign := XBPLAYOUT_RIGHT + XBPLAYOUT_TOP
   oBtn:create( ,, {aSize[1]-95,5}, {90,20} )

  /*
   * Assign a codeblock for posting a user-defined
   * event to the event queue. The event triggers
   * the loading of a new image.
   */
   oBtn:activate:= {|| PostAppEvent(xbeP_NewImage,,, oDlg)}

RETURN

// EOF
