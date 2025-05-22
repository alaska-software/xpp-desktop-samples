//////////////////////////////////////////////////////////////////////
//
//  METAFILE.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      This sample application demonstrates the use of metafiles
//      in Xbase++. An image created using simple GRA primitives
//      is recorded into a metafile and re-displayed at various
//      locations. A copy of the resulting metafile is put into 
//      the clipboard as well as saved to disk.
//   
//////////////////////////////////////////////////////////////////////



#include "Gra.CH"
#include "XBP.CH"

/*
 * Spacing between the display areas
 */
#DEFINE X_SPACING       240
#DEFINE Y_SPACING       160

/*
 * Size of the display areas
 */
#DEFINE X_BOX           200
#DEFINE Y_BOX           120

#ifdef __WIN32__
#define METAFILE_NAME   "testfile.emf"
#endif
#ifdef __OS2__
#define METAFILE_NAME   "testfile.met"
#endif

/*
 * Main procedure
 */
PROCEDURE Main()

   LOCAL oWindowPS          := SetAppWindow():presSpace()
   LOCAL oMetaFilePS
   LOCAL nMaxColorIdx
   LOCAL oClipBoard
   LOCAL oFileDC
   LOCAL oMF
   LOCAL aRect := Array( 4 )
   LOCAL aRectPrev


   /*
    * Render the image to be recorded by the
    * metafile. This is done using simple GRA
    * primitives
    */
   @ MaxRow(), 0 SAY "Recording metafile image ..."

   /* 
    * Create an XbpFileDev object and connect it
    * with a Presentation Space so we can record
    * GRA primitives to construct the image.    
    */
   oFileDC     := XbpFileDev():new():create()
   oMetaFilePS := XbpPresSpace():New():Create( oFileDC, oWindowPS:setPageSize()[1], GRA_PU_PIXEL )

   /* 
    * Generate the image. Note that we'll draw  
    * into the window and the newly created     
    * Presentation Space simultanuously to give 
    * visual feedback that there's something    
    * going on                                  
    */
   RenderImage( oWindowPS, oMetaFilePS )

   /* 
    * Detach the file device and extract the  
    * metafile object with the image recorded   
    */
   oMetaFilePS:configure()
   oMF         := oFileDC:metaFile()
   oFileDC:destroy()

   /*
    * Put a copy of the metafile into the clipboard
    */
   oClipBoard  := XbpClipBoard():new():create()
   oClipBoard:open()

   oClipBoard:setBuffer( oMF )

   oClipBoard:close()
   oClipBoard:destroy()

   /*
    * Play back the metafile to show what we've
    * in the clipboard now.
    * For effects, we'll use the new XBPMETA_DRAW_SCALE
    * option for the draw() method to scale the metafile 
    * to a viewport we set prior to playback.
    */
   /* Save current viewport rectangle for    */
   /* restoring later                        */
   aRectPrev := oWindowPS:setViewport()         
   
   /* Bottom-left corner                     */
   aRect[1] := 0
   aRect[2] := 0
   aRect[3] := X_BOX
   aRect[4] := Y_BOX
   oWindowPS:setViewport( aRect )
   oMF:draw( oWindowPS, XBPMETA_DRAW_SCALE )

   /* Bottom-right corner                    */
   aRect[1] := X_BOX    + X_SPACING
   aRect[2] := 0
   aRect[3] := X_BOX *2 + X_SPACING
   aRect[4] := Y_BOX
   oWindowPS:setViewport( aRect )
   oMF:draw( oWindowPS, XBPMETA_DRAW_SCALE )

   /* Upper-left corner                      */
   aRect[1] := 0
   aRect[2] := Y_BOX    + Y_SPACING
   aRect[3] := X_BOX
   aRect[4] := Y_BOX *2 + Y_SPACING
   oWindowPS:setViewport( aRect )
   oMF:draw( oWindowPS, XBPMETA_DRAW_SCALE )

   /* Upper-right corner                     */
   aRect[1] := X_BOX    + X_SPACING
   aRect[2] := Y_BOX    + Y_SPACING
   aRect[3] := X_BOX *2 + X_SPACING
   aRect[4] := Y_BOX *2 + Y_SPACING
   oWindowPS:setViewport( aRect )
   oMF:draw( oWindowPS, XBPMETA_DRAW_SCALE )

   /* 
    * Reset window's viewport to the previous   
    * settings. This is not strictly necessary  
    * but is considered good style because      
    * other drawing operations might assume a   
    * specific viewport setting                 
    */
   oWindowPS:setViewport( aRectPrev )

   @ MaxRow(), 0 SAY "Done. Please hit a key..."
   INKEY( 0 )

   /*
    * Save a copy of the metafile created to disk
    * and free the object
    */
   /* Erase exising file, else :save() will  */
   /* fail!                                  */
   IF File( METAFILE_NAME )
      FErase( METAFILE_NAME )
   ENDIF

   oMF:save( METAFILE_NAME )
   oMF:destroy()

   CLS
   @ 10, 17 SAY "The image rendered has been recorded and a copy "
   @ 11, 17 SAY "of the resulting metafile has been put into the "
   @ 12, 17 SAY "clipboard. You can paste this into any metafile-"
   @ 13, 17 SAY "capable application. Additionally, a copy of the"
   @ 14, 17 SAY "metafile has been written to disk. You may re-  "
   @ 15, 17 SAY "load it using an XbpMetaFile object. "
   @ MaxRow(), 0 SAY "Finished, please hit a key..."
   INKEY( 0 )

RETURN



/*
 * Render the image to be recorded in the metafile
 * using the Presentation Space passed in
 * "oMetaFilePS". Additionally, we'll draw into
 * the Presentation Space in "oWindowPS". This is
 * just to give visual feedback about what's going
 * on
 */
PROCEDURE RenderImage( oWindowPS, oMetaFilePS )

   LOCAL i
   LOCAL bBlock
   LOCAL nSegment


   /* 
    * Array containing codeblocks with the GRA  
    * primitives to contruct our image with.    
    * This is just for demonstration, you'll    
    * probably choose other means to render 
    * your image.                                    
    */
   aVertices := {  { | oPS | GraPos(  oPS,   { 97, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  { 60, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  { 55, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  { 54, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  { 55, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  { 58, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  { 58, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  { 30, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  { 30, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  { 34, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  { 35, 155} ) },  ;
                   { | oPS | GraLine( oPS,,  { 37, 158} ) },  ;
                   { | oPS | GraLine( oPS,,  { 67, 221} ) },  ;
                   { | oPS | GraLine( oPS,,  { 68, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  { 67, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  { 65, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  { 65, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  { 94, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  { 94, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  { 91, 229} ) },  ;
                   { | oPS | GraLine( oPS,,  { 90, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  { 91, 222} ) },  ;
                   { | oPS | GraLine( oPS,,  {123, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {124, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {126, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {130, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {130, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {100, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {100, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {103, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {104, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {102, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  { 97, 171} ) },  ;
                   { | oPS | GraPos(  oPS,   { 92, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  { 79, 211} ) },  ;
                   { | oPS | GraLine( oPS,,  { 66, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  { 92, 182} ) },  ;
                   { | oPS | GraPos(  oPS,   {159, 164} ) },  ;
                   { | oPS | GraLine( oPS,,  {210, 164} ) },  ;
                   { | oPS | GraLine( oPS,,  {212, 165} ) },  ;
                   { | oPS | GraLine( oPS,,  {214, 168} ) },  ;
                   { | oPS | GraLine( oPS,,  {216, 168} ) },  ;
                   { | oPS | GraLine( oPS,,  {216, 146} ) },  ;
                   { | oPS | GraLine( oPS,,  {214, 146} ) },  ;
                   { | oPS | GraLine( oPS,,  {212, 149} ) },  ;
                   { | oPS | GraLine( oPS,,  {210, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {137, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {137, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {140, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {141, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {141, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {140, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {137, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {137, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {163, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {163, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {160, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {159, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {159, 164} ) },  ;
                   { | oPS | GraPos(  oPS,   {284, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  {247, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  {242, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {241, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {242, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {245, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {245, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {217, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {217, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {221, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {222, 155} ) },  ;
                   { | oPS | GraLine( oPS,,  {224, 158} ) },  ;
                   { | oPS | GraLine( oPS,,  {254, 221} ) },  ;
                   { | oPS | GraLine( oPS,,  {255, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {254, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {252, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {252, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {281, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {281, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {278, 229} ) },  ;
                   { | oPS | GraLine( oPS,,  {277, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {278, 222} ) },  ;
                   { | oPS | GraLine( oPS,,  {310, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {311, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {313, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {317, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {317, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {287, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {287, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {290, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {291, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {289, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {284, 171} ) },  ;
                   { | oPS | GraPos(  oPS,   {279, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  {266, 211} ) },  ;
                   { | oPS | GraLine( oPS,,  {253, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  {279, 182} ) },  ;
                   { | oPS | GraPos(  oPS,   {401, 225} ) },  ;
                   { | oPS | GraLine( oPS,,  {391, 205} ) },  ;
                   { | oPS | GraLine( oPS,,  {390, 206} ) },  ;
                   { | oPS | GraLine( oPS,,  {390, 207} ) },  ;
                   { | oPS | GraLine( oPS,,  {389, 209} ) },  ;
                   { | oPS | GraLine( oPS,,  {386, 212} ) },  ;
                   { | oPS | GraLine( oPS,,  {375, 217} ) },  ;
                   { | oPS | GraLine( oPS,,  {361, 219} ) },  ;
                   { | oPS | GraLine( oPS,,  {350, 217} ) },  ;
                   { | oPS | GraLine( oPS,,  {344, 214} ) },  ;
                   { | oPS | GraLine( oPS,,  {343, 211} ) },  ;
                   { | oPS | GraLine( oPS,,  {342, 209} ) },  ;
                   { | oPS | GraLine( oPS,,  {343, 205} ) },  ;
                   { | oPS | GraLine( oPS,,  {348, 203} ) },  ;
                   { | oPS | GraLine( oPS,,  {352, 202} ) },  ;
                   { | oPS | GraLine( oPS,,  {360, 201} ) },  ;
                   { | oPS | GraLine( oPS,,  {369, 200} ) },  ;
                   { | oPS | GraLine( oPS,,  {377, 199} ) },  ;
                   { | oPS | GraLine( oPS,,  {383, 197} ) },  ;
                   { | oPS | GraLine( oPS,,  {392, 193} ) },  ;
                   { | oPS | GraLine( oPS,,  {399, 185} ) },  ;
                   { | oPS | GraLine( oPS,,  {400, 181} ) },  ;
                   { | oPS | GraLine( oPS,,  {401, 175} ) },  ;
                   { | oPS | GraLine( oPS,,  {400, 167} ) },  ;
                   { | oPS | GraLine( oPS,,  {395, 160} ) },  ;
                   { | oPS | GraLine( oPS,,  {389, 155} ) },  ;
                   { | oPS | GraLine( oPS,,  {380, 151} ) },  ;
                   { | oPS | GraLine( oPS,,  {371, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {363, 149} ) },  ;
                   { | oPS | GraLine( oPS,,  {346, 151} ) },  ;
                   { | oPS | GraLine( oPS,,  {330, 157} ) },  ;
                   { | oPS | GraLine( oPS,,  {328, 158} ) },  ;
                   { | oPS | GraLine( oPS,,  {326, 158} ) },  ;
                   { | oPS | GraLine( oPS,,  {324, 157} ) },  ;
                   { | oPS | GraLine( oPS,,  {322, 158} ) },  ;
                   { | oPS | GraLine( oPS,,  {331, 178} ) },  ;
                   { | oPS | GraLine( oPS,,  {333, 177} ) },  ;
                   { | oPS | GraLine( oPS,,  {333, 176} ) },  ;
                   { | oPS | GraLine( oPS,,  {333, 174} ) },  ;
                   { | oPS | GraLine( oPS,,  {335, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  {340, 168} ) },  ;
                   { | oPS | GraLine( oPS,,  {347, 165} ) },  ;
                   { | oPS | GraLine( oPS,,  {355, 164} ) },  ;
                   { | oPS | GraLine( oPS,,  {363, 163} ) },  ;
                   { | oPS | GraLine( oPS,,  {370, 164} ) },  ;
                   { | oPS | GraLine( oPS,,  {376, 166} ) },  ;
                   { | oPS | GraLine( oPS,,  {381, 169} ) },  ;
                   { | oPS | GraLine( oPS,,  {383, 174} ) },  ;
                   { | oPS | GraLine( oPS,,  {382, 178} ) },  ;
                   { | oPS | GraLine( oPS,,  {379, 180} ) },  ;
                   { | oPS | GraLine( oPS,,  {374, 181} ) },  ;
                   { | oPS | GraLine( oPS,,  {366, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  {357, 183} ) },  ;
                   { | oPS | GraLine( oPS,,  {349, 184} ) },  ;
                   { | oPS | GraLine( oPS,,  {344, 185} ) },  ;
                   { | oPS | GraLine( oPS,,  {336, 187} ) },  ;
                   { | oPS | GraLine( oPS,,  {332, 191} ) },  ;
                   { | oPS | GraLine( oPS,,  {328, 196} ) },  ;
                   { | oPS | GraLine( oPS,,  {326, 201} ) },  ;
                   { | oPS | GraLine( oPS,,  {325, 207} ) },  ;
                   { | oPS | GraLine( oPS,,  {327, 216} ) },  ;
                   { | oPS | GraLine( oPS,,  {332, 224} ) },  ;
                   { | oPS | GraLine( oPS,,  {338, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {344, 231} ) },  ;
                   { | oPS | GraLine( oPS,,  {352, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {361, 233} ) },  ;
                   { | oPS | GraLine( oPS,,  {375, 231} ) },  ;
                   { | oPS | GraLine( oPS,,  {383, 229} ) },  ;
                   { | oPS | GraLine( oPS,,  {392, 225} ) },  ;
                   { | oPS | GraLine( oPS,,  {395, 224} ) },  ;
                   { | oPS | GraLine( oPS,,  {396, 224} ) },  ;
                   { | oPS | GraLine( oPS,,  {399, 225} ) },  ;
                   { | oPS | GraLine( oPS,,  {401, 225} ) },  ;
                   { | oPS | GraPos(  oPS,   {447, 188} ) },  ;
                   { | oPS | GraLine( oPS,,  {435, 178} ) },  ;
                   { | oPS | GraLine( oPS,,  {435, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {436, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {439, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {439, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {413, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {413, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {416, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {417, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {417, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {416, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {413, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {413, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {439, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {439, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {436, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {435, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {435, 197} ) },  ;
                   { | oPS | GraLine( oPS,,  {469, 225} ) },  ;
                   { | oPS | GraLine( oPS,,  {471, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {471, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {471, 229} ) },  ;
                   { | oPS | GraLine( oPS,,  {468, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {468, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {502, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {502, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {499, 229} ) },  ;
                   { | oPS | GraLine( oPS,,  {497, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {494, 225} ) },  ;
                   { | oPS | GraLine( oPS,,  {489, 221} ) },  ;
                   { | oPS | GraLine( oPS,,  {461, 199} ) },  ;
                   { | oPS | GraLine( oPS,,  {493, 161} ) },  ;
                   { | oPS | GraLine( oPS,,  {497, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {499, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {504, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {504, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {472, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {472, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {474, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {475, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {474, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {472, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {447, 188} ) },  ;
                   { | oPS | GraPos(  oPS,   {572, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  {535, 171} ) },  ;
                   { | oPS | GraLine( oPS,,  {530, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {529, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {530, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {533, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {533, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {505, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {505, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {509, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {510, 155} ) },  ;
                   { | oPS | GraLine( oPS,,  {512, 158} ) },  ;
                   { | oPS | GraLine( oPS,,  {542, 221} ) },  ;
                   { | oPS | GraLine( oPS,,  {543, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {542, 228} ) },  ;
                   { | oPS | GraLine( oPS,,  {540, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {540, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {569, 232} ) },  ;
                   { | oPS | GraLine( oPS,,  {569, 230} ) },  ;
                   { | oPS | GraLine( oPS,,  {566, 229} ) },  ;
                   { | oPS | GraLine( oPS,,  {565, 226} ) },  ;
                   { | oPS | GraLine( oPS,,  {566, 222} ) },  ;
                   { | oPS | GraLine( oPS,,  {598, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {599, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {601, 154} ) },  ;
                   { | oPS | GraLine( oPS,,  {605, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {605, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {575, 150} ) },  ;
                   { | oPS | GraLine( oPS,,  {575, 152} ) },  ;
                   { | oPS | GraLine( oPS,,  {578, 153} ) },  ;
                   { | oPS | GraLine( oPS,,  {579, 156} ) },  ;
                   { | oPS | GraLine( oPS,,  {577, 159} ) },  ;
                   { | oPS | GraLine( oPS,,  {572, 171} ) },  ;
                   { | oPS | GraPos(  oPS,   {567, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  {554, 211} ) },  ;
                   { | oPS | GraLine( oPS,,  {541, 182} ) },  ;
                   { | oPS | GraLine( oPS,,  {567, 182} ) } }

   /*
    * Set up the Presentation Spaces passed in.
    * Note that we'll open a path bracket in
    * the metafile PS in order to be able
    * to fill the shapes we are about to render
    */
   GraSetColor( oWindowPS, GRA_CLR_YELLOW )
   GraSetColor( oMetaFilePS, GRA_CLR_BLUE )
   GraPathBegin( oMetaFilePS )

   FOR i := 1 TO Len( aVertices )
      /*
       * Get codeblock with GRA primitive and
       * parameters from array and execute it,
       * once for each PS passed in
       */
      bBlock := aVertices[i]

      Eval( bBlock, oWindowPS )
      Eval( bBlock, oMetaFilePS )

      /* Wait a bit to give the user time to */   
      /* realize there's something going on  */
      Sleep( 1 )
   NEXT

   /*
    * Close the path bracket in the metafile
    * PS and fill it
    */
   GraPathEnd( oMetaFilePS )
   GraPathFill( oMetafilePS )

RETURN
