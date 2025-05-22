//////////////////////////////////////////////////////////////////////
//
//  PCONFIG.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      User defined class XbpPrinterConfig()
//   
//  Remarks:
//      The class displays a modal XbpDialog window which can be used
//      to configure an XbpPrinter object before a print job is started.
//      Configurable values are:
//   
//      - Paper format          => XbpPrinter:setFormSize()
//      - Paper bin             => XbpPrinter:setPaperBin()
//      - Print resolution      => XbpPrinter:setResolution()
//      - Number of copies      => XbpPrinter:setNumCopies()
//      - Color or monochrome   => XbpPrinter:setColorMode()
//      - Print to file         => XbpPrinter:setPrintFile()
//      - Portrait/landscape    => XbpPrinter:setOrientation()
//   
//      The configuration values are obtained from the printer driver and
//      depend on the selected printer!
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"


#include "XbpDev.ch"

/*
 * The code embedded in #ifdef __TEST__ is a test scenario for the
 * XbpPrinterConfig() class. The usage of this class is demonstrated in
 * Main().
 */

#define __TEST__

#ifdef __TEST__

PROCEDURE Main
   LOCAL oDialog, oPrinter

   /*
    * Create the configuration window
    */
   oDialog  := XbpPrinterConfig():new():create()

   /*
    * Create an XbpPrinter object
    */
   oPrinter := XbpPrinter():new():create()
   IF oPrinter:list() == NIL
      MsgBox( "Error - There are no printers installed!", "PConfig Sample" )
      RETURN
   ENDIF

   /*
    * Assign the printer object to the window
    */
   oDialog:setData( oPrinter )

   /*
    * Display the window. It is modal. An event loop runs
    * within :show()
    */
   oDialog:show()

   /*
    * Simple procedure to display the printer configuration data
    */
   DispConfig( oPrinter )

   oDialog:setData( oPrinter )
   oDialog:show()
   DispConfig( oPrinter )
RETURN


/*
 * Display the printer configuration in the XbpCrt window
 */
PROCEDURE DispConfig( oPrinter )
   LOCAL xVal1, xVal2
   LOCAL nIndex

   SetAppFocus( SetAppWindow() )

   xVal1 := oPrinter:setFormSize()
   xVal2 := oPrinter:forms()

   IF xVal1 <> NIL .AND. xVal2 <> NIL
      /*
       * xVal1 = numeric
       * xVal2 = two-dimensional array {{numeric, description},...}
       * The next expression assigns a two element array to xVal2
       * => {numeric, description}
       */
      nIndex := AScan( xVal2, {|a| a[1] == xVal1 } )
      IF nIndex == 0
         nIndex := 1
      ENDIF
      xVal2 := xVal2[ nIndex ]
      ? ":setFormSize()    -> ", xVal2[1], " = ", xVal2[2]
   ENDIF

   xVal1 := oPrinter:setPaperBin()
   xVal2 := oPrinter:paperBins()

   IF xVal1 <> NIL .AND. xVal2 <> NIL
      nIndex := AScan( xVal2, {|a| a[1] == xVal1 } ) 
      IF nIndex == 0
         nIndex := 1
      ENDIF
      xVal2 := xVal2[ nIndex ]
      ? ":setPaperBin()    -> ", xVal2[1], " = ", xVal2[2]
   ENDIF

   xVal1 := oPrinter:setResolution()
   IF xVal1 <> NIL
      ? ":setresolution()  -> ", xVal1
   ENDIF

   xVal1 := oPrinter:setNumCopies()
   IF xVal1 <> NIL
      ? ":setNumCopies()   -> ", xVal1
   ENDIF

   xVal1 := oPrinter:setColorMode()
   IF xVal1 <> NIL
      ? ":setColorMode()   -> ", IIf( xVal1 == XBPPRN_COLORMODE_ON, "ON", "OFF" )
   ENDIF

   xVal1 := oPrinter:setPrintFile()
   IF xVal1 <> NIL
      ? ":setPrintFile()   -> ", xVal1
   ENDIF

   xVal1 := oPrinter:setOrientation()
   IF xVal1 <> NIL
      ? ":setOrientation() -> ", IIf( xVal1 == XBPPRN_ORIENT_PORTRAIT, "XBPPRN_ORIENT_PORTRAIT", "XBPPRN_ORIENT_LANDSCAPE" )
   ENDIF

   WAIT
RETURN

#endif // __TEST__



/*
 * Calculate the centered position for a window
 */
STATIC FUNCTION CenterPos( aSize, aRefSize )
RETURN { Int( (aRefSize[1]-aSize[1]) / 2 ), ;
         Int( (aRefSize[2]-aSize[2]) / 2 )  }


/*
 * Class to configure an XbpPrinter object
 */
CLASS XbpPrinterConfig FROM XbpDialog

   PROTECTED:
   VAR oFocus               // Input focus must be set to this Xbp
                            // when the window is closed
   VAR aForms               // Available paper formats
   VAR aBins                // Available paper bins
   VAR aDPI                 // Available print resolutions

   VAR grpBox1              // Groupbox    Printer settings
   VAR txtForms             // XbpStatic   Text
   VAR forms                // Combobox    Paper format
   VAR txtBins              // XbpStatic   Text
   VAR Bins                 // Combobox    Paper bin
   VAR txtDPI               // XbpStatic   Text
   VAR Dpi                  // Combobox    Print resolution

   VAR grpBox2              // Groupbox    Printjob settings
   VAR txtCopies            // XbpStatic   Text
   VAR numCopies            // Spinbutton  Number of copies
   VAR useColor             // Checkbox    Color/monochrome
   VAR toFile               // Checkbox    Print to file

   VAR grpBox3              // Groupbox    Paper orientation
   VAR portrait             // RadioButton Portrait
   VAR landscape            // Radiobutton Landscape

   VAR btnOK                // Pushbutton  OK
   VAR btnCancel            // PushButton  Cancel

   EXPORTED:
   VAR oPrinter  READONLY   // The XbpPrinter object

   METHOD init              // Life cycle methods
   METHOD create            //

   METHOD show              // Display the dialog window
   METHOD hide              // Hide the window
   METHOD setData           // Assign XbpPrinter object to window
   METHOD getData           // Configure XbpPrinter object and close
                            // the window

ENDCLASS

/*
 * Initialize the object
 */
METHOD XbpPrinterConfig:init( oParent, oOwner )
   LOCAL aPos, aSize, aPP

   DEFAULT oParent  TO AppDesktop(), ;
           oOwner   TO SetAppwindow()

   aPP     := { { XBP_PP_COMPOUNDNAME, FONT_HELV_SMALL } }
   aSize   := {288,333}
   aPos    := CenterPos( aSize, oOwner:currentSize() )
   aPos[1] += oOwner:currentPos()[1]
   aPos[2] += oOwner:currentPos()[2]

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, .F. )

   ::XbpDialog:sysMenu   := .F.
   ::XbpDialog:minButton := .F.
   ::XbpDialog:maxButton := .F.
   ::XbpDialog:border    := XBPDLG_DLGBORDER
   ::XbpDialog:title     := "Printer configuration"
   ::XbpDialog:close     := {|mp1, mp2, obj| obj:hide() }

   ::grpBox1             := XbpStatic():new( ::drawingArea, , {8,192}, {264,112} )
   ::grpBox1:caption     := "Printer settings"
   ::grpBox1:type        := XBPSTATIC_TYPE_GROUPBOX

   ::txtForms            := XbpStatic():new( ::grpBox1, , {8,72}, {80,24} )
   ::txtForms:caption    := "Paper format"
   ::txtForms:options    := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::forms               := XbpCombobox():new( ::grpBox1, , {96,-16}, {160,112} )
   ::forms:type          := XBPCOMBO_DROPDOWNLIST
   ::forms:tabStop       := .T.

   ::txtBins             := XbpStatic():new( ::grpBox1, , {8,40}, {80,24} )
   ::txtBins:caption     := "Paper bin"
   ::txtBins:options     := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::bins                := XbpCombobox():new( ::grpBox1, , {96,-50}, {160,112} )
   ::bins:type           := XBPCOMBO_DROPDOWNLIST
   ::bins:tabStop        := .T.

   ::txtDPI              := XbpStatic():new( ::grpBox1, , {8,8}, {80,24} )
   ::txtDPI:caption      := "Print resolution"
   ::txtDPI:options      := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::dpi                 := XbpCombobox():new( ::grpBox1, , {96,-82}, {160,112} )
   ::dpi:type            := XBPCOMBO_DROPDOWNLIST
   ::dpi:tabStop         := .T.

   ::grpBox2             := XbpStatic():new( ::drawingArea, , {8,104}, {264,80} )
   ::grpBox2:caption     := "Printjob settings"
   ::grpBox2:type        := XBPSTATIC_TYPE_GROUPBOX

   ::txtCopies           := XbpStatic():new( ::grpBox2, , {16,40}, {104,24} )
   ::txtCopies:caption   := "Number of copies"
   ::txtCopies:options   := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_RIGHT

   ::numCopies           := XbpSpinbutton():new( ::grpBox2, , {154,40}, {64,24} )
   ::numCopies:tabStop   := .T.

   ::useColor            := XbpCheckBox():new( ::grpBox2, , {32,8}, {104,24} )
   ::useColor:caption    := "Use colors"
   ::useColor:tabStop    := .T.

   ::toFile              := XbpCheckBox():new( ::grpBox2, , {154,8}, {104,24} )
   ::toFile:caption      := "Print to file"
   ::toFile:tabStop      := .T.

   ::grpBox3             := XbpStatic():new( ::drawingArea, , {8,48}, {264,48} )
   ::grpBox3:caption     := "Paper orientation"
   ::grpBox3:type        := XBPSTATIC_TYPE_GROUPBOX

   ::portrait            := XbpRadiobutton():new( ::grpBox3, , {32,8}, {88,24} )
   ::portrait:caption    := "Portrait"
   ::portrait:tabStop    := .T.
   ::portrait:selection  := .T.

   ::landscape           := XbpRadiobutton():new( ::grpBox3, , {154,8}, {88,24} )
   ::landscape:caption   := "Landscape"
   ::landscape:tabStop   := .T.

   ::btnOK               := XbpPushButton():new( ::drawingArea, , {8,8}, {104,24} )
   ::btnOK:caption       := "Ok"
   ::btnOK:activate      := {|| ::getData(), ::hide() }
   ::btnOK:tabStop       := .T.

   ::btnCancel           := XbpPushButton():new( ::drawingArea, , {168,8}, {104,24} )
   ::btnCancel:caption   := "Cancel"
   ::btnCancel:activate  := {|| ::hide() }
   ::btnCancel:tabStop   := .T.

RETURN self


/*
 * Request system resources
 */
METHOD XbpPrinterConfig:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::grpBox1:create()
   ::txtForms:create()
   ::forms:create()
   ::txtBins:create()
   ::Bins:create()
   ::txtDPI:create()
   ::dpi:create()

   ::grpBox2:create()
   ::txtCopies:create()
   ::numCopies:create()
   ::numCopies:setNumLimits( 1, 99 )
   ::useColor:create()
   ::toFile:create()

   ::grpBox3:create()
   ::portrait:create()
   ::landscape:create()

   ::btnOK:create()
   ::btnCancel:create()
RETURN self


/*
 * Assign XbpPrinter object and transfer the current configuration
 * values to the edit controls (XbaseParts)
 */
METHOD XbpPrinterConfig:setData( oPrinter )
   LOCAL i, imax, xVal
   LOCAL nIndex

   ::oPrinter  := oPrinter
   ::aForms    := oPrinter:forms()
   ::aBins     := oPrinter:paperBins()
   ::aDpi      := oPrinter:resolutions()

   IF ::aForms == NIL
      ::forms:xbpSle:setData( "" )
      ::forms:disable()
   ELSE
      ::forms:enable()
      imax := Len( ::aForms )
      FOR i:=1 TO imax
         ::forms:addItem( ::aForms[i,2] )
      NEXT

      xVal := oPrinter:setFormSize()
      i    := AScan( ::aForms, {|a| a[1] == xVal } )
      IF i != 0
        ::forms:XbpSle:setData( ::aForms[i,2] )
      ENDIF
   ENDIF

   IF ::aBins == NIL
      ::bins:xbpSle:setData( "" )
      ::bins:disable()
   ELSE
      ::bins:enable()
      imax := Len( ::aBins )
      FOR i:=1 TO imax
         ::bins:addItem( ::aBins[i,2] )
      NEXT

      xVal := oPrinter:setPaperBin()
      i    := AScan( ::aBins, {|a| a[1] == xVal } )
      IF i == 0
         i := 1
      ENDIF
      ::bins:XbpSle:setData( ::aBins[i,2] )
   ENDIF

   IF ::aDpi == NIL
      ::dpi:xbpSle:setData( "" )
      ::dpi:disable()
   ELSE
      ::dpi:enable()
      imax := Len( ::aDpi )
      FOR i:=1 TO imax
         ::dpi:addItem( LTrim( Str(::aDpi[i,1]) ) + " x " + LTrim( Str(::aDpi[i,2]) ) + " dpi" )
      NEXT

      xVal := oPrinter:setResolution()
      i    := AScan( ::aDpi, {|a| a[1] == xVal[1] .AND. a[2] == xVal[2] } )
      IF i == 0
         i := 1
      ENDIF
      ::dpi:XbpSle:setData( LTrim( Str(::aDpi[i,1]) ) + " x " + LTrim( Str(::aDpi[i,2]) ) + " dpi" )
   ENDIF

   xVal := oPrinter:setNumCopies()
   IF xVal == NIL
      ::numCopies:setData( 1 )
      ::numCopies:disable()
   ELSE
      ::numCopies:enable()
      ::numCopies:setData( xVal )
   ENDIF

   xVal := oPrinter:setColorMode()
   IF xVal == NIL
      ::useColor:setData( .F. )
      ::useColor:disable()
   ELSE
      ::useColor:enable()
      ::useColor:setData( xVal == XBPPRN_COLORMODE_ON )
   ENDIF

   xVal := oPrinter:setPrintFile()
   IF xVal == NIL
      ::toFile:setData( "" )
      ::toFile:disable()
   ELSE
      ::toFile:enable()
      ::toFile:setData( .NOT. Empty(xVal) )
   ENDIF

   xVal := oPrinter:setOrientation()
   IF xVal == NIL
      ::portrait:setData( .F. )
      ::landscape:setData( .F. )
      ::portrait:disable()
      ::landscape:disable()
   ELSE
      ::portrait:enable()
      ::landscape:enable()
      ::portrait:setData( xVal == XBPPRN_ORIENT_PORTRAIT )
      ::landscape:setData( xVal <> XBPPRN_ORIENT_PORTRAIT )
   ENDIF
RETURN self


/*
 * Transfer the configuration data to the XbpPrinter object
 */
METHOD XbpPrinterConfig:getData()
   LOCAL xVal

   IF ::oPrinter == NIL
      RETURN self
   ENDIF

   IF ::forms:isEnabled()
      xVal := ::forms:getData()
      IF .NOT. Empty( xVal )
         ::oPrinter:setFormSize( ::aForms[ xVal[1], 1 ] )
      ENDIF
   ENDIF

   IF ::bins:isEnabled()
      xVal := ::bins:getData()
      IF .NOT. Empty( xVal )
         ::oPrinter:setPaperBin( ::aBins[ xVal[1], 1 ] )
      ENDIF
   ENDIF

   IF ::dpi:isEnabled()
      xVal := ::dpi:getData()
      IF .NOT. Empty( xVal )
         ::oPrinter:setResolution( ::aDpi[ xVal[1] ] )
      ENDIF
   ENDIF

   IF ::numCopies:isEnabled()
      ::oPrinter:setNumCopies( ::numCopies:getData() )
   ENDIF

   IF ::useColor:isEnabled()
      IF ::useColor:getData()
         ::oPrinter:setColorMode( XBPPRN_COLORMODE_ON )
      ELSE
         ::oPrinter:setColorMode( XBPPRN_COLORMODE_OFF )
      ENDIF
   ENDIF

   IF ::toFile:isEnabled()
      IF ::toFile:getData()
         ::oPrinter:setPrintFile( XBPPRN_FILE_PROMPT )
      ELSE
         ::oPrinter:setPrintFile( "" )
      ENDIF
   ENDIF

   IF ::portrait:isEnabled()
      IF ::portrait:getData()
         ::oPrinter:setOrientation( XBPPRN_ORIENT_PORTRAIT )
      ELSE
         ::oPrinter:setOrientation( XBPPRN_ORIENT_LANDSCAPE )
      ENDIF
   ENDIF
RETURN self


/*
 * Display the window in modal state (event loop!)
 */
METHOD XbpPrinterConfig:show()
   LOCAL nEvent, mp1, mp2, oXbp

   ::oFocus := SetAppFocus()
   ::XbpDialog:show()
   ::setModalState( XBP_DISP_APPMODAL )

   SetAppFocus( ::forms )

   DO WHILE ::isVisible()
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   ::setModalState( XBP_DISP_MODELESS )
RETURN self


/*
 * Hide the window and release configuration data
 */
METHOD XbpPrinterConfig:hide()
   IF ::getModalState() != XBP_DISP_MODELESS
      ::setModalState( XBP_DISP_MODELESS )
   ENDIF
   SetAppFocus( ::oFocus )
   ::XbpDialog:hide()

   ::aForms    := ;
   ::aBins     := ;
   ::aDPI      := ;
   ::oPrinter  := ;
   ::oFocus    := NIL

   ::forms:clear()
   ::bins:clear()
   ::dpi:clear()
RETURN self
