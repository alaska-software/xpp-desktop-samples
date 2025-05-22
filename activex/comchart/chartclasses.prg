//////////////////////////////////////////////////////////////////////
//
//  CHARTCLASSES.PRG
//
//  Copyright:
//   Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//   Implementation of two classes. A listbox derived from XbpListBox
//   which collects all producers of CARS.DBF in its class method
//   :initClass().
//   Furthermore a Dialog is implemented here derived from XbpDialog.
//   With that dialog the user can select one ore more producers which
//   will be evaluated and displayed on the chart.
//   
//  Remarks:
//   
//  Syntax:
//   
//  Return:
//   
//////////////////////////////////////////////////////////////////////



#include "xbp.ch"

#include "owcchart.ch"

#define DLG_SIZE {800, 480}
#define CRLF     Chr(13) + Chr(10)

CLASS ProducerListBox FROM XbpListBox

EXPORTED
  CLASS METHOD initClass
  CLASS VAR    aProducers

  //life cycle
  METHOD init
  METHOD create
  METHOD destroy
ENDCLASS

CLASS ChartViewer FROM XbpDialog
PROTECTED
  VAR    oChart
  VAR    oGraph

  METHOD initChart
  METHOD addChartItem
  METHOD setCollumnName
  METHOD updateChart

EXPORTED
  VAR    oProdList
  VAR    oShowButton
  VAR    oImageArea
  VAR    oImage
  VAR    oHint

  METHOD init
  METHOD create
  METHOD destroy

  METHOD resize

  METHOD showChart
ENDCLASS

/// <summary>
/// <para>
/// Collect all PRODUCERS in CARS in class var ::aProducers
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
CLASS METHOD ProducerListBox:initClass()

  // The list of producers are collected in a class var. Thus
  // the list is generated once when the classobject is initialized.
  // The drawback is that when the producers are added or deleted
  // then ::aProducers will not be updated.
  FIELD PRODUCER

  IF NIL != ::aProducers
    RETURN
  ENDIF

  ::aProducers := {}

  // Execute a table scan to find all producers
  DbGoTop()
  DO WHILE !EOF()

    IF 0 == AScan( ::aProducers, PRODUCER )
      AAdd( ::aProducers, PRODUCER )
    ENDIF

    DbSkip()

  ENDDO

  ASort( ::aProducers )

RETURN self


/// <summary>
/// <para>
/// Initialization of class and baseclass
/// </para>
/// <para>
/// Collect all PRODUCERS in CARS
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ProducerListBox:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
  ::XbpListBox:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
RETURN self

/// <summary>
/// <para>
/// Creation of class and baseclass
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ProducerListBox:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  ::XbpListBox:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )

  AEval( ::aProducers, {|cItem| ::addItem(cItem) } )

  ::setData( 1 )

RETURN self

/// <summary>
/// <para>
/// Destruction of class and baseclass
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ProducerListBox:destroy()
  ::XbpListBox:destroy()
RETURN self

//############################################################################

/// <summary>
/// <para>
/// Construction of class and its baseclass
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:init( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
  aSize := aSize
  ::XbpDialog:init( oParent, oOwner, aPos, DLG_SIZE, aPresParam, lVisible )

  // Listbox showing all producers
  ::oProdList := ProducerListBox():new( ::drawingArea )
  ::oProdList:markMode := XBPLISTBOX_MM_MULTIPLE

  // Pushbutton for recalcing the chart
  ::oShowButton := XbpPushButton():new( ::drawingArea )
  ::oShowButton:caption := "Show Chart"
  ::oShowButton:activate  := {|| ::showChart() }

  // Bitmap and static for visualisation of the gif picture
  ::oImage      := XbpBitmap():new()

  ::oImageArea  := XbpStatic():new( ::drawingArea )
  ::oImageArea:setColorBg( GraMakeRGBColor( { 255, 255, 255 } ) )
  ::oImageArea:type := XBPSTATIC_TYPE_BITMAP

  // Help text
  ::oHint      := XbpStatic():new( ::drawingArea )
  ::oHint:setColorBg( GraMakeRGBColor( { 255, 255, 255 } ) )
  ::oHint:options := XBPSTATIC_TEXT_WORDBREAK
  ::oHint:caption := "Choose one or more producers in the listbox " + ;
                     "and click the 'Show Chart' Button."

RETURN

/// <summary>
/// <para>
/// Create the chart dialog
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:create( oParent, oOwner, aPos, aSize, aPresParam, lVisible )
  aSize := aSize
  ::XbpDialog:create( oParent, oOwner, aPos, DLG_SIZE, aPresParam, lVisible )

  ::oProdList:create()
  ::oShowButton:create()
  ::oImageArea:create()
  ::oImage:create()
  ::oHint:create()

  ::maxSize := DLG_SIZE
  ::minSize := DLG_SIZE
  ::setSize(DLG_SIZE)


RETURN self

/// <summary>
/// <para>
/// Destroy the chart dialog
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:destroy()
  ::XbpDialog:destroy()

  IF NIL != ::oChart
    ::oChart:destroy()
  ENDIF
RETURN self

/// <summary>
/// <para>
/// Reposition the XbaseParts after resize occured
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:resize()

  LOCAL nY

  nY := ::drawingArea:currentSize()[2]

  ::oImageArea:setPos( { 150, 0 } )
  ::oImageArea:setSize( DLG_SIZE )

  ::oProdList:setPos(  {10, ny - 300} )
  ::oProdList:setSize( { 140, 300 } )

  nY -= 315
  ::oShowButton:setPos(  {10, ny - 30} )
  ::oShowButton:setSize( { 100, 30 } )

  ::oHint:setPos( {5, 5} )
  ::oHint:setSize( {140, 95} )


RETURN self

/// <summary>
/// <para>
/// Initialize or clear the chart object
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:initChart()

  IF NIL == ::oChart

    // Create and initialize the chart object when this
    // method is called the first time
    ::oChart := CreateObject( "OWC.Chart" )

    // Check whether chart object was successfully created.
    // CreateObject() fails if components are missing, for
    // example.
    IF ::oChart == NIL
       MsgBox( "Error creating Chart component. Please make sure MS" + CRLF +;
               "Office Web Components are installed on your computer.",      ;
               "COM Chart Sample" )
       QUIT
    ENDIF

    ::oChart:HasChartSpaceTitle := .T.
    ::oChart:ChartSpaceTitle:Caption := "Xbase++ Chart Sample"
    ::oChart:ChartSpaceTitle:Font:Bold := .T.

    ::oGraph := ::oChart:Charts:Add()
    ::oGraph:HasLegend := .T.

    ::oGraph:PlotArea:Interior:Color := "LightYellow"

  ELSE

    // Prepare the chart for next usage by deleting
    // all items
    DO WHILE ::oGraph:SeriesCollection:count > 0

      ::oGraph:SeriesCollection:delete( 0 )

    ENDDO

  ENDIF

RETURN self

/// <summary>
/// <para>
/// Set the column name
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:setCollumnName( cCollumn )
  LOCAL oColl
  IF ::oGraph:SeriesCollection:count > 0
    oColl := ::oGraph:SeriesCollection( 0 )
    oColl:SetData( chDimCategories, chDataLiteral, cCollumn )
  ENDIF
RETURN self

/// <summary>
/// <para>
/// Add the chart object
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:addChartItem( cItem, xValue )
  LOCAL nCount, oColl
  ::oGraph:SeriesCollection:Add()
  nCount := ::oGraph:SeriesCollection:Count
  oColl := ::oGraph:SeriesCollection( nCount - 1 )
  oColl:Caption := cItem
  oColl:SetData( chDimValues, chDataLiteral, xValue )
RETURN self

/// <summary>
/// <para>
/// Read CARS.DBF and enter the values of all selected PRODUCERS
/// into the diagram.
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:showChart()

  FIELD SELLPRICE, PRODUCER, BUYPRICE
  LOCAL aSelected, n, nLen, nPos
  LOCAL aProdList := {}, aProd

  ::oShowButton:disable()

  aSelected := ::oProdList:getData()

  // clear/initialize chart object
  ::initChart( )

  // Collect all selected producers
  nLen := Len( aSelected )
  FOR n := 1 TO nLen
    AAdd( aProdList, {::oProdList:getItem( aSelected[n] ), 0, 0, 0} )
  NEXT

  // Determine the totoal Buyprice and SellPrice of all selected
  // producers
  DbGoTop()
  DO WHILE !EOF()

    IF 0 == ( nPos := AScan( aProdList, {|elem| elem[1] = PRODUCER} ) )
      DbSkip()
      loop
    ENDIF

    aProd := aProdList[nPos]

    aProd[2] += BUYPRICE
    aProd[3] += SELLPRICE

    DbSkip()

  ENDDO

  // Evaluate the Profit and Add the chart item
  FOR n := 1 TO nLen
    aProd := aProdList[n]
    aProd[4] := aProd[3] - aProd[2]
    ::AddChartItem( aProd[1], {aProd[2], aProd[3], aProd[4]} )
  NEXT

  // Set the column names
  ::setCollumnName( {"Bye Price", "Sell Price", "Profit"} )

  // Paint chart
  ::updateChart()

  ::oShowButton:enable()
RETURN self

/// <summary>
/// <para>
/// Generate the gif picture and display it
/// </para>
/// </summary>
/// <returns>
/// self
/// </returns>
METHOD ChartViewer:updateChart()

  LOCAL cFile

  cFile := GetTmpFileName()

  // generate the gif picture
  ::oChart:ExportPicture( cFile, "gif", 640, 400 )

  // load the gif to XbpBitmap
  ::oImage:loadFile( cFile )

  // update the XbpStatic
  ::oImageArea:setCaption( ::oImage )

  // remove the temporary file
  FErase( cFile )

RETURN self

/// <summary>
/// <para>
/// Create a unique filename
/// </para>
/// </summary>
/// <returns>
/// cFilename
/// </returns>
FUNCTION GetTmpFileName()

  STATIC nNum := 0
  LOCAL  cFilename := "tmpFile"
  LOCAL  cExt := ".gif"

  nNum++

  DO WHILE File( cFilename + "_" + Alltrim( str( nNum ) ) + cExt )
    nNum++
  ENDDO

RETURN cFilename + "_" + Alltrim( str( nNum ) ) + cExt

// EOF
//////
