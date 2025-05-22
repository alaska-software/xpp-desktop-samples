//  Function-oriented code created by the Xbase++ FormDesigner
//    Creation date: 05/13/2005 Time: 18:09:35
//
//  Contents  :
//    This file contains the basic structure for the utilization-level of
//    a form. It may (and should) be modified.
//
///////////////////////////////////////////////////////////////////////////////

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#pragma library( "XPPUI2.LIB" )
#pragma library( "ADAC20B.LIB" )

#define CRLF         Chr(10) + Chr(13)

#define HTML_FORMAT  "HTML Format"
#define URL_FORMAT   "UniformResourceLocator"

// Use Internet Explorer for loading unknown data formats?
STATIC bUseIE := .F.

******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _MainDialog class.
******************************************************************************
CLASS MainDialog FROM _MainDialog

   PROTECTED:
      METHOD NewTextDocument()
      METHOD NewGFXDocument()
      METHOD NewDBFDocument()
      METHOD NewIEDocument()

      METHOD ValidateFormat()
      METHOD GetDropData()

   EXPORTED:
      VAR    DropFormats
      VAR    CurrData
      VAR    FileExts

      METHOD Init()
      METHOD Create()

      METHOD ShowStatus()

      METHOD Resize()
      METHOD HandleDragEnter()
      METHOD HandleDragDrop()
      METHOD HandleDragLeave()

      METHOD NewDocument()

ENDCLASS


******************************************************************************
* Initialize form
******************************************************************************
METHOD MainDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   // Call method of base class
   ::_MainDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::MinSize                := {400,200}
   ::Title                  := "Drop Sample Application"

   // Define drop zone and assign code blocks for handling
   // the relevant events
   ::DrawingArea:DropZone   := .T.
   ::DrawingArea:DragEnter  := {|aState,oData| ::HandleDragEnter(aState,oData) }
   ::DrawingArea:DragLeave  := {|aState,uNIL|  ::HandleDragLeave(aState,) }
   ::DrawingArea:DragDrop   := {|aState,oData| ::HandleDragDrop(aState,oData) }

   // Definition of the formats and file extensions
   // supported for drag-and-drop by this application
   ::FileExts               := {{".LOG", "TEXT"  },;
                                {".TXT", "TEXT"  },;
                                {".PRG", "TEXT"  },;
                                {".XPJ", "TEXT"  },;
                                {".CH",  "TEXT"  },;
                                {".ME",  "TEXT"  },;
                                {".GIF", "BITMAP"},;
                                {".JPG", "BITMAP"},;
                                {".JPEG","BITMAP"},;
                                {".PNG", "BITMAP"},;
                                {".BMP", "BITMAP"},;
                                {".EMF", "EMF"   },;
                                {".HTM", "HTML"  },;
                                {".XML", "HTML"  },;
                                {".URL", "HTML"  },;
                                {".HTML","HTML"  },;
                                {".DBF", "DBF"   }}

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD MainDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

 LOCAL aSizeSB
 LOCAL aSizeDA
 LOCAL oPanel

   // Call method of base class
   ::_MainDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   // Define statusbar object as a child of
   // the dialog. This prevents MDI child
   // windows from overlapping the statusbar
   // object
   ::Statusbar:Setparent( self )
   aSizeSB := ::Statusbar:CurrentSize()
   aSizeDA := ::DrawingArea:Currentsize()
   ::DrawingArea:SetPosAndSize( {0,aSizeSB[2]}, {aSizeDA[1],;
                                 aSizeDA[2]-aSizeSB[2]} )

   // Add a statusbar panel for displaying
   // the current time
   ::Statusbar:AddItem(,,, XBPSTATUSBAR_PANEL_TIME )
   oPanel := ::Statusbar:GetItem( 1 )
   oPanel:autoSize := XBPSTATUSBAR_AUTOSIZE_SPRING

   ::ShowStatus( "" )

RETURN self


******************************************************************************
* Display status message
******************************************************************************
METHOD MainDialog:ShowStatus( cMsg )

   IF Empty(cMsg) == .T.
      cMsg := "Ready - Please drop a file or an item of another application."
   ENDIF

   ::Statusbar:GetItem(1):Caption := cMsg 

RETURN


******************************************************************************
* Arrange child objects after dialog was resized
******************************************************************************
METHOD MainDialog:Resize( aOldSize, aNewSize )

 LOCAL aSize   := ::Statusbar:CurrentSize()
 LOCAL aDASize := ::CalcClientRect( {0,0,aNewSize[1],aNewSize[2]} )

   aDASize := {aDASize[3] - aDASize[1],aDASize[4] - aDASize[2]}
   ::Statusbar:SetSize( {aDASize[1],aSize[2]} )

RETURN self


******************************************************************************
* Process "xbeP_DragEnter" notification. The procedure checks whether the
* item dropped represents data of a format supported by the application.
* For this, the associated "DragDataObject" is used. 
******************************************************************************
METHOD MainDialog:HandleDragEnter( aState, oData )

 LOCAL nReturn  := XBP_DROPMODE_NONE
 LOCAL cMsg     := "(Unsupported Item Format)"

    UNUSED( aState )

    ::CurrData := ::ValidateFormat( oData )
    IF Len(::CurrData) > 0
       cMsg    := "(Item Format Supported)"
       nReturn := XBP_DROPMODE_COPY
    ENDIF

    ::ShowStatus( cMsg )

RETURN nReturn



******************************************************************************
* Process "xbeP_DragLeave" notification. The procedure resets the status
* message.
******************************************************************************
METHOD MainDialog:HandleDragLeave( aState )
   ::CurrData := NIL
   ::ShowStatus( "" )
RETURN self


******************************************************************************
* Process "xbeP_DragDrop" notification. The procedure retrieves the data 
* associated with the object dropped. If a file list was dropped, this step
* is performed for each file individually.
******************************************************************************
METHOD MainDialog:HandleDragDrop( aState, oData )

 LOCAL i

  ::ShowStatus( "Item Dropped - Opening New Document..." )

  FOR i:=1 TO Len(::CurrData)
      ::NewDocument( ::CurrData[i][1], ::CurrData[i][2] )
  NEXT

  ::ShowStatus( "" )

RETURN XBP_DROPMODE_COPY


******************************************************************************
* Validate the format of the data encapsulated by the "DragDataObject" passed 
* in. If the data can be retrieved in a format supported by the application,
* the method returns an array of the following format:
*   {{<cFormat>,<xData>}...}
* 
* "<cFormat>" identifies the data format and is used to determine the window
* type, see ":NewDocument()". "<xData>" is the data itself. Note that if a 
* group of files is dropped from the Windows Shell, more than one item will 
* be contained in the array. Shell links are resolved first.If one or more 
* items have an unknown format, the method returns the empty array.
******************************************************************************
METHOD MainDialog:ValidateFormat( oData )

 LOCAL aReturn  := {}
 LOCAL i, j
 LOCAL nFileExt, nFormat
 LOCAL cTmp
 LOCAL bBlock
 LOCAL aFiles
 LOCAL aTmp

   //
   // Check if the data is available in one of the 
   // formats supported by the application. Note
   // in addition to the standard formats (text, 
   // image, metafile or file list), the example 
   // two formats defined by Internet Explorer.
   // If a group of files was dropped, each file
   // is processed individually to determine the 
   // format of the data it represents. 
   //
   IF oData:QueryGetFormat(XBPCLPBRD_TEXT) == .T.
      RETURN {{"TEXT", ::GetDropData("TEXT", oData)}}
   ELSEIF oData:QueryGetFormat(XBPCLPBRD_BITMAP) == .T.
      RETURN {{"BITMAP", ::GetDropData("BITMAP",oData) }}
   ELSEIF oData:QueryGetFormat(XBPCLPBRD_METAFILE) == .T.
      RETURN {{"EMF", ::GetDropData("EMF",oData) }}
   ELSEIF oData:QueryGetFormat(XBPCLPBRD_FILELIST) == .T.
      aFiles := oData:GetData( XBPCLPBRD_FILELIST )
      bBlock := {|x| IIF(Upper(Right(x,4)) == ".LNK",;
                     x:=ShellLinkResolve(x),)}
      AEval( aFiles, bBlock,,, .T. )

      FOR j:=1 TO Len(aFiles)
         cTmp := SubStr( aFiles[j], At(".", aFiles[j]) )
         nFileExt := AScan(::FileExts, {|x| Upper(cTmp) == x[1]} )
         IF nFileExt > 0 
            aTmp := {::FileExts[nFileExt][2],::GetDropData(::FileExts[nFileExt][2],;
                     aFiles[j])}
         ELSEIF bUseIE == .T.
            aTmp := {"HTML",::GetDropData("HTML", aFiles[j])}
         ELSE
            RETURN {}
         ENDIF

         AAdd( aReturn, aTmp )
      NEXT

      IF Len(aReturn) > 0
         RETURN aReturn
      ENDIF
   ELSEIF oData:QueryGetFormat(HTML_FORMAT) == .T.
      RETURN {{"HTML",oData:GetData(HTML_FORMAT)}}
   ELSEIF oData:QueryGetFormat(URL_FORMAT) == .T. 
      RETURN {{"HTML",oData:GetData(URL_FORMAT)}}
   ENDIF

RETURN aReturn


******************************************************************************
* Process the data transported via drag-and-drop. "cFormat" is a format
* recognized by the application, eg. "BITMAP" or "TEXT". "xData" presents the
* data item. If a file was dropped, "xData" is the name of a file. If an
* item was dragged over from another application, "xData" usually references
* the data itself.
* IMPORTANT: This example application only supports data transported in
* memory!
******************************************************************************
METHOD MainDialog:GetDropData( cFormat, xData )

 LOCAL xReturn

   DO CASE
      CASE cFormat == "BITMAP"
         IF ValType(xData) == "O"
            RETURN xData:GetData( XBPCLPBRD_BITMAP )
         ENDIF       

         xReturn := XbpBitmap():New():Create()
         xReturn:LoadFile( xData )
         RETURN xReturn
      CASE cFormat == "TEXT"
         IF ValType(xData) == "O"
            RETURN xData:GetData( XBPCLPBRD_TEXT )
         ENDIF       

         RETURN MemoRead( xData )
      CASE cFormat == "EMF"
         IF ValType(xData) == "O"
            RETURN xData:GetData( XBPCLPBRD_METAFILE )
         ENDIF       

         xReturn := XbpMetaFile():New():Create()
         xReturn:Load( xData )
         RETURN xReturn
      CASE cFormat == "HTML"
         RETURN xData
      CASE cFormat == "DBF"
         RETURN xData
    ENDCASE

RETURN NIL


******************************************************************************
* Open a new MDI document to display the data associated with the item
* dropped. "cFormat" is an application-defined string that identifies the
* the data format.
******************************************************************************
METHOD MainDialog:NewDocument( cFormat, xData )

 LOCAL cTitle := "New Document"
 LOCAL oTmp

   ::DrawingArea:SetPointer(, XBPSTATIC_SYSICON_WAIT )

   DO CASE
      CASE cFormat == "TEXT"
        ::NewTextDocument( xData, cTitle + " (Text)" )
      CASE cFormat == "BITMAP"
        ::NewGFXDocument( xData, cTitle  + " (Image)" )
      CASE cFormat == "EMF"
        ::NewGFXDocument( xData, cTitle  + " (Meta File)" )
      CASE cFormat == "DBF"
         ::NewDBFDocument( xData, xData + "(DBF)" )
      OTHERWISE
        ::NewIEDocument( xData, cTitle )
   ENDCASE

   ::DrawingArea:SetPointer(, XBPSTATIC_SYSICON_DEFAULT )

RETURN


******************************************************************************
* Open a new MDI document to display a DBF table using an "XbpQuickBrowse"
* object.
******************************************************************************
METHOD MainDialog:NewDBFDocument( xData, cTitle )

 LOCAL oDlg := XbpDialog():New(), oQB
 LOCAL aSize


   IF Empty(xData) == .T.
      RETURN
   ENDIF
   IF UseDB(xData) == .F.
      RETURN
   ENDIF

   /* Create the viewer dialog
    */
   oDlg:Title := cTitle
   oDlg:DrawingArea:ClipChildren := .T.
   oDlg:Create( ::DrawingArea,,, {400, 300},, .F. )
   oDlg:Cargo := Select()
   oDlg:Close := {|| oDlg:Destroy()}
   oDlg:DrawingArea:Resize := {|aOld, aNew| oQB:Setsize(aNew) }

   /* Create an XbpQuickBrowse object to view the table
    */
   aSize := oDlg:DrawingArea:CurrentSize()

   oQB := XbpQuickBrowse():New( oDlg:DrawingARea,,, aSize )
   oQB:DataLink := DacPagedDatastore():New( Select() )
   oQB:Style := XBP_STYLE_SYSTEMDEFAULT
   oQB:Create()

   CenterControl( oDlg )
   oDlg:Show()
   SetAppFocus( oDlg )

RETURN


******************************************************************************
* Open a new MDI document to display text using a "XbpMLE" object.
******************************************************************************
METHOD MainDialog:NewTextDocument( xData, cTitle )

 LOCAL oDlg := XbpDialog():New(), oMLE
 LOCAL aSize


   IF Empty(xData) == .T.
      RETURN
   ENDIF

   /* Create the viewer dialog
    */
   oDlg:Title := cTitle
   oDlg:DrawingArea:ClipChildren := .T.
   oDlg:Create( ::DrawingArea,,, {400, 300},, .F. )
   oDlg:Close := {|| oDlg:Destroy() }
   oDlg:DrawingArea:Resize := {|aOld, aNew| oMLE:Setsize( aNew ) }

   /* Create a MLE to view the text
    */
   aSize := oDlg:DrawingArea:CurrentSize()

   oMLE := XbpMLE():New( oDlg:DrawingARea,,, aSize )
   oMLE:Create()
   oMLE:SetFontCompoundName( "10.Courier" )
   oMLE:SetData( xData )

   CenterControl( oDlg )
   oDlg:Show()
   SetAppFocus( oDlg )

RETURN


******************************************************************************
* Open a new MDI document to display an image using a XbpPresSpace object
******************************************************************************
METHOD MainDialog:NewGFXDocument( xData, cTitle )

 LOCAL oDlg, oPS, aSize, aPos
 LOCAL aSizeDlg


   IF ValType(xData) != "O" .OR. ;
      (xData:IsDerivedFrom(XbpBitmap())   == .F. .AND. ;
       xData:IsDerivedFrom(XbpMetaFile()) == .F.)
      RETURN
   ENDIF

   /* Create the viewer dialog
    */
   oDlg := XbpDialog():New()
   oDlg:Title := cTitle
   oDlg:DrawingArea:ClipChildren := .T.
   oDlg:ClipChildren := .T.
   oDlg:Create( ::DrawingArea,,, {100, 100},, .F. )
   oDlg:SizeRedraw := .T.
   IF xData:IsDerivedFrom(XbpBitmap()) == .T.
      oDlg:DrawingArea:SetColorBG( XBPSYSCLR_TRANSPARENT )
   ENDIF
   oDlg:Close := {|| oDlg:Destroy()}

   // Create a XbpPresSpace object. Use a code block
   // assigned to the ":paint" callback slot for
   // rendering the image into the XbpPresSpace
   // object
   oPS := XbpPresSpace():New():Create( oDlg:drawingArea:WinDevice() )

   oDlg:DrawingArea:Resize := {|aOld,aNew| oPS:SetPageSize(aNew) }

   IF xData:IsDerivedFrom(XbpBitmap()) == .T.
      // Bitmap image
      aSize    := { xData:xSize, xData:ySize }
      aSizeDlg := oDlg:DrawingArea:CurrentSize()
      aSize[1] := Max( 16, Min(aSize[1], aSizeDlg[1]) )
      aSize[2] := Max( 16, Min(aSize[2], aSizeDlg[2]) )
      aSize    := oDlg:CalcFrameRect( {0,0, aSize[1], aSize[2]} )

      oDlg:SetSize( {aSize[3], aSize[4]} )

      oDlg:DrawingArea:Paint := {|x,y,obj| x:=obj:CurrentSize(), ;
                                  xData:Draw(oPS, {0, 0, x[1], x[2]}, ;
                                  {0, 0, xData:xSize, xData:ySize},,;
                                  GRA_BLT_BBO_IGNORE)}
   ELSE
      // Meta file
      aSize := { 300, 250 }
      oDlg:setSize( aSize )
      oDlg:drawingarea:paint := {|| xData:Draw(oPS, XBPMETA_DRAW_SCALE) }
   ENDIF

   CenterControl( oDlg )
   oDlg:Show()
   SetAppFocus( oDlg )

RETURN


******************************************************************************
* Open a new MDI document to display HTML-encoded data. The browser is also
* used for displaying data of unknown format.
******************************************************************************
METHOD MainDialog:NewIEDocument( xData, cTitle )

 LOCAL oDlg := XbpDialog():New(), oIE
 LOCAL aSize
 LOCAL bOldBlock, oError


   IF Empty(xData) == .T.
      RETURN
   ENDIF

   /* Create the viewer dialog
    */
   oDlg:Title := cTitle
   oDlg:DrawingArea:ClipChildren := .T.
   oDlg:Create( ::drawingArea,,, {400, 300},, .F. )
   oDlg:Cargo := Select()
   oDlg:Close := { || oDlg:Destroy() }
   oDlg:DrawingArea:Resize := { |aOld, aNew| oIE:Setsize(aNew) }

   /* Create the HTML Viewer
    */
   aSize := oDlg:DrawingArea:CurrentSize()
   oIE := XbpHTMLViewer():New( oDlg:DrawingARea,,, aSize )

   bOldBlock := ErrorBlock( {|e| Break(e)} )
   BEGIN SEQUENCE
     oIE:Create()
   RECOVER USING oError
      ErrorBlock( bOldBlock )
      Alert( "Error creating webbrowser control. Please" + CRLF + ;
             "ensure required components are available." , {"OK"} )
   END SEQUENCE
   ErrorBlock( bOldBlock )

   IF At("HTTP://", Upper(Left(xData,7))) == 0
      oIE:Navigate( "FILE://"+ xData )
   ELSE
      oIE:Navigate( xData )
   ENDIF

   CenterControl( oDlg )
   oDlg:Show()
   SetAppFocus( oDlg )

RETURN


******************************************************************************
*Main procedure to test a form
******************************************************************************
PROCEDURE Main

 LOCAL nEvent, oXbp, mp1, mp2
 LOCAL oDlg

   oDlg := MainDialog():New():Create()
   CreateMenuSystem( oDlg )
   oDlg:Close := {|| PostAppEvent(xbeP_Quit,,, oDlg)}
   CenterControl( oDlg )
   oDlg:Show()
   SetAppWindow( oDlg )
   SetAppFocus( oDlg )

   nEvent := xbe_None
   WHILE nEvent != xbeP_Quit
      nEvent := AppEvent ( @mp1, @mp2, @oXbp )
      oXbp:HandleEvent ( nEvent, mp1, mp2 )
      IF nEvent == xbeP_Quit
         QUIT   // AppQuit()
      ENDIF
   ENDDO

RETURN


******************************************************************************
* Overloaded AppSys() procedure to prevent opening of the default XbpCrt
* window
******************************************************************************
PROCEDURE AppSys()
RETURN


*******************************************************************************
* DbeSys() procedure for loading the DBEs used in this sample application
*******************************************************************************
PROCEDURE dbeSys()

  IF ! DbeLoad( "DBFDBE", .F.)
     Alert( "Database-Engine DBFDBE could not be loaded" , {"OK"} )
  ENDIF

  IF ! DbeLoad( "FOXDBE", .F.)
     Alert( "Database-Engine FOXDBE could not be loaded" , {"OK"} )
  ENDIF

RETURN

******************************************************************************
* Procedure that tries to use a DBF file with a DBE specified
******************************************************************************
FUNCTION TryOpenDB( cFile, cDBE )

   LOCAL bError := ErrorBlock( {|e| Break(e) } )

   BEGIN SEQUENCE
      DbeSetDefault( cDBE )
      USE (cFile) NEW
   RECOVER
      Errorblock ( bError )
      RETURN FALSE
   END SEQUENCE

   Errorblock ( bError )

RETURN TRUE


******************************************************************************
* Open a DBF file using one of the loaded DBEs
******************************************************************************
FUNCTION UseDB( cFile )

   IF TryOpenDB( cFile, "DBFDBE" )
      RETURN TRUE
   ENDIF
   IF TryOpenDB( cFile, "FOXDBE" )
      RETURN TRUE
   ENDIF

RETURN FALSE


******************************************************************************
* Create the menu system of the application
******************************************************************************
PROCEDURE CreateMenuSystem( oDlg )

 LOCAL oMB
 LOCAL oMI

   oMB := oDlg:Menubar()

   oMI := XbpMenu():New( oMB )
   oMI:Title := "~File"
   oMI:Create()

   oMI:AddItem( {"~Exit", {|| PostAppEvent(xbeP_Quit,,,oDlg)}} )
   oMB:AddItem( {oMI} )

   oMI := XbpMenu():New( oMB )
   oMI:Title := "~Options"
   oMI:Create()

   oMI:AddItem( {"~Use Internet Explorer for Unknown Data", ;
                {|| bUseIE:=!oMI:IsItemChecked(1),oMI:CheckItem(1,bUseIE)}})
   oMB:AddItem( {oMI} )

RETURN

//EOF
/////
