//////////////////////////////////////////////////////////////////////
//
//  STYLEDCOMBOCLASS.PRG
//
//  Copyright:
//      Alaska Software, (c) 2015-2025. All rights reserved.         
//  
//  Contents:
//      This source code file contains the procedure CreateComboBox(), in which an
//      XbpComboBox object is created whose elements are styled and displayed using
//      HTML/CSS and the owner-drawing feature. 
//   
//      This functionality is encapsulated in a custom Xbase Part class. In 
//      addition, this example uses some advanced programming techniques, for 
//      example for loading images dynamically. Please see the file styledcombo.prg 
//      for a demonstration of the basic concepts for using the XbpHTMLStyle class.
//   
//////////////////////////////////////////////////////////////////////

#include "xbp.ch"

#pragma library( "xppwui.lib" )

/// <summary>Create a combo box object for
/// displaying parts information, styled using
/// HTML and CSS.</summary>
PROCEDURE CreateComboBox( oDlg )
 LOCAL oCb
 LOCAL oItem

   /*
    * Create the combo box object and add the
    * combo box items using the HTML template.
    */
   oCb:= XbpStyledComboBox():New( oDlg:DrawingArea )
   oCb:Create( ,, {50,80}, {300,230} )

   GO TOP
   DO WHILE !EOF()
      oItem := XbpStyledComboBoxItem():new()
      oItem:partName := FIELD->PARTNAME
      oItem:partNo   := FIELD->PARTNO
      oItem:partType := FIELD->PARTTYPE
      oItem:image    := FIELD->BMPIMAGE

      oCb:additem( oItem )
      SKIP 1
   ENDDO
RETURN


/// <summary>
/// The combo box control implemented by this 
/// class displays items styled using HTML
/// and CSS. The class enhances the original
/// combo box class with support for managing
/// items represented as objects, and for
/// dynamically loading images referenced in
/// the HTML markup.
/// </summary>
CLASS XbpStyledComboBox FROM XbpComboBox
 PROTECTED:
   VAR            _items

   METHOD         getItemIndex()
   METHOD         getItemById()

 EXPORTED:
   METHOD         init()

   METHOD         addItem()
   METHOD         delItem()
   METHOD         insItem()
   METHOD         getItem()
   METHOD         clear()

   METHOD         getItemImageData()
ENDCLASS


/// <summary>
/// The XbpStyledComboBoxItem class implements
/// the items displayed in the styled combo box.
/// Basically, the item objects are containers 
/// for the HTML markup representing the item 
/// data (part number, part name, part type 
/// and image).
/// </summary>
CLASS XbpStyledComboBoxItem
 PROTECTED
    VAR           _id
    VAR           _owner
    VAR           _imageData

 EXPORTED:
    METHOD        init()

    ACCESS METHOD getId()           VAR id
    ACCESS METHOD getOwner()        VAR owner
    ASSIGN METHOD setOwner()        VAR owner

    VAR           partName
    VAR           partNo
    VAR           partType
    VAR           image
    VAR           cargo

    METHOD        getHTML()
    METHOD        getImageData()
ENDCLASS


/// <summary>
/// Initialize the combo box object
/// </summary>
/// <param name="oParent">The parent object</param>
/// <param name="oOwner">The owner object</param>
/// <param name="aPos">The position of the combo box on the parent object</param>
/// <param name="aSize">The size of the combo box</param>
/// <param name="aPP">An array with visual properties, such as colors
/// or fonts</param>
/// <param name="lVisible">Specifies whether the combo box is visible
/// after it is created</param>
/// <returns>This method returns self</returns>
METHOD XbpStyledComboBox:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
 LOCAL cCSS
 LOCAL oCSS
 LOCAL aRGB

   ::XbpComboBox:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   /*
    * Define the styling information (CSS) of the
    * combo box items. The following CSS classes
    * are used to do this:
    * .item                  Styling of a normal
    *                        combo box item
    * .item:active           Styling of highlighted
    *                        item
    * .item.comboboxedit     Styling of selected
    *                        item (edit component)
    * .imagebox              Size and position of
    *                        item image including 
    *                        borders
    * .imagebox.comboboxedit Size and position of
    *                        image in selected item
    *                        including borders
    * .image                 Size and position of
    *                        item image within image
    *                        frame
    * .image.comboboxedit    Size and position of
    *                        selected item image 
    *                        within image frame
    * .partname              Styling of the article
    *                        name
    * .partname.comboboxedit Styling of the article
    *                        name in selected item
    * .partinfo              Styling of the article
    *                        description
    * .partinfo.comboboxedit Styling of the article
    *                        description in selected
    *                        item
    * .parttype              Styling of the article
    *                        type
    * .partno                Styling of the article
    *                        number
    *
    * Note the ::color placeholder for inserting
    * color information determined at runtime!
    */
   TEXT INTO cCSS WRAP
    <style>
     .item {
        font-size: 15px;
        color: #232323;
        height: 100%;
        width: 100%;
        overflow: hidden;
     }
     .item:active {
        color: white;
        background-color: rgb(::color);
     }
     .item.comboboxedit {
        font-size: 0.8em;
     }
     .imagebox {
        width:  42px;
        height: 42px;
        padding-left: 5px;
        padding-top:  3px;
     }
     .image {
        width:  42px;
        height: 42px;
     }
     .imagebox.comboboxedit {
        width:  20px;
        height: 1.2em;
        padding-top: 0px;
     }
     .image.comboboxedit {
        width:  1.2em;
        height: 1.2em;
     }
     .partname {
        font-weight: bold;
        margin-left: 12px;
        margin-top: 5px;
        white-space: nowrap;
     }
     .partname.comboboxedit {
        font-weight: normal;
        margin-left: 5px;
        margin-top: 0px;
     }
     .partinfo {
        font-size: smaller;
        color: #666666;
        margin-left: 12px;
        margin-top: 5px;
     }
     .partinfo.comboboxedit {
        display: none;
     }
     .parttype.comboboxedit {
        display: none;
     }
     .partno {
     }
    </style>
   ENDTEXT

   // Insert user-configurable background color.
   // A text container object is used to do this,
   // which can automatically replace tokens with 
   // values from its member variables 
   oCSS := WUITextContainer():new( cCSS )
   aRGB := GraGetRGBIntensity( TranslateSysColor(XBPSYSCLR_HILITEBACKGROUND) )
   oCSS:color := LTrim(Str(aRGB[1])) + "," + LTrim(Str(aRGB[2])) + "," + LTrim(Str(aRGB[3]))

   ::visualStyle := XbpHTMLStyle():new( oCSS:asANSI() )
   ::visualStyle:minHeight := 52
   ::_items := {}

   ::type := XBPCOMBO_DROPDOWNLIST
RETURN self


/// <summary>
/// Add an item to the combo box
/// </summary>
/// <param name="oItem">The item to be added</param>
/// <returns>The ordinal position of the new item</returns>
METHOD XbpStyledComboBox:addItem( oItem )
   IF ValType(oItem) != "O" .OR. oItem:isDerivedFrom(XbpStyledComboBoxItem()) == .F.
      XbpException():raiseParameterType( {oItem} )
   ENDIF

   IF ::status() != XBP_STAT_CREATE
      RETURN .F.
   ENDIF

   /* Add the item to the internal item
    * array. In order to have the system
    * use the HTML markup representing
    * the item's data for drawing, the
    * HTML must be assigned as the
    * caption of the combo box item.
    */
    oItem:owner := self
   AAdd( ::_items, oItem )
RETURN ::XbpComboBox:addItem(oItem:getHTML())


/// <summary>
/// Remove an item from the combo box 
/// </summary>
/// <param name="xItem">The item to be removed</param>
/// <returns>The number of items remaining in the combo box</returns>
/// <remarks>xItem may be an ordinal item number or
/// an item object</remarks>
METHOD XbpStyledComboBox:delItem( xItem )
 LOCAL nIdx
 LOCAL cType

   cType := ValType( xItem )

   IF cType != "N" .AND. cType != "O" .AND.;
      xItem:isDerivedFrom(XbpStyledComboBoxItem()) == .F.
      XbpException():raiseParameterType( {xItem} )
   ENDIF

   /* Remove the item from the internal
    * item array. Also, remove 'physical'
    * combo box item with the HTML markup
    */
   nIdx := ::getItemIndex( xItem )
   IF nIdx > 0
      ::XbpComboBox:delItem( nIdx )
      ARemove( ::_items, nIdx )
   ENDIF
RETURN ::numItems()


/// <summary>
/// Get the specified item of a combo box object
/// </summary>
/// <param name="nItem">The ordinal position of the element</param>
/// <returns>The specified combo box item</returns>
METHOD XbpStyledComboBox:getItem( nItem )
 LOCAL nIdx

   nIdx := ::getItemIndex( nItem )
   IF nIdx > 0
      RETURN ::_items[nIdx]
   ENDIF
RETURN NIL


/// <summary>
/// Get the specified item of a combo box object
/// using its identifier
/// </summary>
/// <param name="cId">The id of the item</param>
/// <returns>The specified combo box item</returns>
METHOD XbpStyledComboBox:getItemById( cId )
 LOCAL nIdx

   nIdx := AScan( ::_items, {|o| o:id == cId} )
   IF nIdx > 0
      RETURN ::_items[nIdx]
   ENDIF
RETURN NIL


/// <summary>
/// Insert an item into the combo box object
/// </summary>
/// <param name="xInsAfter">The item to insert the new
/// item after</param>
/// <returns>.T. if the item is added, .F. otherwise
/// </returns>
/// <remarks>xInsAfter may be an ordinal item number,
/// an item name or a combo box item</remarks>
METHOD XbpStyledComboBox:insItem( xInsAfter, oItem )
 LOCAL cType
 LOCAL nIdx

   cType := ValType( xInsAfter )
   IF cType != "N" .AND. cType != "O" .AND.;
      xInsAfter:isDerivedFrom(XbpStyledComboBoxItem()) == .F.
      XbpException():raiseParameterType( {xInsAfter,oItem} )
   ENDIF
   IF ValType(oItem) != "O" .AND. oItem:isDerivedFrom(XbpStyledComboBoxItem()) == .F.
      XbpException():raiseParameterType( {xInsAfter,oItem} )
   ENDIF

   nIdx := ::getItemIndex( xInsAfter )
   IF nIdx == 0
      RETURN .F.
   ENDIF

   /* Insert the item into the internal item
    * array. In order to have the system
    * use the HTML markup representing
    * the item's data for drawing, the
    * HTML must be assigned as the caption
    * of the combo box item.
    */
   oItem:owner := self
   ::XbpStyledComboBox:insItem( nIdx, oItem:getHTML() )
   AAdd( ::_items, NIL )
   AIns( ::_items, nIdx, oItem )
RETURN .T.


/// <summary>
/// Remove all items from the combo box object
/// </summary>
/// <returns>.T. if the items are removed, .F. otherwise
/// </returns>
METHOD XbpStyledComboBox:clear()
   ::_items := {}
   ::XbpComboBox:clear()
RETURN .T.


/// <summary>
/// Get the ordinal position of an item within the
/// combo box
/// </summary>
/// <param name="xItem">An item specifier</param>
/// <returns>The item's oridinal position</returns>
/// <remarks>xItem may be an ordinal item number
/// or an item object</remarks>
METHOD XbpStyledComboBox:getItemIndex( xItem )
 LOCAL nIdx
 LOCAL cType

   cType := ValType( xItem )
   IF cType == "N"
      nIdx := xItem
      nIdx := Max( nIdx, 0 )
      nIdx := Min( ::numItems(), nIdx )
   ELSEIF cType == "O" .AND. xItem:isDerivedFrom("XbpStyledComboBoxItem")
      nIdx := AScan( ::_items, xItem )
   ELSE
      XbpException():raiseParameterValue( {xItem} )
   ENDIF
RETURN nIdx


/// <summary>
/// Get the raw data of the image assigned
/// to a combo box item.
/// </summary>
/// <returns>The image data or the empty
/// string, if no image is assigned</returns>
/// <remarks>This is a callback method which
/// gets executed when the item's HTML markup
/// is prepared for rendering. See the
/// method:// reference in the HTML</remarks>
METHOD XbpStyledComboBox:getItemImageData( cId )
 LOCAL oItem

   oItem := ::getItemById( cId )
   IF oItem != NIL
      RETURN oItem:getImageData()
   ENDIF
RETURN ""


/// <summary>
/// Initialize the combo box item object
/// </summary>
/// <returns>This method returns self</returns>
METHOD XbpStyledComboBoxItem:init()
   ::_id         := UUIDToChar( UUIDCreate() )
   ::_owner      := NIL
   ::_imageData  := NIL

   ::image       := NIL
   ::partName    := ""
   ::partNo      := ""
   ::partType    := ""
RETURN self


/// <summary>
/// Get the owner of the combo box item.
/// This is the combo box object the item
/// is contained in.
/// </summary>
/// <returns>A combo box object or NIL</returns>
METHOD XbpStyledComboBoxItem:getOwner()
RETURN ::_owner


/// <summary>
/// Change the owner of the combo box item.
/// </summary>
/// <param name="oOwner">The new owner</param>
/// <returns>This method returns self</returns>
METHOD XbpStyledComboBoxItem:setOwner( oOwner )
   IF ValType(oOwner) != "O" .AND. oOwner:isDerivedFrom(XbpStyledComboBox()) == .F.
      XbpException():raiseParameterType( {oOwner} )
   ENDIF

   ::_owner := oOwner
RETURN self


/// <summary>
/// Get the id of the combo box item.
/// </summary>
/// <returns>The item's id</returns>
METHOD XbpStyledComboBoxItem:getId()
RETURN ::_id


/// <summary>
/// Get the HTML markup representing the data
/// displayed in the combo box item.
/// </summary>
/// <returns>A string with the HTML markup
/// </returns>
METHOD XbpStyledComboBoxItem:getHTML()
 LOCAL oHTML
 LOCAL cTempl

   /*
    * Create a template for creating the HTML
    * markup (data and structure) of the combo
    * box items.
    *
    * Note the placeholders for information
    * determined at runtime! These get 
    * automatically replaced with values from
    * the member variables of the text container
    * object managing the HTML markup.
    */
   TEXT INTO cTempl
     <table class="item">
       <tr>
         <td class="imagebox">
           <img class="image" src="method://getitemimagedata/::id"/>
         </td>
         <td>
           <div class="partname">::partname</div>
           <div class="partinfo">
             <span class="partno">No.: ::partno</span>
             <span class="parttype">Type: ::parttype</span>
           </div>
         </td>
       </tr>
     </table>
   ENDTEXT

   oHTML           := WUITextContainer():new( cTempl )
   oHTML:id        := ::_id
   oHTML:partName  := ::partName
   oHTML:partNo    := ::partNo
   oHTML:partType  := ::partType
RETURN oHTML:asANSI()


/// <summary>
/// Get the raw data of the image assigned
/// to the combo box item.
/// </summary>
/// <returns>The image data in PNG format,
/// or the empty string if no image is
/// assigned</returns>
METHOD XbpStyledComboBoxItem:getImageData()
 LOCAL cType

   /* If an image is assigned to the
    * item, retrieve the raw data and
    * cache it for future requests.
    * Return the data from the cache
    * if it has been cached already
    */
   IF !Empty(::_imageData) .OR. ::image == NIL
      RETURN ::_imageData
   ENDIF

   cType := ValType( ::image )
   IF cType $ "CM"
      ::_imageData := ::image
   ELSEIF cType == "O" .AND. ::image:isDerivedFrom(XbpBitmap())
      ::_imageData := ::image:setBuffer( , XBPBMP_FORMAT_PNG, 0 )
   ENDIF
RETURN ::_imageData

// EOF
