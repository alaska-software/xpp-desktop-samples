//////////////////////////////////////////////////////////////////////
//
//  STYLEDCOMBO.PRG
//
//  Copyright:
//      Alaska Software, (c) 2015-2025. All rights reserved.         
//  
//  Contents:
//      This source code file contains the procedure CreateComboBox(), in which 
//      an XbpComboBox object is created whose elements are styled and displayed
//      using HTML/CSS and the owner-drawing feature. 
//   
//      This implementation was intentionally kept simple and is meant to 
//      demonstrate the basic concepts. A more advanced example, in which the same
//      functionality is encapsulated in generic interfaces to be used in the
//      application, can be found in the file styledcomboclass.prg.
//      
//   
//////////////////////////////////////////////////////////////////////

#include "xbp.ch"

#pragma library( "asiutl10.lib" )

/// <summary>Create a combo box object for
/// displaying parts information, styled using
/// HTML and CSS.</summary>
PROCEDURE CreateComboBox( oDlg )
 LOCAL cItem
 LOCAL oCb
 LOCAL cTempl
 LOCAL cCSS
 LOCAL aRGB

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
        background-color: rgb(%COLOR%);
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

   // Insert user-configurable background color
   aRGB := GraGetRGBIntensity( TranslateSysColor(XBPSYSCLR_HILITEBACKGROUND) )
   cCSS := StrTran( cCSS, "%COLOR%", LTrim(Str(aRGB[1]))+","+LTrim(Str(aRGB[2]))+","+;
                    LTrim(Str(aRGB[3])) )

   /*
    * Create a template for creating the HTML
    * markup (data and structure) of the combo
    * box items
    *
    * Note the placeholders for information
    * determined at runtime!
    */
   TEXT INTO cTempl
     <table class="item">
       <tr>
         <td class="imagebox">
           <img class="image" src="data:image/png;base64,%IMAGE%"/>
         </td>
         <td>
           <div class="partname">%PARTNAME%</div>
           <div class="partinfo">
             <span class="partno">No.: %PARTNO%</span>
             <span class="parttype">Type: %PARTTYPE%</span>
           </div>
         </td>
       </tr>
     </table>
   ENDTEXT

   /*
    * Create the combo box object and add the
    * combo box items using the HTML template.
    * Note the Base64-encoding of the part image
    * (transformation to text)!
    */
   oCb:= XbpComboBox():New( oDlg:DrawingArea )
   oCb:VisualStyle := XbpHTMLStyle():New( cCSS )
   oCb:VisualStyle:MinHeight:= 52
   oCb:Type := XBPCOMBO_DROPDOWNLIST
   oCb:Create( ,, {50,80}, {300,230} )

   GO TOP
   DO WHILE !EOF()
      cItem := StrTran( cTempl, "%PARTNAME%", FIELD->PARTNAME )
      cItem := StrTran( cItem,  "%PARTNO%",   FIELD->PARTNO )
      cItem := StrTran( cItem,  "%PARTTYPE%", FIELD->PARTTYPE )
      cItem := StrTran( cItem,  "%IMAGE%",    Bin2Base64(FIELD->BMPIMAGE) )

      oCb:additem( cItem )
      SKIP 1
   ENDDO
RETURN

// EOF

