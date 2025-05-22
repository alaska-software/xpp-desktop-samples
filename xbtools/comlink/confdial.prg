//////////////////////////////////////////////////////////////////////
//
//  CONFDIAL.PRG
//
//  Copyright:
//       Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//       COMLINK configuration dialog.
//////////////////////////////////////////////////////////////////////


#include "gra.ch"
#include "xbp.ch"
#include "common.ch"
#include "appevent.ch"
#include "transmit.ch"

STATIC saBaudRates  := {  300,  1200,  2400,  4800,   9600, ;
                        14400, 19200, 38400, 57600, 115200  }

STATIC saOptions    := { "7N1", "8N1", "7E1", "8E1", "7O1", "8O1", ;
                         "7N2", "8N2", "7E2", "8E2", "7O2", "8O2"  }

STATIC saBlockSizes := { 128, 256, 512, 1024, 2048, 4096 }


******************************************************************************
* ConfigDialog class
******************************************************************************
CLASS ConfigDialog FROM XbpDialog
    EXPORTED:
        VAR SerialBox           // Groupbox: Serial Options
        VAR BaudRateCombo       //           BaudRate combobox
        VAR OptionsCombo        //           Options combobox
        VAR OptionsText         //           Text Static: Options
        VAR BaudRateText        //           Text Static: BaudRate
        VAR COMPortText         //           COM port select text
        VAR COMPortSpin         //           COM port select spin button
        VAR ButtonOK            // OK button
        VAR ButtonCancel        // Cancel button
        VAR TransferBox         // Groupbox: Transfer Options
        VAR BlkSizeCombo        //           Block size combobox
        VAR BlkSizeText         //           Block size text
        VAR CRCCheckBox         //           Use CRC checkbox
        VAR ExistBox            //           Groupbox: If File Exists
        VAR PromptRadio         //                     Prompt User Radiobutton
        VAR OverwriteRadio      //                     Overwrite Radiobutton
        VAR oRootDlg            // root dialog

        VAR nComPort            // selected COM port
        VAR nBaudRate           // selected line speed
        VAR cParity             // selected parity
        VAR nDataBits           // number of data bits
        VAR nStopBits           // number of stop bits
        VAR lUseCRC             // CRC on | off
        VAR lPromptUser         // Prompt user on | off

        METHOD init
        METHOD create
        METHOD popup
        METHOD popdown
        METHOD saveValues
        METHOD setValues
ENDCLASS


******************************************************************************
* Initialize dialog
******************************************************************************
METHOD ConfigDialog:init(oParent, oOwner, aPos, aSize, aPP, lVisible)

   DEFAULT oParent  TO SetAppWindow():setParent(), ;
           aPos     TO {76,170}, ;
           aSize    TO {424,311}, ;
           lVisible TO .F.

   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::SerialBox := XbpStatic():new( ::drawingArea, , {18,78}, {198,174} )

   ::BaudRateCombo  := XbpCombobox():new( ::SerialBox, , {6,6}, {84,84} )
   ::OptionsCombo   := XbpCombobox():new( ::SerialBox, , {108,6}, {84,84} )
   ::OptionsText    := XbpStatic():new( ::SerialBox, , {108,91}, {84,24} )
   ::BaudRateText   := XbpStatic():new( ::SerialBox, , {6,91}, {84,24} )
   ::COMPortText    := XbpStatic():new( ::SerialBox, , {6,121}, {72,24} )
   ::COMPortSpin    := XbpSpinbutton():new( ::SerialBox, , {78,121}, {60,23} )

   ::TransferBox    := XbpStatic():new( ::drawingArea, , {234,66}, {162,186} )
   ::BlkSizeCombo   := XbpCombobox():new( ::TransferBox, , {78,79}, {78,78} )
   ::BlkSizeText    := XbpStatic():new( ::TransferBox, , {6,133}, {66,23} )
   ::CRCCheckBox    := XbpCheckbox():new( ::TransferBox, , {6,103}, {102,24} )

   ::ExistBox       := XbpStatic():new( ::TransferBox, , {6,6}, {132,78} )
   ::PromptRadio    := XbpRadiobutton():new( ::ExistBox, , {6,8}, {120,11} )
   ::OverwriteRadio := XbpRadiobutton():new( ::ExistBox, , {6,32}, {120,17} )

   ::ButtonOK       := XbpPushButton():new( ::drawingArea, , {18,24}, {72,30} )
   ::ButtonCancel   := XbpPushButton():new( ::drawingArea, , {102,24}, {78,30} )

   ::nComPort       :=  2              // Assume COM port 2 to be used
   ::nBaudRate      := 19200           // Initial transmission data
   ::cParity        := "N"
   ::nDataBits      :=  8
   ::nStopBits      :=  1
   ::lUseCRC        := .T.
   ::lPromptUser    := .T.

RETURN self


******************************************************************************
* Request system resources.
******************************************************************************
METHOD ConfigDialog:create(oParent, oOwner, aPos, aSize, aPP, lVisible)
   LOCAL i

   ::XbpDialog:border       := XBPDLG_SIZEBORDER
   ::XbpDialog:icon         := ID_APPNAME
   ::XbpDialog:taskList     := .F.
   ::XbpDialog:title        := "Configuration Dialog"
   ::XbpDialog:visible      := .F.
   ::XbpDialog:minButton    := .F.
   ::XbpDialog:maxButton    := .F.
   ::XbpDialog:sysMenu      := .F.
   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:drawingArea:setColorBG( GRA_CLR_PALEGRAY )
   ::XbpDialog:drawingArea:setFontCompoundName( "8.Helv.normal" )

   ::SerialBox:caption      := "Serial Options"
   ::SerialBox:type         := XBPSTATIC_TYPE_GROUPBOX
   ::SerialBox:create()

   ::COMPortText:caption    := "COM Port:"
   ::COMPortText:options    := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   ::COMPortText:create()

   ::COMPortSpin:create()
   ::COMPortSpin:setColorBG( GRA_CLR_WHITE )
   ::COMPortSpin:setNumLimits(1, 4)

   ::BaudRateText:caption   := "Line Speed:"
   ::BaudRateText:options   := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   ::BaudRateText:create()

   ::BaudRateCombo:type     := XBPCOMBO_SIMPLE
   ::BaudRateCombo:editable := .F.
   ::BaudRateCombo:create()
   ::BaudRateCombo:setColorBG( GRA_CLR_WHITE )

   FOR i := 1 TO Len(saBaudRates)
      ::BaudRateCombo:addItem( Alltrim(Str(saBaudRates[i])) )
   NEXT

   ::OptionsText:caption    := "Options:"
   ::OptionsText:options    := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   ::OptionsText:create()

   ::OptionsCombo:editable  := .F.
   ::OptionsCombo:type      := XBPCOMBO_SIMPLE
   ::OptionsCombo:create()
   ::OptionsCombo:setColorBG( GRA_CLR_WHITE )

   FOR i := 1 TO Len(saOptions)
       ::OptionsCombo:addItem(saOptions[i])
   NEXT

   ::TransferBox:caption    := "Transfer Options"
   ::TransferBox:type       := XBPSTATIC_TYPE_GROUPBOX
   ::TransferBox:create()

   ::BlkSizeCombo:type      := XBPCOMBO_DROPDOWNLIST
   ::BlkSizeCombo:create()
   ::BlkSizeCombo:setColorBG( GRA_CLR_WHITE )

   FOR i := 1 TO Len(saBlockSizes)
       ::BlkSizeCombo:addItem( AllTrim(Str(saBlockSizes[i])) )
   NEXT
   ::BlkSizeCombo:setData( {4} )

   // Disable block size selection for the kind of
   // protocol used in this application.
   ::BlkSizeCombo:disable()

   ::BlkSizeText:caption    := "Block Size: "
   ::BlkSizeText:options    := XBPSTATIC_TEXT_VCENTER+XBPSTATIC_TEXT_CENTER
   ::BlkSizeText:create()

   ::CRCCheckBox:caption    := "Use CRC"
   ::CRCCheckBox:selection  := .T.
   ::CRCCheckBox:pointerFocus := .F.
   ::CRCCheckBox:create()

   ::ExistBox:caption       := "If File Exists"
   ::ExistBox:type          := XBPSTATIC_TYPE_GROUPBOX
   ::ExistBox:create()

   ::PromptRadio:caption    := "Prompt User"
   ::PromptRadio:selection  := .T.
   ::PromptRadio:pointerFocus := .F.
   ::PromptRadio:create()

   ::OverwriteRadio:caption := "Overwrite"
   ::OverwriteRadio:pointerFocus := .F.
   ::OverwriteRadio:create()

   ::ButtonOK:caption       := "OK"
   ::ButtonOK:preSelect     := .T.
   ::ButtonOK:activate      := {|| ::saveValues(), ::popdown() }
   ::ButtonOK:create()

   ::ButtonCancel:caption   := "Cancel"
   ::ButtonCancel:activate  := {|| ::popdown() }
   ::ButtonCancel:create()

   ::setValues()
RETURN self


******************************************************************************
* Popup configuration dialog.
******************************************************************************
METHOD ConfigDialog:popup(aPos)

   stopReceiver()                      // Halt receive thread
   ::setValues()                       // Get current settings
   ::setPos({aPos[1]+20, aPos[2]+20})  // Set dialog position
   ::show()                            // and show it.

   SetAppFocus( ::ButtonOK )           // Make sure OK button is selected.
RETURN

******************************************************************************
* Hide configuration dialog.
******************************************************************************
METHOD ConfigDialog:popdown()

   ::hide()                            // Hide dialog.
   SetAppFocus( ::oRootDlg )           // Set focus to root dialog.
   ::oRootDlg:receive()                // Start receive thread again
RETURN

******************************************************************************
* Save settings in member variables.
******************************************************************************
METHOD ConfigDialog:saveValues
   LOCAL cOptions := ::OptionsCombo:XbpSLE:getData()

   ::nComPort    := ::COMPortSpin:getData()
   ::nBaudRate   := Val( ::BaudRateCombo:XbpSLE:getData() )
   ::cParity     := SubStr( cOptions, 2, 1 )
   ::nDataBits   := Val( SubStr(cOptions, 1, 1) )
   ::nStopBits   := Val( SubStr(cOptions, 3, 1) )
   ::lUseCRC     := ::CRCCheckBox:getData()
   ::lPromptUser := ::PromptRadio:getData()
RETURN


******************************************************************************
* Set control values according to member variables
******************************************************************************
METHOD ConfigDialog:setValues
    LOCAL cOptions := ::OptionsCombo:XbpSLE:getData()

    ::COMPortSpin:setData( ::nComPort )
    ::BaudRateCombo:setData( {AScan(saBaudRates, ::nBaudRate)} )
    ::OptionsCombo:setData( {AScan(saOptions, Str(::nDataBits,1) + ::cParity + Str(::nStopBits,1))})
    ::CRCCheckBox:setData(::lUseCRC)
    ::PromptRadio:setData(::lPromptUser)
    ::OverwriteRadio:setData(!::lPromptUser)
RETURN
