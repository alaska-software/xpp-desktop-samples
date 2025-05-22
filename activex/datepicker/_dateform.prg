//////////////////////////////////////////////////////////////////////
//
//  _DATEFORM.PRG
//
//  Copyright:
//     Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//     Class code created by the Xbase++ FormDesigner
//   
//////////////////////////////////////////////////////////////////////


#ifndef _FLIGHTDETAILS_
#define _FLIGHTDETAILS_

#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

CLASS _FlightDetails FROM XbpDialog
   EXPORTED:
      VAR editControls

      * Contained control elements
      VAR Static1
      VAR Static2
      VAR Static21
      VAR Static3
      VAR Static4
      VAR Static5
      VAR Static6
      VAR Static7
      VAR oDeparture
      VAR oArrival
      VAR oDeparturePort
      VAR oDestinationPort
      VAR oAdults
      VAR oInfants
      VAR oOk
      VAR oCancel
      VAR oCalendar

      METHOD init
      METHOD create
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD _FlightDetails:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   DEFAULT oParent  TO AppDesktop(), ;
           aPos     TO {513,454}, ;
           aSize    TO {351,356}, ;
           lVisible TO .F.

   DEFAULT aPP TO {}
   AAdd ( aPP, { XBP_PP_COMPOUNDNAME, "8.Tahoma" } )
   ::XbpDialog:init( oParent, oOwner, aPos, aSize, aPP, lVisible )
   ::XbpDialog:taskList := .T.
   ::XbpDialog:title := "FlightDetails"

   ::editControls := {}

   ::Static1          := XbpStatic():new( ::drawingArea, , {8,284}, {300,36}, { { XBP_PP_COMPOUNDNAME, "14.Tahoma Bold" } } )
   ::Static1:caption := "Alaska Air Travel"
   ::Static1:clipSiblings := .T.
   ::Static1:options := XBPSTATIC_TEXT_VCENTER

   ::Static2          := XbpStatic():new( ::drawingArea, , {24,56}, {292,216} )
   ::Static2:caption := "Flight Details"
   ::Static2:clipSiblings := .T.
   ::Static2:type := XBPSTATIC_TYPE_GROUPBOX

   ::Static21         := XbpStatic():new( ::Static2, , {16,172}, {132,24}, { { XBP_PP_COMPOUNDNAME, "8.Tahoma Bold" } } )
   ::Static21:caption := "Date of Departure"
   ::Static21:clipSiblings := .T.
   ::Static21:options := XBPSTATIC_TEXT_VCENTER

   ::Static3          := XbpStatic():new( ::Static2, , {16,140}, {132,24}, { { XBP_PP_COMPOUNDNAME, "8.Tahoma Bold" } } )
   ::Static3:caption := "Date of Arrival"
   ::Static3:clipSiblings := .T.
   ::Static3:options := XBPSTATIC_TEXT_VCENTER

   ::Static4          := XbpStatic():new( ::Static2, , {16,76}, {132,24}, { { XBP_PP_COMPOUNDNAME, "8.Tahoma Bold" } } )
   ::Static4:caption := "Destination Airport"
   ::Static4:clipSiblings := .T.
   ::Static4:options := XBPSTATIC_TEXT_VCENTER

   ::Static5          := XbpStatic():new( ::Static2, , {16,108}, {132,24}, { { XBP_PP_COMPOUNDNAME, "8.Tahoma Bold" } } )
   ::Static5:caption := "Departure Airport"
   ::Static5:clipSiblings := .T.
   ::Static5:options := XBPSTATIC_TEXT_VCENTER

   ::Static6          := XbpStatic():new( ::Static2, , {16,44}, {132,24}, { { XBP_PP_COMPOUNDNAME, "8.Tahoma Bold" } } )
   ::Static6:caption := "Number of adults"
   ::Static6:clipSiblings := .T.
   ::Static6:options := XBPSTATIC_TEXT_VCENTER

   ::Static7          := XbpStatic():new( ::Static2, , {16,12}, {132,24}, { { XBP_PP_COMPOUNDNAME, "8.Tahoma Bold" } } )
   ::Static7:caption := "Children"
   ::Static7:clipSiblings := .T.
   ::Static7:options := XBPSTATIC_TEXT_VCENTER

   ::oDeparture       := XbpDatepicker():new( ::Static2, , {160,172}, {120,24} )

   ::oArrival         := XbpDatepicker():new( ::Static2, , {160,140}, {120,24} )

   ::oDeparturePort   := XbpCombobox():new( ::Static2, , {160,48}, {120,84}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oDeparturePort:tabstop := .T.
   ::oDeparturePort:type := XBPCOMBO_DROPDOWNLIST

   ::oDestinationPort := XbpCombobox():new( ::Static2, , {160,16}, {120,84}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oDestinationPort:tabstop := .T.
   ::oDestinationPort:type := XBPCOMBO_DROPDOWNLIST

   ::oAdults          := XbpSpinbutton():new( ::Static2, , {160,44}, {60,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oAdults:tabStop := .T.

   ::oInfants         := XbpSpinbutton():new( ::Static2, , {160,12}, {60,24}, { { XBP_PP_BGCLR, XBPSYSCLR_ENTRYFIELD } } )
   ::oInfants:tabStop := .T.

   ::oOk              := XbpPushButton():new( ::drawingArea, , {128,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oOk:caption := "Ok"
   ::oOk:tabStop := .T.
   ::oOk:activate := {|| NIL }

   ::oCancel          := XbpPushButton():new( ::drawingArea, , {236,12}, {96,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oCancel:caption := "Cancel"
   ::oCancel:tabStop := .T.
   ::oCancel:activate := {|| NIL }

   ::oCalendar        := XbpPushButton():new( ::drawingArea, , {10,12}, {50,24}, { { XBP_PP_BGCLR, XBPSYSCLR_BUTTONMIDDLE }, { XBP_PP_FGCLR, -58 } } )
   ::oCalendar:caption := "Display"
   ::oCalendar:tabStop := .T.
   ::oCalendar:activate := {|| NIL }

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD _FlightDetails:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::XbpDialog:create( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::Static1:create()

   ::Static2:create()

   ::Static21:create()

   ::Static3:create()

   ::Static4:create()

   ::Static5:create()

   ::Static6:create()

   ::Static7:create()

   ::oDeparture:create()

   ::oArrival:create()

   ::oDeparturePort:create()

   ::oDestinationPort:create()

   ::oAdults:create()

   ::oInfants:create()

   ::oOk:create()

   ::oCancel:create()

   ::oCalendar:create()

RETURN self

#endif

//EOF
/////
