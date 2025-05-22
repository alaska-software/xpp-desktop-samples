//////////////////////////////////////////////////////////////////////
//
//  DATEFORM.PRG
//
//  Copyright:
//   Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Sample for XbpDatePicker class
//   
//////////////////////////////////////////////////////////////////////


#include "Gra.ch"
#include "Xbp.ch"
#include "Common.ch"
#include "Appevent.ch"
#include "Font.ch"

#define CRLF Chr(13)+Chr(10)

******************************************************************************
* This class is derived from the implementation-level class of the form.
* Instance variables are declared in the _FlightDetails class.
******************************************************************************
CLASS FlightDetails FROM _FlightDetails
   EXPORTED:
      METHOD init
      METHOD create
      METHOD collectValues
ENDCLASS

******************************************************************************
* Initialize form
******************************************************************************
METHOD FlightDetails:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   * Execute method of the super class
   ::_FlightDetails:init( oParent, oOwner, aPos, aSize, aPP, lVisible )

   ::oCancel:Cancel := .T.
   ::oOk:Default    := .T.

RETURN self


******************************************************************************
* Request system resources
******************************************************************************
METHOD FlightDetails:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
  LOCAL oError
  LOCAL bOldBlock
  LOCAL aAirports := {"Frankfurt", "London Heathrow", "London Gatwick", ;
                      "Stockholm", "Oslo",            "Milan",          ;
                      "Munich",    "New York JFK",    "Boston",         ;
                      "Helsinki",  "Moscow",          "Paris Charles de Gaulle"}

   // Create flight details dialog. BEGIN/END SEQUENCE is used to
   // handle eventual errors, eg. due to a missing component.
   bOldBlock := ErrorBlock( {|e| Break(e)} )
   BEGIN SEQUENCE
      ::_FlightDetails:create( oParent, oOwner, aPos, aSize, aPP, lVisible )
   RECOVER USING oError
      ErrorBlock( bOldBlock )

      IF oError:subCode == 6500
         // Handle the error when the control is not installed

         MsgBox( "Error creating ActiveX Control. Please make sure MS" + CRLF +;
                 "Common Controls 2 6.0 is installed on your computer.",         ;
                 "DatePicker Sample" )
         QUIT
      ELSE

         // Handle all other errors
         Break( oError )     
      ENDIF
   END SEQUENCE
   ErrorBlock( bOldBlock )

   * Set Default Data Values
   ASort( aAirports )
   AEval( aAirports, {|c| ::oDeparturePort:AddItem(c)} )
   ::oDeparturePort:SetData(1)
   AEval( aAirports, {|c| ::oDestinationPort:AddItem(c)} )
   ::oDestinationPort:SetData(2)
   ::oAdults:SetData(1)

   * Display Information about start and stop date
   MsgBox( "The example determines the start and end dates for" + CRLF +;
           "a flight booking. This is done using date picker"   + CRLF +;
           "controls, see DATEFORM.PRG for details.", "Information" )
   * Set Minimal/Maximal Dates
   ::oDeparture:SetData( Date() )
   ::oDeparture:MinDate := Date() -1
   ::oDeparture:MaxDate := Date()+60 // input only within the next 60 days

   ::oArrival:SetData(Date()+1)
   ::oArrival:MinDate   := Date()+1
   ::oArrival:MaxDate   := Date()+60 // input only within the next 60 days

   ::oCalendar:Activate := {|| DisplayCalendar(self)}

RETURN self

******************************************************************************
* Collect the values entered in the dialog
******************************************************************************
METHOD FlightDetails:collectValues()

  LOCAL cValues := ""

   cValues += "Departure: " + Var2LChar( ::oDeparture:GetData() ) + " from " + ;
              ::oDeparturePort:XbpSLE:GetData() + CRLF
   cValues += "Arrival:   " + Var2LChar( ::oArrival:GetData() )   + " to "   + ;
              ::oDestinationPort:XbpSLE:GetData() + CRLF
   cValues += "Adults:    " + Var2LChar( ::oAdults:GetData() )    + CRLF
   cValues += "Children:  " + Var2LChar( ::oInfants:GetData() )   + CRLF

RETURN cValues


******************************************************************************
* Main procedure to test the form
******************************************************************************
PROCEDURE Main

 LOCAL oForm
 LOCAL cData := "(No input was made)"

   /* Create our Flight Details Dialog */
   oForm := FlightDetails():New()
   oForm:Create()

   CenterControl( oForm )

   IF oForm:ShowModal() != XBP_MRESULT_CANCEL
      cData := oForm:CollectValues()
   ENDIF

   MsgBox( cData, "Information" )

RETURN


******************************************************************************
* Quit the application
******************************************************************************
PROCEDURE AppQuit()
   QUIT
RETURN


******************************************************************************
* Display calendar with current booking
******************************************************************************
PROCEDURE DisplayCalendar( oOwner )

  LOCAL oDlg
  LOCAL oXbp
  LOCAL aRect

     oDlg := XbpDialog():New( AppDesktop(), oOwner )
     oDlg:Title       := "Current Booking (Close Window to proceed)"
     oDlg:Border      := XBPDLG_RAISEDBORDERTHIN_FIXED
     oDlg:Create( ,,,{100,100},, .F. )

     oXbp := XbpMonthView():New( oDlg:DrawingArea )
     oXbp:Create()
     oXbp:MultiSelect := .T.
     oXbp:MinDate     := oOwner:oDeparture:GetData()
     oXbp:MaxDate     := oOwner:oArrival:GetData()
     oXbp:MaxSelect   := 9999
     oXbp:SelStart    := oOwner:oDeparture:GetData()
     oXbp:SelEnd      := oOwner:oArrival:GetData()
     oXbp:MonthRows   := 2
     oXbp:MonthColumns:= 2
     oXbp:ShowWeekNumbers := .T.

     oXbp:SetPos( {0,0} )
     oXbp:Disable()

     aRect := oXbp:CurrentSize()
     aRect := {0,0,aRect[1],aRect[2]}
     aRect := oDlg:CalcFrameRect( aRect )
     oDlg:SetSize( {aRect[3],aRect[4]} )

     CenterControl( oDlg, oOwner )

     oDlg:ShowModal()
     oDlg:Destroy()

RETURN


******************************************************************************
* Overloaded AppSys to prevent creation of the default XbpCrt console
******************************************************************************
PROCEDURE AppSys()
RETURN

//EOF
/////
