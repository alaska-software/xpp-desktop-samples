//////////////////////////////////////////////////////////////////////
//
//  SEEKDLG.PRG
//
//  Copyright:
//      Alaska Software, (c) 1997-2025. All rights reserved.         
//  
//  Contents:
//      Contains class SeekDialog to seek data within a
//      DataDialog window --> DATADLG.PRG
//   
//      All records are listed whose index value match with a string
//      entered in a single line edit control (SLE)
//      e.g: entering "S" lists all customers whose lastname
//      starts with "S"
//   
//  Remarks:
//      SeekDialog only works with index keys having datatype "C"
//   
//////////////////////////////////////////////////////////////////////


#include "AppEvent.ch"
#include "Common.ch"
#include "Font.ch"
#include "Gra.ch"
#include "Xbp.ch"


/*
 * Class declaration of a Seek dialog
 */
CLASS SeekDialog FROM XbpDialog
   PROTECTED:
     VAR staticText           // Text "Enter:"
     VAR sleSeek              // Entry field
     VAR listBox              // Listbox to display found records
     VAR buttonOK             // OK pushbutton
     VAR buttonCancel         // Cancel pushbutton
     VAR aFound               // Array for found records
     METHOD arrayToListBox    // Transfer aFound to Listbox

   EXPORTED:
     VAR fieldList            // Array containing field names
     VAR indexOrder           // Index to be searched
     VAR transform            // Code block to convert the search string
                              // to a valid DbSeek() value
     VAR dataDialog           // A DataDialog window
     VAR lastFocus            // The Xbase Part that had focus

     METHOD init              // Life cycle
     METHOD create
     METHOD destroy
     METHOD handleKey         // Customized key handler
     METHOD show              // Display SeekDialog
     METHOD seek              // Seek all records matching the entered
                              // search string
     METHOD goTo              // Go to selected record
ENDCLASS



/*
 * Initialize the object
 */
METHOD SeekDialog:init( oParent, oOwner, aPos )
   LOCAL bKeyboard

   DEFAULT aPos TO  {260,205}

   ::xbpDialog:init( oParent, oOwner, aPos, {400,280}, NIL, .F. )
   ::title     := "Seek"
   ::border    := XBPDLG_THINBORDER
   ::minButton := .F.
   ::maxButton := .F.
   ::sysMenu   := .F.

   bKeyBoard := {|mp1,mp2,obj| ::handleKey( mp1, obj ) }

   ::staticText := XbpStatic():new( ::drawingArea ,, {10,212}, {80,20} )
   ::staticText:caption := "Enter: "

   ::sleSeek := XbpSLE():new( ::drawingArea ,, {90,210}, {285,22} )
   ::sleSeek:tabStop  := .T.
   ::sleSeek:keyBoard := bKeyboard

   ::listBox := XbpListBox():new( ::drawingArea,, {10,60}, {370,135})
   ::listBox:tabStop := .T.
   ::listBox:itemSelected := {|| ::goTo() }

   ::buttonOk := XbpPushButton():new( ::drawingArea,, {10,13}, {100,30} )
   ::buttonOk:caption      := "Ok"
   ::buttonOk:tabstop      := .T.
   ::buttonOK:keyBoard     := bKeyboard
   ::buttonOK:activate     := {|| ::goTo() }

   ::buttonCancel := XbpPushButton():new( ::drawingArea,, {140,13}, {100,30})
   ::buttonCancel:caption  := "Cancel"
   ::buttonCancel:keyBoard := bKeyboard
   ::buttonCancel:activate := {|| ::destroy() }

   ::transform := {|x| x }
   ::aFound    := {}
RETURN self



/*
 * Request system resources
 */
METHOD SeekDialog:create( nOrder, bTransform, aFields, cTitle )

   IF Valtype( cTitle ) == "C"
      ::title := cTitle
   ENDIF

   ::xbpDialog:create()
   ::staticText:create()
   ::sleSeek:create()
   ::listBox:create()
   ::listBox:setFontCompoundName( FONT_DEFFIXED_MEDIUM )
   ::buttonOk:create()
   ::buttonCancel:create()

   ::indexOrder := nOrder
   ::fieldList  := aFields
   ::transform  := bTransform
   ::setOwner():disable()       // Disable owner so that
                                // Seekdialog becomes modal
RETURN self



/*
 * Release system resources
 */
METHOD SeekDialog:destroy
   ::setOwner():enable()
   ::hide()
   IF ::lastFocus <> NIL
      SetAppFocus( ::lastFocus )
      ::lastFocus := NIL
   ENDIF
   ::dataDialog := NIL
   ::xbpDialog:destroy()

RETURN self



/*
 * Process Return and Esc keys
 */
METHOD SeekDialog:handleKey( nKey, obj )

   DO CASE
   CASE nKey == xbeK_ESC
      ::destroy()

   CASE nKey == xbeK_RETURN
      DO CASE
      CASE obj == ::sleSeek
         ::seek()
      CASE obj == ::buttonOK .OR. obj == ::buttonCancel
         PostAppEvent( xbeP_Activate,,, obj )
      ENDCASE
   ENDCASE

RETURN self



/*
 * Display the dialog window and set focus to SLE
 */
METHOD SeekDialog:show
   ::xbpDialog:show()
   SetAppFocus(::sleSeek)
RETURN self



/*
 * Seek data fron SLE in index
 */
METHOD SeekDialog:seek
   LOCAL nOldArea := select()
   LOCAL cData    := Eval( ::transform, ::sleSeek:getData() )
   LOCAL aFound   := {}
   LOCAL i, imax, nOldOrder, nOldRec, nStartRec, nEndRec
/*
 * Add the define USE_POSTGRES_FTS to your project-settings -> compiler tab
 * -> defines ( USE_POSTGRES, USE_POSTGRES_FTS ) to enable the full text
 * search feature.
 */
#ifdef USE_POSTGRES_FTS
   LOCAL oStmt
#endif

   DbSelectArea( ::dataDialog:area )
   DbSuspendNotifications()           // Prevent Notify message to dialog
                                      // during DbSeek() and DbSkip()

#ifdef USE_POSTGRES_FTS
  /*
   * A Sql-Pass-Through-Statement to perform a Full-Text-Search query
   * using the :: notation a parameter is created. This parameter can then
   * be used such as a normal member variable as shown below.
   */
   oStmt := DacSqlStatement():fromChar("SELECT *,__record FROM customer WHERE fts_Col @@ plainto_tsquery(::SearchTerm)")
   oStmt:SearchTerm := cData

   /*
    * Now build the statement and execute the query. We are using __record from
    * the query to retrieve a reference back to the customer table record#.
    */
   oStmt:Build():Query()
   IF(RecCount()>0)
     DO WHILE !EOF()
        AAdd( aFound, { RecordToString( ::fieldList ), field->__record } )
        SKIP 1
     ENDDO

     ::aFound := aFound
     ::arrayToListBox()
   ELSE
     MsgBox( cData + Chr(13)+Chr(10) + "Not found!" )
   ENDIF
   DbCloseArea()
   DbSelectArea( ::dataDialog:area )

#else                                      // during DbSeek() and DbSkip()
   nOldOrder := OrdNumber()
   nOldRec   := Recno()

   OrdSetFocus( ::indexOrder )
   cData := Trim( cData )


   IF DbSeek( cData, .T. )            // Data is found
      nStartRec := nEndRec := Recno()
      IF DbSeekLast( cData )          // Find last matching record
         nEndRec := Recno()
      ENDIF

      DbGoto( nStartRec )             // Load records to array
      AAdd( aFound, { RecordToString( ::fieldList ), Recno() } )
      DO WHILE nEndRec <> Recno()
         DbSkip()
         AAdd( aFound, { RecordToString( ::fieldList ), Recno() } )
      ENDDO

      ::aFound := aFound
      ::arrayToListBox()              // Transfer array to list box
   ELSE
      MsgBox( cData + Chr(13)+Chr(10) + "Not found!" )
   ENDIF

   OrdSetFocus( nOldOrder )
   DbGoto( nOldRec )
#endif

   DbResetNotifications()
   DbSelectArea( nOldArea )

RETURN self



/*
 * Transfer found data to listbox
 */
METHOD SeekDialog:arrayToListBox
    LOCAL i, imax := Len( ::aFound )

    ::listBox:clear()

    FOR i:=1 TO imax
       ::listBox:addItem( ::aFound[i,1] )
    NEXT
RETURN self



/*
 * Go to the record selected in the listbox
 */
METHOD SeekDialog:goto
    LOCAL aItems := ::listBox:getData()
    LOCAL nArea  := ::dataDialog:area
    LOCAL nRecno

    IF ! Empty(aItems)
       nRecno := ::aFound[ aItems[1], 2 ]
    ENDIF

    ::destroy()

    IF nRecno <> NIL
       (nArea) -> ( DbGoTo( nRecno ) )
    ENDIF

RETURN self



/*
 * Create one string from multiple fields
 */
FUNCTION RecordToString( aFieldNames )
   LOCAL i, imax := Len( aFieldNames )
   LOCAL cString := "", xValue, cType

   FOR i:=1 TO imax
      xValue := FieldGet( FieldPos( aFieldNames[i] ) )
      cType  := Valtype( xValue )
      DO CASE
      CASE cType == "C"
         cString += xValue
      CASE cType == "D"
         cString += DtoC( xValue )
      CASE cType == "L"
         cString += IIf( xValue, ".T.", ".F." )
      CASE cType == "N"
         cString += Str( xValue )
      ENDCASE
      IF i < imax
         cString += " " + Chr(179) +" "
      ENDIF
   NEXT

RETURN cString



/*
 * Find last record matching a given index value
 * - the index expression MUST return a string
 */
FUNCTION dbSeekLast( cString )
   LOCAL cIndexKey, cIndexVal, nLen

   cIndexKey  := IndexKey(0)
   IF Empty( cIndexKey )               // No index active
      RETURN .F.                       // *** RETURN  ***
   ENDIF

   nLen    := Len( cString )           // Increase last Chr() by 1
   DbSeek( Left(cString,nLen-1) + ;    // for SOFTSEEK
           Chr(Asc(Right(cString,1)) + 1 ), ;
          .T.)                         // SOFTSEEK ON
   DbSkip(-1)                          // skip 1 back
                                       // Get index value
   IF ( cString == Left(&(cIndexKey),nLen) )
      RETURN .T.                       // Ok, we have a match!
   ENDIF

   DbSkip(1)                           // In case that Eof() was .T.

RETURN .F.



/*
 * Seek record for current child window
 */
PROCEDURE SeekRecord()
   LOCAL oSeekDlg, aPos, aPos1, oDlg := WinMenu():currentWin

   aPos     := oDlg:currentPos()
   aPos1    := GetApplication():mainForm:currentPos()
   aPos[1]  += 10
   aPos[2]  += 10
   aPos[1]  += aPos1[1]
   aPos[2]  += aPos1[2]
   oSeekDlg := SeekDialog():new( AppDesktop(), ;
                                 GetApplication():MainForm, ;
                                 aPos )
   oSeekDlg:create( oDlg:seekOrder , ;
                    oDlg:seekExpr  , ;
                    oDlg:seekFields, ;
                    oDlg:seekTitle   )

   oSeekDlg:lastFocus  := SetAppFocus()
   oSeekDlg:dataDialog := oDlg
   oSeekDlg:show()
RETURN



/*
 * Delete record in current child window
 */
PROCEDURE DeleteRecord()
   LOCAL nButton, oXbp := SetAppFocus()
   LOCAL nArea := Select()

   nButton := ConfirmBox( , ;
                 "Delete this record ?", ;
                 "Delete", ;
                  XBPMB_YESNO , ;
                  XBPMB_QUESTION+XBPMB_APPMODAL+XBPMB_MOVEABLE )

   IF nButton == XBPMB_RET_YES
      DbSelectArea( WinMenu():currentWin:area )

      IF Rlock()
         DbDelete()
         DbUnlock()
         DbSkip( 0 )
      ELSE
         MsgBox( "Record is currently locked!" )
      ENDIF
      DbSelectArea( nArea )
   ENDIF

   SetAppFocus( oXbp )
RETURN
