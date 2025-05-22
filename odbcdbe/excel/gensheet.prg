//////////////////////////////////////////////////////////////////////
//
//  GENSHEET.PRG
//
//  Copyright:
//      Alaska Software, (c) 2000-2025. All rights reserved.         
//  
//  Contents:
//      This sample that explains the various problems and their solutions
//      when working with Excel-files through ODBC.
//   
//////////////////////////////////////////////////////////////////////


#pragma library( "XppUi2.LIB" )
#pragma library( "ADAC20B.LIB" )
/*
 * Including odbcdbe.ch includes requests to ODBCUTIL.LIB.
 * Please see odbcdbe.ch for details.
 */
#include "odbcdbe.ch"
#include "directry.ch"

/* 
 * What is possible?
 * 1. Create a new file
 *     - DSN must contain DBQ=<pathname> . If you don't provide a
 *       path, you won't find the file (it's not created in the current dir).
 *     - Connect to excel
 *     - Check if the file exists (FExists()), delete it if yes (FErase()),
 *       otherwise, Excel will complain that the table already exists.
 *     - Call DbCreate(<sheetname>). Watch the data types. 
 *       "M" is the best bet for a start.
 *     - If you call DbCreate() for an existing file/sheet, you are destroying
 *       data while Excel reports an error ("table already exists."). Be prepared.
 *
 * 2. Append data 
 *     - USE (<sheetname>), don't use the filename here.
 *     - then go ahead as usual (dbAppend(), REPLACE)
 *
 * 3. Update data of existing sheet
 *     - DSN must contain DBQ=<pathname> . If you don't provide a
 *       path, you won't find the file (it's not created in the current dir).
 *     - Connect to excel
 *     - Do an USE to either the sheet name or a cell range, or combined:
 *       USE("sheet1$A1:C45") ... Named regions are also possible.
 *     - Note: The first row starts in cell A2, not A1.
 *     - Then replace data, and goto or skip to the cell you wish before.
 *     - Attention: If the cell is empty, an update can fail. If you need to
 *       update empty cells, you must provide something like a record id by yourself,
 *       like writing numbers into column 1. Use this field as a unique record 
 *       identifier for updates by calling  DbInfo(ODBCDBO_UNIQUE_FIELDS, {1})
 *       after the USE.
 *
 * 4. Read data from sheets, cell ranges or named regions
 *     - DSN must contain DBQ=<pathname> . If you don't provide a
 *       path, you won't find the file (it's not created in the current dir).
 *     - Connect to excel
 *     - Do an USE to either the sheet name or a cell range, or combined:
 *       USE("sheet1$A1:C45") ... Named regions are also possible.
 *     - Note: The first row starts in cell A2, not A1.
 *
 * 5. Delete
 *     - Since the record-termi is unknown to Excel, there is no Delete.
 *       Issueing a DbDelete() will cause an error like "Delete not supported .."
 *       Think different: you just empty the cell. That's it.
 *     - However, the normal DbDelete() would just mark the row as deleted, so
 *       there would be no need to physically remove something. ODBCDBE will
 *       emulate this behaviour if there is a column named _DELETED. The DbDelete()
 *       will just mark these cells with an asterisk, and DbRecall() removes it.
 *
 * 6. Troubleshooting
 *    Because of the different nature of Excel, and the various differences 
 *    if different drivers are considered, it is not possible to tell all 
 *    possible errors, nor their exact origin. Some things, however, should
 *    be taken into account before spending too much time debugging the code.
 *    - A Spreadsheet is a different concept than a database table.
 *    - A spreadsheet can contain tables, where a table can be a cell range,
 *      a named region or a whole worksheet.
 *    - Different types can be used in the same column. Excel uses the first, eventually
 *      more than just the first row to determine the data type of the column.
 *      If one cell contains a different data type, an error will occur immediately.
 *    - There are no records, nor record identifiers. Since updates/deletes require
 *      a where clause, that means, data which identify the row to work with, it is
 *      dangerous when working on non-unique fields, and working on completely empty
 *      cell rows will fail.
 */

/*
 * Write all directories and exe-files into an Excel sheet, useful
 * as a validation matrix of your applications.
 */
FUNCTION Main(cStartDir, cExcel, cOpt)
   LOCAL aStruct
   LOCAL cConnect
   LOCAL oSession
   LOCAL nInsertInto := 0
   LOCAL nEndRow
   LOCAL aEntries
   LOCAL cWsName
   LOCAL bErrb, i

   aStruct := { { "TESTCASE", "F", 0, 0},; 
                { "FILE",     "M", 255,0},;
                { "SYMPTOM",  "M", 255,0}}


   ? "GenSheet <startdir> <xlsfile> [/i:<insertAtRow>]"

   IF cStartDir = NIL 
       cStartDir := CurDir()
   ENDIF
   IF cExcel = NIL
      cExcel = ".\test.xls"
   ENDIF

   IF cOpt != NIL .AND. cOpt = "/i:"
      nInsertInto := val(substr(cOpt,4))
   ENDIF

   ? "Collecting data:"
   aEntries := {}
   EvalDirs(cStartDir, {|n,d| AAdd(aEntries, {n, d})}, "*.exe")
   IF len( aEntries) = 0
        ? "No Entries found in " + cStartDir
        QUIT
   ENDIF
   ? len(aEntries), " entries found."

   cConnect := "DBE=ODBCDBE;DSN=Excel Files;ReadOnly=0;UID=;PWD=;DBQ="
   cConnect += cExcel

   oSession := DacSession():new(cConnect)
   IF !oSession:isConnected()
      MsgBox("Connect to Excel failed:" + oSession:getLastMessage())
      QUIT
   ENDIF

   /*
    * The name of the worksheet is used if the file is to be created,
    * and will be combined with the cell range if to be updated. (/i passed)
    */
   cWsName := "validate"
   IF nInsertInto < 1
      IF FExists(cExcel) 
          FErase(cExcel)
      ENDIF   
      DbCreate(cWsName, aStruct)
   ELSE
      nEndRow := nInsertInto + len(aEntries) + 8
      cWsName := '"' + cWsName + "$A"+var2char(nInsertinto-1)+":C"+var2char(nEndRow) +'"'
   ENDIF
   ? "Opening sheet:" + cWsName
   USE (cWsName) NEW VIA "ODBCDBE"
   IF nInsertInto < 1
      ? "Appending data:" + cWsName
      i:=1
      AEval(aEntries, {|e| Qout(e[1]), dbAppend(), FieldPut(1, i++),;
                           FieldPut(FieldPos("FILE"), e[1])})
   ELSE
      /*
       * Use the testcase column as record-id.
       */
      DbInfo(ODBCDBO_UNIQUE_FIELDS, {FieldPos("TESTCASE")})
      ? "Inserting data:" + cWsName, reccount()
      i := nInsertInto
      AEval(aEntries, {|e| Qout(FieldPut(1,FieldGet(1))), Qout(e[1]),;
                           FieldPut(FieldPos("FILE"), e[1]), DbSkip() })
      /*
       * empty remaining records, if there are any
       */
      DO WHILE !eof()
         FieldPut(FieldPos("FILE"), "")
         dbSkip()
      END
   ENDIF
   dbCloseAll()
   oSession:disConnect()
   ? cWsName + " - done."
RETURN (.T.)

/*
 * Read directories recursively, starting at cDir.
 * Call bBlock for every directory found and all files matching
 * cMask located in this directory.
 */
FUNCTION EvalDirs(cDir, bBlock, cMask)
LOCAL aDirs, cCurDir, i
   aDirs := Directory(cDir+"\*.*", "=D*")
   FOR i:= 1 TO len(aDirs)
      cCurDir := aDirs[i][F_NAME]
      IF cCurDir[-1] != "." .and. cCurDir != "CVS"
         EvalDirs(cDir +"\"+cCurDir, bBlock, cMask)
      ENDIF
   NEXT
   aDirs := Directory(cDir+"\"+cMask)
   Eval(bBlock, Upper(cDir), {})
   FOR i:= 1 TO len(aDirs)
      Eval(bBlock, "  " + cDir +"\"+aDirs[i][F_NAME], aDirs[i])
   NEXT
RETURN NIL

/*
 * Load ODBCDBE as default dbe
 */
PROCEDURE DbeSys
   DbeLoad( "ODBCDBE" )
   DbeSetDefault( "ODBCDBE" )
RETURN


