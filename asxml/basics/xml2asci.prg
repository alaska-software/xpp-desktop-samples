/*
 *  XML2ASCI.PRG
 *
 *  Contents:
 *      Example that shows how to dump all a XML document
 *      by traversing the document tree.
 *
 */

#include "simpleio.ch"
#include "asxml.ch"

/*
 * add library request
 */
#pragma library("asxml10.lib")


PROC APPSYS
RETURN


/*
 * xml2ascii <cFileName>
 *
 * Dump XML file to stdout.
 */
PROC Main(cFileName)
    LOCAL nXMLDoc, nActions

    IF(cFileName == NIL)
        ? "usage: xml2acsi <cFileName>"
        QUIT
    ENDIF

    /*
     * create and open new XML document
     */
    nXMLDoc := XMLDocOpenFile(cFileName)
    
    IF(nXMLDoc == 0)
        /*
         * print out any errors occured while opening, reading or parsing the document
         */
        PrintErrors(nXMLDoc)
        QUIT
    ENDIF
 
    /*
     * Get the root tag of the document.
     */
    nTag := XMLDocGetRootTag(nXMLDoc)

    /*
     * dump tag and all of this children
     */
    DumpTag(nTag)

    /*
     * close XML document
     */
    XMLDocClose(nXMLDoc)
RETURN
    
/*
 * dump a XML tag and its children recursively
 */
PROC DumpTag(nTag)
    LOCAL aMember, n

    IF !XMLGetTag(nTag, @aMember)
        RETURN
    ENDIF

    ?
    ? "--- Tag (", AllTrim(Str(nTag)), ")---"
    ? "Name   : ", aMember[XMLTAG_NAME]
    ? "Content: ", aMember[XMLTAG_CONTENT]
    ? "Attrib : ", aMember[XMLTAG_ATTRIB]
    ? "Child  : ", aMember[XMLTAG_CHILD]
      
    /*
     * dump all children
     */
    IF aMember[XMLTAG_CHILD] != NIL
        FOR n := 1 TO Len(aMember[XMLTAG_CHILD])
            DumpTag(aMember[XMLTAG_CHILD][n])
        NEXT
    ENDIF
RETURN

/*
 * print error messages
 */
PROC PrintErrors(nXmlDoc)
    LOCAL n, aErrors := XMLDocGetErrorList(nXMLDoc)

    IF aErrors == NIL
        ? "No error descriptions available!"
    ENDIF

    FOR n := 1 TO Len(aErrors)
        /*
         * format and print error message
         */
        ? aErrors[n, XML_ERROR_FILE]+;
          "("+AllTrim(Str(aErrors[n, XML_ERROR_LINE]))+":"+;
          AllTrim(Str(aErrors[n, XML_ERROR_COLUMN]))+")"+;
          " error XPL00"+AllTrim(Str(aErrors[n, XML_ERROR_ID]))+": "+;
          GetErrorText(aErrors[n, XML_ERROR_ID])
        IF aErrors[n, XML_ERROR_ADDINFO] != NIL
            ? aErrors[n, XML_ERROR_FILE]+;
              "("+AllTrim(Str(aErrors[n, XML_ERROR_LINE]))+":"+;
              AllTrim(Str(aErrors[n, XML_ERROR_COLUMN]))+")"+;
              " info: "+aErrors[n, XML_ERROR_ADDINFO]
        ENDIF
    NEXT
RETURN

/*
 * array to translate error codes to error messages
 */
FUNCTION GetErrorText(nError)
    STATIC aErrorText := {;
        "out of memory; close some applications and try again",;
        "invalid DTD declaration",;
        "content outside XML tags not allowed",;
        "document has no root tag",;
        "invalid endtag or no endtag found",;
        "expecting string delimiter",;
        "unterminated string",;
        "file not found",;
        "file read error",;
        "missing name of file",;
        "duplicate attribute",;
        "malformed attribute",;
        "invalid name for attribute",;
        "error while processing action codeblock",;
    }
RETURN aErrorText[nError]
