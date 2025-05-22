/*
 *  DUMPXML.PRG
 *
 *  Contents:
 *      Example that shows how to dump all the content of a XML document
 *      by using action codeblocks.
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
 * dumpxml <cFileName>
 *
 * Dump XML file to stdout.
 */
PROC Main(cFileName)
    LOCAL nXMLDoc, nActions

    IF(cFileName == NIL)
        ? "usage: dumpxml <cFileName>"
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
     * set action codeblock on every #PCDATA content tag
     */
    nActions := XMLDocSetAction(nXMLDoc, "//#PCDATA", {|n,c,a,h|DumpTag(n,c,a,h)})
    
    IF nActions == 0
        ? "Document contains no #PCDATA content"
    ENDIF

    /*
     * set action codeblock on every #CDATA content tag
     */
    nActions := XMLDocSetAction(nXMLDoc, "//#CDATA", {|n,c,a,h|DumpTag(n,c,a,h)})
    
    /*
     * process action codeblocks
     */
    XMLDocProcess(nXMLDoc)
    /*
     * close XML document
     */
    XMLDocClose(nXMLDoc)
RETURN
    
/*
 * dump a XML tag
 */
FUNCTION DumpTag(cTag, cContent, aAttribs, nHandle)    
    LOCAL n

    /*
     * test if tag has attributes
     */
    IF aAttribs != NIL
        ? cTag+"("
        /*
         * append all attributes as name=value pairs
         */
        IF aAttribs != NIL
            FOR n := 1 TO Len(aAttribs)     
                ?? aAttribs[n, TAGATTR_NAME] + '="' + aAttribs[n, TAGATTR_VALUE] + '" '
            NEXT
        ENDIF        
        ?? "):"
    ENDIF
    IF cContent != NIL
        ? cContent
    ENDIF
        
RETURN XML_PROCESS_CONTINUE

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
