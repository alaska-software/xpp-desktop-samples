/******************************************************************************
 *
 *  CALL.C
 *
 *  Copyright:
 *    Alaska Software, (c) 1995-2000. All rights reserved.
 *  
 *  Contents :
 *    Example to call an Xbase++ function from a C function
 *
 ******************************************************************************/

#ifdef __WINDOWS__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <os2def.h>
#endif

#include <xpppar.h>
#include <xppcon.h>
#include <error.ch>

static char* outString = "Hello World!";

XPPRET XPPENTRY HELLOWORLD(XppParamList paramList)
{

   ContainerHandle chResult = _conNew(NULLCONTAINER);
   ContainerHandle chString = _conPutC(NULLCONTAINER, outString);
   XPPAPIRET       xr;

   /*
      call QOUT() function
   */
   xr = _conCall(chResult, "QOut", 1, chString);
   /*
      Check return value
   */
   if (xr != 0)
      _retnl(paramList, xr);
   else
      _retc(paramList, outString);
   /*
      Release temporary containers
   */
   _conRelease(chResult);
   _conRelease(chString);
}

#ifdef __WINDOWS__
/*
  Windows-API wrapper and callback example:

*/
typedef struct {
      ContainerHandle chArray;
      ULONG index;
} EnumParam;


/*
  The enumeration callback function

  return: TRUE: continue, FALSE: stop the enumeration
*/
static BOOL CALLBACK EnumWindowProc(HWND  hwnd, EnumParam *eParam) 
{
   ContainerHandle chElement;

   /* filter visible windows */
   if (!IsWindowVisible( hwnd)) {
      return TRUE;
   }
   /* always increment index */
   eParam->index++;

   /* count only or fill ? */
   if (eParam->chArray) {
      chElement = _conPutNL( NULLCONTAINER, (ULONG) hwnd );
      /* insert new element into the array */
      if (chElement != NULLCONTAINER && \
	  0==_conArrayPut( eParam->chArray, chElement, eParam->index, 0)) {
	 /* element inserted, continue with enumeration */
	 return TRUE;
      }
      else {
	 /* error with insertion, release element and stop enumeration */
	 _conRelease( chElement );
	 return FALSE;
      }
   }
   return TRUE;
}

/*
  ListWindowHandles() => aHandles

  returns all visible top level windows found in the system as
  an array of handles
*/
XPPRET XPPENTRY LISTWINDOWHANDLES(XppParamList paramList) 
{
   ULONG rc;
   EnumParam eParam;
   
   eParam.index = 0;
   eParam.chArray = 0;

   /* step 1: count elements */
   rc = EnumWindows((WNDENUMPROC) EnumWindowProc, (LPARAM) &eParam);

   /* step 2: create array */
   eParam.chArray = _conNewArray(1, eParam.index);

   /* step 3: fill array */
   if (rc && eParam.index > 0 && eParam.chArray) {
      eParam.index = 0;
      EnumWindows((WNDENUMPROC) EnumWindowProc, (LPARAM) &eParam);
   }

   /* return the created array */
   _conReturn( paramList, eParam.chArray );
}

/*
  GetWindowText( nHandle) => cText

  Simple WinAPI-wrapper for GetWindowText()
*/
XPPRET XPPENTRY GETWINDOWTEXT(XppParamList paramList) 
{
   HWND hnd;
   CHAR buffer[255]={0};

   if (XPP_IS_NUM(_partype(paramList,1))) {
      hnd = (HWND) _parnl( paramList, 1 );
      GetWindowText( hnd, buffer, sizeof(buffer)-1 );
   }

   _retc( paramList, buffer );
}
#endif
#ifdef __OS2__
 
#define INCL_WINWINDOWMGR       /* Window Manager Functions     */
#include <os2.h>
 
/*
  ListWindowHandles() => aHandles

  returns all visible top level windows found in the system as
  an array of handles
*/
XPPRET XPPENTRY LISTWINDOWHANDLES(XppParamList paramList) 
{
   HWND  hwndParent;       /* Handle of the parent window */
   HWND  hwndNext;         /* current enumeration handle  */
   HENUM hEnum;            /* enumeration handle          */
   ULONG numWin;
   ULONG index;
   ContainerHandle chArray;
   ContainerHandle chElement;
   
   /* step 1: count elements */
   hwndParent = HWND_DESKTOP;
   numWin = 0;
   hEnum = WinBeginEnumWindows(hwndParent);
   while ((hwndNext = WinGetNextWindow(hEnum)) != NULLHANDLE) {
      if (WinIsWindowShowing( hwndNext )) {
	 numWin++;
      }
   }
   WinEndEnumWindows (hEnum);

   /* step 2: create array */
   chArray = _conNewArray(1, numWin);
   hwndParent = HWND_DESKTOP;
   hEnum = WinBeginEnumWindows(hwndParent);
   index = 1;
   while ((hwndNext = WinGetNextWindow(hEnum)) != NULLHANDLE && index <= numWin) {

      if (WinIsWindowShowing( hwndNext )) {

	 chElement = _conPutNL( NULLCONTAINER, (ULONG) hwndNext );
	 if (chElement != NULLCONTAINER && \
	     0==_conArrayPut( chArray, chElement, index, 0)) {
	    index++;
	 }
	 else {
	    _conRelease( chElement );
	 }
      }
   }
   WinEndEnumWindows (hEnum);
 
   /* return the created array */
   _conReturn( paramList, chArray );
}

/*
  GetWindowText( nHandle) => cText

  Simple WinAPI-wrapper for GetWindowText()
*/
XPPRET XPPENTRY GETWINDOWTEXT(XppParamList paramList) 
{
   HWND hnd;
   CHAR buffer[255]={0};

   if (XPP_IS_NUM(_partype(paramList,1))) {
      hnd = (HWND) _parnl( paramList, 1 );
      WinQueryWindowText( hnd, sizeof(buffer)-1, buffer );
   }

   _retc( paramList, buffer );
}
#endif


