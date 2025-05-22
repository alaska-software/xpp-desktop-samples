/******************************************************************************
 *
 *  SOUNDEX.C
 *
 *  Copyright:
 *    Alaska Software, (c) 1997-2002. All rights reserved.
 *  
 *  Contents :
 *    the C-functions SOUNDEX(), DIFFERENCE() 
 *    which are callable from Xbase++
 *
 *    A soundex is used to search for phonetical matches. The difference 
 *    between two soundexes expresses the ranking of correspondence, 
 *    0 means totally different.
 *
 ******************************************************************************/

#ifdef __WINDOWS__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <os2def.h>
#endif
#undef DIFFERENCE
#undef NULL
#undef toupper
#define NULL 0

#include <xpppar.h>
#include <xppcon.h>
#include <error.ch>
#include "soundex.ch"

#define isupperalpha(x) (x>='A' && x<='Z')
#define toupper(x)      ((x>='a' && x <='z')? x&0x5F: x)

#define SOUNDEX_LENGTH 4

/* 
   _SoundexInC

   INPUT:  source, length of source, pointer to dest, compatibily mode
   OUTPUT: soundex-code on dest
   RETURN: number of chars on dest

   Note: This function is usable by other C-functions only.
*/

static int _SoundexInC(char *source, ULONG srcLen, char *dest, int compMode) 
{
ULONG i;                /* count to srcLen */
int j;                  /* to SOUNDEX_LENGTH */
char result;            /* result of transformation */
char prevChar;          /* previous char */
                        /* transformation table */
                          /* A   B   C   D   E   F   G   H   I   J   K   L   M   */
static char transform[26]= { 0 ,'1','2','3', 0 ,'1','2', 0 , 0 ,'2','2','4','5',
                          /* N   O   P   Q   R   S   T   U   V   W   X   Y   Z */
                            '5', 0 ,'1','2','6','2','3', 0 ,'1', 0 ,'2', 0, '1'};

/* soundex algorithm by Russell
 * - skip spaces
 * - convert to upper case
 * - retain 1st letter
 * - transform every following char, if it not matches the previous and
 *   its transformation not equals to 0 
 * - other chars: skip (Clipper),abort
 * - after reaching len(dest): 
 *   if len(dest) < SOUNDEX_LENGTH, fill up with  '0's
 */

   for (i=0,j=0, prevChar=0; i<srcLen && j<SOUNDEX_LENGTH; i++ ) {
      char c = toupper(source[i]);
      if (c==' ') {
         continue;
      }
      if (!isupperalpha(c)) {
         if (compMode!=SOUNDEX_CLIPPER) {
            break;
         }
      }
      else {
         result=transform[c-'A'];
         if (j == 0) {
            dest[j++] = c;
         }
         else if ( result != 0 && result != prevChar ) {
            dest[j++] = result;
         }
         prevChar = result;
      }
   }
   while (j < SOUNDEX_LENGTH) {
      dest[j++] = '0';
   }
   dest[j]=0;
   return (j);
}

/* 
   _DifferenceInC

   INPUT:  source 1, length 1, source 2, length 2, soundex-compat.mode
   RETURN: level of 

   Note: This function is usable by other C-functions only.
*/

static int _DifferenceInC(char *src1,ULONG src1len,
                          char *src2,ULONG src2len,int SoundexMode)
{
int result=0,i;                   /* count to SOUNDEX_LENGTH */
char scode1[SOUNDEX_LENGTH+1],    /* soundex-codes */
     scode2[SOUNDEX_LENGTH+1];

   /*
    * gets the soundex code for both strings
    * and counts equals per position
    */
   _SoundexInC(src1,src1len,scode1,SoundexMode);
   _SoundexInC(src2,src2len,scode2,SoundexMode);
   for (i=0;i<SOUNDEX_LENGTH;i++) {
      if (scode1[i]==scode2[i])
         result++;
   }
   return result;
}


/* 
   SOUNDEX(cInput, [nModus]) -> cSoundex

   Prepares Xbase++ parameter and calls _SoundexInC.

   INPUT:   STRING (references are ignored), 
            NUMERIC (compatibility mode, optional, default.:Clipper)
   RETURN:  STRING, LEN(STRING)==SOUNDEX_LENGTH

   Note: This function is usable from Xbase++ only.
*/                      

XPPRET XPPENTRY SOUNDEX(XppParamList paramList)
{
char *Word;                        /* pointer to passed string */
ULONG wordSize;                    /* length of string */
ContainerHandle chWord;            /* container handle for string */
char SoundexCode[SOUNDEX_LENGTH+1];/* the soundex code */
int i,compMode;                          

    /* fill soundex code with '00...' */
    for (i=0;i<SOUNDEX_LENGTH;i++)
       SoundexCode[i]='0';
    SoundexCode[i]=0;

    if (!XPP_IS_CHAR(_partype(paramList,1))) {
       _retclen(paramList, SoundexCode, SOUNDEX_LENGTH);
       return;
    }
    if (XPP_IS_NUM(_partype(paramList,2))) {
       compMode=_parnl(paramList,2);
    }
    else {
       compMode=SOUNDEX_CLIPPER;
    }
    if ((chWord = _conParam(paramList,1,NULL)) != NULLCONTAINER) {
       if (_conRLockC(chWord,&Word,&wordSize) == XPP_ERR_NONE) {

          _SoundexInC( Word, wordSize,SoundexCode,compMode);
             _conUnlockC(chWord);

       }
       _conRelease(chWord);
    }
    /* this function always returns a value */
    _retclen(paramList, SoundexCode, SOUNDEX_LENGTH);
}

/* 
   DIFFERENCE(cInput1, cInput2, [nModus]) -> nResult
   Prepares Xbase++ parameter and calls _DifferenceInC.

   INPUT:   STRING1,STRING2(references are ignored), 
            NUMERIC (compatibility mode, optional, default.:Clipper)
   RETURN:  INTEGER 
            number of equalities of the soundex codes of both strings.
            0=min, 4=max
	    Note: In case of short strings the soundex code may contain
            '0's, and DIFFERENCE will find as more equalities as there are.

   Note: This function is usable from Xbase++ only.
*/                      

XPPRET XPPENTRY DIFFERENCE(XppParamList paramList)
{
char *Word1,*Word2;                /* pointer to passed strings */
ULONG wordSize1,wordSize2;         /* length of strings */
ContainerHandle chWord1,chWord2;   /* container handles for strings */
int compMode;                            
ULONG result=0;                    


    if (!XPP_IS_CHAR(_partype(paramList,1)) || \
        !XPP_IS_CHAR(_partype(paramList,2))) {
       _retnl(paramList, 0);
       return;
    }
    if (XPP_IS_NUM(_partype(paramList,3))) {
       compMode=_parnl(paramList,3);
    }
    else {
       compMode=SOUNDEX_CLIPPER;
    }
    if ((chWord1 = _conParam(paramList,1,NULL)) != NULLCONTAINER) {
       if (_conRLockC(chWord1,&Word1,&wordSize1) == XPP_ERR_NONE) {

          if ((chWord2 = _conParam(paramList,2,NULL)) != NULLCONTAINER) {
             if (_conRLockC(chWord2,&Word2,&wordSize2) == XPP_ERR_NONE) {

                result=_DifferenceInC( Word1, wordSize1,Word2,wordSize2,compMode);
                _conUnlockC(chWord2);

             }
	     _conRelease(chWord2);
          }
          _conUnlockC(chWord1);
       }
       _conRelease(chWord1);
    }
    _retnl(paramList, result);
}







