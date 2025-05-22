//////////////////////////////////////////////////////////////////////
//
//  XFDEC.PRG
//
//  Copyright:
//       Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//       Decrypting a file
//  Remarks:
//       The sample demonstrates how to decrypt data of a file.
//////////////////////////////////////////////////////////////////////


#include "crypt.ch"

FUNCTION MAIN(cSrcFile, cTargetFile, cKey)
LOCAL oKey
LOCAL nRet
  ? "XFDEC - a file decrypter "
  ? "Usage: <source file> <target file> <key>"
  IF PCount() < 3
     RETURN 1
  ENDIF
  nRet := DecryptFile(cSrcFile, cTargetFile, cKey)

RETURN nRet


FUNCTION DecryptFile(cSrcFile, cTargetFile, cKey)
LOCAL oKey
LOCAL oFile
LOCAL nRet := 0
LOCAL cbErr

  /*
   * use SecureKey():setStrKey() to convert a hexadecimal string key
   * to a key object
   */
  oKey := SecureKey():new()
  oKey:setStrKey(cKey)
  /*
   * use CryptFile()-class to decrypt file using the passed key
   */
  oFile := CryptFile():new(, oKey)

  cbErr := ErrorBlock( {|e| Break(e)})
  BEGIN SEQUENCE
      IF oFile:decrypt(cSrcFile, cTargetFile)
         ? cTargetFile + " successfully created."
      ELSE
         ? "Error creating " + cTargetFile
      ENDIF
  RECOVER USING oError
      /*
       * error handling of errors during encryption
       */
      ErrorBlock(cbErr)
      IF oError:osCode != 0
          nRet := oError:osCode
         ? oError:description, DosErrorMessage(nRet)
      ELSE
         ? oError:description
      ENDIF
  END SEQUENCE
  ErrorBlock(cbErr)

  oKey:destroy()

RETURN nRet
