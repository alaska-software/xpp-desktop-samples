//////////////////////////////////////////////////////////////////////
//
//  XFENC.PRG
//
//  Copyright:
//       Alaska Software, (c) 2002-2025. All rights reserved.         
//  
//  Contents:
//       Crypting a file
//  Remarks:
//       The sample demonstrates how to encrypt data of a file.
//       Optionally, a generated key is used. You should write down the key to be
//       able to decrypt the file.
//////////////////////////////////////////////////////////////////////


#include "crypt.ch"

FUNCTION MAIN(cSrcFile, cTargetFile, cKey)
LOCAL oKey

  ? "XFENC - a file encrypter "
  ? "Usage: <source file> <target file> [<key>]"

  IF PCount() < 2
     RETURN 1
  ENDIF

  IF cKey = NIL
     /*
      * generate a key for encryption
      */
     oKey := SecureKey():generate()
     ? "Please write down this key:", oKey:toString(, ":")
     ?
  ELSE
     oKey := SecureKey():new()
     oKey:setStrKey(cKey)
  ENDIF

RETURN EncryptFile(cSrcFile, cTargetFile, oKey)

FUNCTION EncryptFile(cSrcFile, cTargetFile, oKey)
LOCAL oAes
LOCAL oFile
LOCAL nRet := 0
LOCAL cbErr

  /*
   * use AES as CryptProvider
   */
  oAes := AesCrypt():new(oKey)
  /*
   * use CryptFile()-class to encrypt file using the passed key
   */
  oFile := CryptFile():new(oAes, oKey)

  cbErr := ErrorBlock( {|e| Break(e)})
  BEGIN SEQUENCE
      IF oFile:encrypt(cSrcFile, cTargetFile)
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
