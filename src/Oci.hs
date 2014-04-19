{-# LANGUAGE CPP, ForeignFunctionInterface, DeriveDataTypeable #-}
module Oci (

  module Ocic,
  envCreate,
  handleAlloc,
  serverAttach,
  OciObjHandle,
  OciEnvHandle,
  OciErrorHandle,
  OciSvcCtxHandle,
  OciStmtHandle,
  OciBindHandle,
  OciDefineHandle,
  OciDescribeHandle,
  OciServerHandle

) where
import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Foreign.C.String
import Foreign.ForeignPtr
import Control.Exception
import Data.Typeable
import Control.Monad

import Ocic
{-
> foreign import ccall "oci.h OCIHandleAlloc" ociHandleAlloc ::
>   OCIHandle -> Ptr OCIHandle -> CInt -> CInt -> Ptr a -> IO CInt
>
> handleAlloc :: CInt -> OCIHandle -> IO OCIHandle
> handleAlloc handleType env = alloca $ \ptr -> do
>   rc <- ociHandleAlloc env ptr handleType 0 nullPtr
>   if rc < 0
>     then throwOCI (OCIException rc "allocate handle")
>     else peek ptr
-}


type OciObjHandle = ForeignPtr OciObj
type OciEnvHandle = ForeignPtr OciEnv
type OciErrorHandle = ForeignPtr OciError
type OciSvcCtxHandle = ForeignPtr OciSvcCtx
type OciStmtHandle = ForeignPtr OciStmt
type OciBindHandle = ForeignPtr OciBind
type OciDefineHandle = ForeignPtr OciDefine
type OciDescribeHandle = ForeignPtr OciDescribe
type OciServerHandle = ForeignPtr OciServer

data OciException = OciException {
                      getCode :: Int,
                      getMessage :: String
                      }
  deriving (Show, Typeable)

instance Exception OciException

checkErr :: OCIErrorRet -> OciErrorPtr -> IO ()
checkErr status errhp
  | status == oci_success = return ()
  | status == oci_error = do
     allocaBytes 512 $ \errstrp -> do
       alloca $ \errcodep -> do
         ret <- ociErrorGet errhp 1 nullPtr errcodep errstrp (fromIntegral 512) htype_error
         errstr <- peek errstrp >>= \cstr -> peekCString cstr
         errcode <- liftM fromIntegral (peek errcodep)
         throw (OciException (fromIntegral errcode) errstr)
  | status == oci_no_data = throw (OciException (fromIntegral (getRet oci_no_data)) "No data")
  | status == oci_need_data = throw (OciException (fromIntegral (getRet oci_need_data)) "Need data")
  | status == oci_invalid_handle = throw (OciException (fromIntegral (getRet oci_invalid_handle)) "Invalid handle")
  | status == oci_still_executing = throw (OciException (fromIntegral (getRet oci_still_executing)) "Still executing")

envCreate :: OCIInitializeMode -> IO OciEnvHandle
envCreate mode = alloca $ \ptr -> do
  ret <- ociEnvCreate ptr mode nullPtr nullFunPtr nullFunPtr nullFunPtr 0 nullPtr
  if ret /= getRet (oci_success)
    then throw (OciException (fromIntegral ret) "Unable to create environment")
    else do
      handle <- peek ptr
      print $ "env handle: " ++ show handle
      newForeignPtr finalizerFree handle

handleAlloc :: OciObjHandle -> OCIHandleType -> IO OciObjHandle
handleAlloc parent htype = withForeignPtr parent $ \parenthp -> do
                          alloca $ \ptr -> do
                            ret <- ociHandleAlloc parenthp ptr htype 0 nullPtr
                            if ret /= getRet (oci_success)
                              then throw (OciException (fromIntegral ret) "Unable to allocate handle")
                              else do
                                handle <- peek ptr
                                print $ show htype ++ ": " ++ show handle
                                newForeignPtr finalizerFree handle

serverAttach :: OciServerHandle -> OciErrorHandle -> String -> OCIConnectionPoolMode -> IO OciServerHandle
serverAttach srvrHandle errHandle connectString mode =
      withForeignPtr errHandle $ \errhp -> do
        withForeignPtr srvrHandle $ \srvrhp -> do
          withCStringLen connectString $ \cstrlen -> do
            ret <- ociServerAttach srvrhp errhp (fst cstrlen) ((fromIntegral . snd) cstrlen) mode
            checkErr (OCIErrorRet ret) errhp >> return srvrHandle

attrSet :: OciObjHandle-> OCIHandleType -> OciObjHandle -> Int -> OCIAttrType -> OciErrorPtr
attrSet targetHandle htype attrHandle attrLen attrType =
















