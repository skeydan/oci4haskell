{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Ocic

(module Ocihsc,
  OciObj,
  OciEnv,
  OciError,
  OciSvcCtx,
  OciStmt,
  OciBind,
  OciDefine,
  OciDescribe,
  OciServer,
  OciObjPtr,
  OciEnvPtr,
  OciErrorPtr,
  OciSvcCtxPtr,
  OciStmtPtr,
  OciBindPtr,
  OciDefinePtr,
  OciDescribePtr,
  OciServerPtr,
  ociHandleAlloc,
  ociEnvCreate,
  ociServerAttach,
  ociErrorGet

) where

import Foreign.C.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Ocihsc

data OciObj
data OciEnv
data OciError
data OciSvcCtx
data OciStmt
data OciBind
data OciDefine
data OciDescribe
data OciServer

type OciObjPtr = Ptr OciObj
type OciEnvPtr = Ptr OciEnv
type OciErrorPtr = Ptr OciError
type OciSvcCtxPtr = Ptr OciSvcCtx
type OciStmtPtr = Ptr OciStmt
type OciBindPtr = Ptr OciBind
type OciDefinePtr = Ptr OciDefine
type OciDescribePtr = Ptr OciDescribe
type OciServerPtr = Ptr OciServer


{-
sword OCIEnvCreate (OCIEnv **envhpp, ub4 mode, const void *ctxp, const void *(*malocfp) (void *ctxp, size_t size),
                    const void *(*ralocfp) (void *ctxp, void *memptr, size_t newsize), const void (*mfreefp)
                    (void *ctxp, void *memptr)), size_t xtramemsz, void **usrmempp);
-}
foreign import ccall unsafe "oci.h OCIEnvCreate"
     ociEnvCreate :: Ptr OciEnvPtr -> OCIInitializeMode -> Ptr a -> FunPtr b -> FunPtr c
                      -> FunPtr d -> CUInt -> Ptr (Ptr e) -> IO CInt


{-
Bind handle and define handles are allocated with respect to a statement handle.
All other handles are allocated with respect to an environment handle which is passed in as a parent handle.
sword OCIHandleAlloc (const void *parenth, void **hndlpp, ub4 type, size_t xtramem_sz, void **usrmempp);
-}
foreign import ccall unsafe "oci.h OCIHandleAlloc"
     ociHandleAlloc :: Ptr a -> Ptr OciObjPtr -> OCIHandleType -> CUInt -> Ptr a -> IO CInt

{-
sword OCIServerAttach (OCIServer *srvhp, OCIError *errhp, const OraText *dblink,
                       sb4 dblink_len, ub4 mode);
-}
foreign import ccall unsafe "oci.h OCIServerAttach"
     ociServerAttach :: OciServerPtr -> OciErrorPtr -> CString -> CInt -> OCIConnectionPoolMode -> IO CInt

{-
sword OCIErrorGet (void *hndlp, ub4 recordno, OraText *sqlstate, ub4 *errcodep,
                   OraText *bufp, ub4 bufsiz, ub4 type);
hndlp (IN) - the error handle, in most cases, or the environment handle (for errors on OCIEnvInit(), OCIHandleAlloc()).
-}

foreign import ccall unsafe "oci.h OCIErrorGet"
    ociErrorGet :: Ptr a -> CUInt -> Ptr CString -> Ptr CUInt -> Ptr CString -> CUInt -> OCIHandleType -> IO CInt

{-
sword   OCIAttrSet (void  *trgthndlp, ub4 trghndltyp, void  *attributep,
                    ub4 size, ub4 attrtype, OCIError *errhp);
size needs to be passed in only for text* values
-}
foreign import ccall unsafe "oci.h OCIAttrSet"
    ociAttrSet :: Ptr a -> OCIHandleType -> Ptr a -> CUInt -> OCIAttrType -> OciErrorPtr


