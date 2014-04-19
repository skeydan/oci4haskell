{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main (
    main
) where

import GHC.ForeignPtr
import Oci

main = do

  let connstr = "CS1_55"

  envHandle <- envCreate mode_default
  print envHandle

  errHandle <- handleAlloc (castForeignPtr envHandle :: OciObjHandle) htype_error  >>= \h ->
    return (castForeignPtr h :: OciErrorHandle)
  print errHandle

  serverHandle <- handleAlloc (castForeignPtr envHandle :: OciObjHandle) htype_server >>= \h ->
    return (castForeignPtr h :: OciServerHandle)
  print serverHandle

  svcContextHandle <- handleAlloc (castForeignPtr envHandle :: OciObjHandle) htype_servicecontext >>= \h ->
    return (castForeignPtr h :: OciSvcCtxHandle)
  print svcContextHandle

  serverHandle <- serverAttach serverHandle errHandle connstr mode_conn_default
  print serverHandle

  svcContextHandle <- attrSet svcContextHandle htype_servicecontext serverHandle 0 htype_server errHandle



  print "attached?"
  return serverHandle

{-
  /* set attribute server context in the service context */
  (void) OCIAttrSet( (dvoid *) svchp, OCI_HTYPE_SVCCTX, (dvoid *)srvhp,
                     (ub4) 0, OCI_ATTR_SERVER, (OCIError *) errhp);

  (void) OCIHandleAlloc((dvoid *) envhp, (dvoid **)&authp,
                        (ub4) OCI_HTYPE_SESSION, (size_t) 0, (dvoid **) 0);

  -}


