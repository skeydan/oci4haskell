{- hsc2hs -I /usr/include/oracle/12.1/client64/ Ocihsc.hsc -}

{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Ocihsc
where
import Foreign
import Foreign.C.Types

#include <oci.h>

newtype OCIInitializeMode = OCIInitializeMode { getOCIInitializeMode :: CUInt }
  deriving (Eq, Show)

#{enum OCIInitializeMode, OCIInitializeMode
  , mode_default  =    OCI_DEFAULT
  , mode_threaded =    OCI_THREADED
  , mode_object   =    OCI_OBJECT
  , mode_events   =    OCI_EVENTS
 }

newtype OCIConnectionPoolMode = OCIConnectionPoolMode { getOCIConnectionPoolMode :: CUInt }
  deriving (Eq, Show)

#{enum OCIConnectionPoolMode, OCIConnectionPoolMode
  , mode_conn_default  =    OCI_DEFAULT
  , mode_conn_cpool    =    OCI_CPOOL
 }

newtype OCIErrorRet = OCIErrorRet { getRet :: CInt }
  deriving (Eq, Show)

#{enum OCIErrorRet, OCIErrorRet
  , oci_success           = OCI_SUCCESS
  , oci_error             = OCI_ERROR
  , oci_no_data           = OCI_NO_DATA
  , oci_invalid_handle    = OCI_INVALID_HANDLE
  , oci_need_data         = OCI_NEED_DATA
  , oci_still_executing   = OCI_STILL_EXECUTING
}

newtype OCIHandleType = OCIHandleType { getOCIHandleType :: CUInt }
  deriving (Eq, Show)

#{enum OCIHandleType, OCIHandleType
  , htype_env             =   OCI_HTYPE_ENV
  , htype_error           =   OCI_HTYPE_ERROR
  , htype_servicecontext  =   OCI_HTYPE_SVCCTX
  , htype_statement       =   OCI_HTYPE_STMT
  , htype_bind            =   OCI_HTYPE_BIND
  , htype_define          =   OCI_HTYPE_DEFINE
  , htype_describe        =   OCI_HTYPE_DESCRIBE
  , htype_server          =   OCI_HTYPE_SERVER
  , htype_session         =   OCI_HTYPE_SESSION
  , htype_authinfo        =   OCI_HTYPE_AUTHINFO
  , htype_trans           =   OCI_HTYPE_TRANS
  , htype_complexobject   =   OCI_HTYPE_COMPLEXOBJECT
  , htype_security        =   OCI_HTYPE_SECURITY
  , htype_subscription    =   OCI_HTYPE_SUBSCRIPTION
  , htype_dirpat_ctx      =   OCI_HTYPE_DIRPATH_CTX
  , htype_dirpath_column_array  =   OCI_HTYPE_DIRPATH_COLUMN_ARRAY
  , htype_dirpath_stream        =   OCI_HTYPE_DIRPATH_STREAM
  , htype_proc                  =   OCI_HTYPE_PROC
  , htype_dirpath_fn_ctx        =   OCI_HTYPE_DIRPATH_FN_CTX
  , htype_dirpath_fn_col_array  =   OCI_HTYPE_DIRPATH_FN_COL_ARRAY
  , htype_xadsession            =   OCI_HTYPE_XADSESSION
  , htype_xadtable              =   OCI_HTYPE_XADTABLE
  , htype_xadfield              =   OCI_HTYPE_XADFIELD
  , htype_xadgranule            =   OCI_HTYPE_XADGRANULE
  , htype_xadrecord             =   OCI_HTYPE_XADRECORD
  , htype_xadio                 =   OCI_HTYPE_XADIO
  , htype_cpool                 =   OCI_HTYPE_CPOOL
  , htype_spool                 =   OCI_HTYPE_SPOOL
  , htype_admin                 =   OCI_HTYPE_ADMIN
  , htype_event                 =   OCI_HTYPE_EVENT
}

newtype OCIAttrType = OCIAttrType { getOCIAttrType :: CUInt }
  deriving (Eq, Show)

#{enum OCIAttrType, OCIAttrType
  , attr_env                    =   OCI_ATTR_ENV
  , attr_server                 =   OCI_ATTR_SERVER
  , attr_session                =   OCI_ATTR_SESSION
}


