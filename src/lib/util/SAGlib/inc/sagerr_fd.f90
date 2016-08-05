!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE sagerr_fd

!==== SAG error codes

  INTEGER, PARAMETER :: SAG_ERR_NULL   =  0
  INTEGER, PARAMETER :: SAG_ERR_UNKN   =  1
  INTEGER, PARAMETER :: SAG_ERR_OPENED =  2
  INTEGER, PARAMETER :: SAG_ERR_OPEN   =  3
  INTEGER, PARAMETER :: SAG_ERR_TEMP   =  4
  INTEGER, PARAMETER :: SAG_ERR_IPDAT  =  5
  INTEGER, PARAMETER :: SAG_ERR_IPGRD  =  6
  INTEGER, PARAMETER :: SAG_ERR_IPNAM  =  7
  INTEGER, PARAMETER :: SAG_ERR_READ   =  8
  INTEGER, PARAMETER :: SAG_ERR_WRITE  =  9
  INTEGER, PARAMETER :: SAG_ERR_MXGRD  = 10
  INTEGER, PARAMETER :: SAG_ERR_MXNAM  = 11
  INTEGER, PARAMETER :: SAG_ERR_MXFLD  = 12
  INTEGER, PARAMETER :: SAG_ERR_INVALID= 13
  INTEGER, PARAMETER :: SAG_ERR_IVAR   = 14
  INTEGER, PARAMETER :: SAG_ERR_IFLD   = 15
  INTEGER, PARAMETER :: SAG_ERR_NOTFOUND=16
  INTEGER, PARAMETER :: SAG_ERR_STATUS = 17
  INTEGER, PARAMETER :: SAG_ERR_TYPE   = 18
  INTEGER, PARAMETER :: SAG_ERR_SKIP   = 19
  INTEGER, PARAMETER :: SAG_ERR_FIND   = 20
  INTEGER, PARAMETER :: SAG_ERR_MALLOC = 21
  INTEGER, PARAMETER :: SAG_ERR_LUN    = 22
  INTEGER, PARAMETER :: SAG_ERR_COPY   = 23
  INTEGER, PARAMETER :: SAG_ERR_BACK   = 24
  INTEGER, PARAMETER :: SAG_ERR_INCOMP = 25
  INTEGER, PARAMETER :: SAG_ERR_IPNODE = 26
  INTEGER, PARAMETER :: SAG_ERR_IPDATA = 27
  INTEGER, PARAMETER :: SAG_ERR_MXNODE = 28
  INTEGER, PARAMETER :: SAG_ERR_MXDATA = 29
  INTEGER, PARAMETER :: SAG_ERR_MXTRI  = 30
  INTEGER, PARAMETER :: SAG_ERR_IPTRI  = 31
  INTEGER, PARAMETER :: SAG_ERR_IPEDGE = 32
  INTEGER, PARAMETER :: SAG_ERR_IPCELL = 33
  INTEGER, PARAMETER :: SAG_ERR_NONODE = 34
  INTEGER, PARAMETER :: SAG_ERR_NOEDGE = 35
  INTEGER, PARAMETER :: SAG_ERR_IPCONN = 36
  INTEGER, PARAMETER :: SAG_ERR_NOTRI  = 37
  INTEGER, PARAMETER :: SAG_ERR_IPENOD = 38
  INTEGER, PARAMETER :: SAG_ERR_MIBE   = 39
  INTEGER, PARAMETER :: SAG_ERR_NODATA = 40
  INTEGER, PARAMETER :: SAG_ERR_NOCNTR = 41
  INTEGER, PARAMETER :: SAG_ERR_IPPTS  = 42
  INTEGER, PARAMETER :: SAG_ERR_IPLNS  = 43
  INTEGER, PARAMETER :: SAG_ERR_MXPTS  = 44
  INTEGER, PARAMETER :: SAG_ERR_MXLNS  = 45
  INTEGER, PARAMETER :: SAG_ERR_IPFIL  = 46
  INTEGER, PARAMETER :: SAG_ERR_IPLEV  = 47
  INTEGER, PARAMETER :: SAG_ERR_USER   = 48
  INTEGER, PARAMETER :: SAG_ERR_NOLEV  = 49
  INTEGER, PARAMETER :: SAG_ERR_IPBLK  = 50
  INTEGER, PARAMETER :: SAG_ERR_FTYPE  = 51
  INTEGER, PARAMETER :: SAG_ERR_BLKNAM = 52
  INTEGER, PARAMETER :: SAG_ERR_WRTPOS = 53
  INTEGER, PARAMETER :: SAG_ERR_CLOSED = 54
  INTEGER, PARAMETER :: SAG_ERR_NCELL  = 55
  INTEGER, PARAMETER :: SAG_ERR_CALLBACK = 56
  INTEGER, PARAMETER :: SAG_ERR_FSIZE  = 57
  INTEGER, PARAMETER :: SAG_ERR_NYI    = 2056

!==== SAG status codes

  INTEGER, PARAMETER :: SAG_TEMP     = -1
  INTEGER, PARAMETER :: SAG_CLOSED   =  0
  INTEGER, PARAMETER :: SAG_HEAD_REC =  1
  INTEGER, PARAMETER :: SAG_GRID_REC =  2
  INTEGER, PARAMETER :: SAG_DATA_REC =  3
  INTEGER, PARAMETER :: SAG_EOF_REC  =  4
  INTEGER, PARAMETER :: SAG_BLCK_REC =  5
  INTEGER, PARAMETER :: SAG_AUX_REC  =  6

END MODULE sagerr_fd
