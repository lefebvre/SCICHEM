# Main program is metsci

PROG =	metsci

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = bin/met2sci/SCIPRE/ExtractOSdata.f90 bin/met2sci/SCIPRE/OSread_fd.f90 \
	  bin/met2sci/SCIPRE/MaxChar_fd.f90 \
	  bin/met2sci/SCIPRE/MetSCIparam_fd.f90 \
	  bin/met2sci/SCIPRE/FileList_fd.f90 \
	  bin/met2sci/SCIPRE/WriteUAheader.f90 \
	  bin/met2sci/SCIPRE/ParsePathwayInput.f90 \
	  bin/met2sci/SCIPRE/MetSCIutil.f90 \
	  bin/met2sci/SCIPRE/ExtractSFCdata.f90 \
	  bin/met2sci/SCIPRE/WriteOSdata.f90 \
	  bin/met2sci/SCIPRE/block2_irnd.f90 \
	  bin/met2sci/SCIPRE/ReportError.f90 \
	  bin/met2sci/SCIPRE/WriteUAdata.f90 \
	  bin/met2sci/SCIPRE/met2sci_fi.f90 \
	  bin/met2sci/SCIPRE/ExtractUAdata.f90 \
	  bin/met2sci/SCIPRE/error_fd.f90 bin/met2sci/SCIPRE/met2sci.f90 \
	  bin/met2sci/SCIPRE/WriteSFCheader.f90 \
	  bin/met2sci/SCIPRE/WriteSFCdata.f90 \
	  bin/met2sci/SCIPRE/WriteOSheader.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

SRCS_for = bin/met2sci/SCIPRE/SetUAQA.for bin/met2sci/SCIPRE/SetUA.for \
	  bin/met2sci/SCIPRE/GetUAloc.for \
	  bin/met2sci/SCIPRE/SetAERMETdata.for \
	  bin/met2sci/SCIPRE/mod_AsosCommDates.for \
	  bin/met2sci/SCIPRE/SetSFC.for bin/met2sci/SCIPRE/SetOS.for \
	  bin/met2sci/AERMET/FMTCRD.for bin/met2sci/AERMET/FLIWK1.for \
	  bin/met2sci/AERMET/OSSMRY.for bin/met2sci/AERMET/OSREAD.for \
	  bin/met2sci/AERMET/OSTEST.for bin/met2sci/AERMET/OSPRNT.for \
	  bin/met2sci/AERMET/GREG.for bin/met2sci/AERMET/SFCCRD2.for \
	  bin/met2sci/AERMET/CLMCRD.for bin/met2sci/AERMET/UAUDIT.for \
	  bin/met2sci/AERMET/GMTLST.for bin/met2sci/AERMET/AUTCHK.for \
	  bin/met2sci/AERMET/OSPATH.for bin/met2sci/AERMET/FLIWK2.for \
	  bin/met2sci/AERMET/OSCHK.for bin/met2sci/AERMET/BANNER.for \
	  bin/met2sci/AERMET/JBCARD.for bin/met2sci/AERMET/UAQAST.for \
	  bin/met2sci/AERMET/HTCALC.for bin/met2sci/AERMET/COMPDT.for \
	  bin/met2sci/AERMET/P2MSUB.for bin/met2sci/AERMET/MPPBL.for \
	  bin/met2sci/AERMET/D3280L.for bin/met2sci/AERMET/ICHRND.for \
	  bin/met2sci/AERMET/OSTRA.for bin/met2sci/AERMET/OAUDIT.for \
	  bin/met2sci/AERMET/OSQACK.for bin/met2sci/AERMET/DTCRD.for \
	  bin/met2sci/AERMET/SECCRD2.for bin/met2sci/AERMET/EQ_CCVR.for \
	  bin/met2sci/AERMET/HEAT.for bin/met2sci/AERMET/CHRCRD2.for \
	  bin/met2sci/AERMET/FETCH.for bin/met2sci/AERMET/DATCRD.for \
	  bin/met2sci/AERMET/MERGE.for bin/met2sci/AERMET/RDHUSW.for \
	  bin/met2sci/AERMET/CVG.for bin/met2sci/AERMET/SUMRY1.for \
	  bin/met2sci/AERMET/XDTCRD.for bin/met2sci/AERMET/CHROND.for \
	  bin/met2sci/AERMET/UATRA.for bin/met2sci/AERMET/SFQAST.for \
	  bin/met2sci/AERMET/SMTHZI.for bin/met2sci/AERMET/LOCCRD.for \
	  bin/met2sci/AERMET/OSHRAV.for bin/met2sci/AERMET/CLHT.for \
	  bin/met2sci/AERMET/UAPATH.for bin/met2sci/AERMET/VALCRD.for \
	  bin/met2sci/AERMET/D6201L.for bin/met2sci/AERMET/GETWRD.for \
	  bin/met2sci/AERMET/WRTCRD.for bin/met2sci/AERMET/UACHK.for \
	  bin/met2sci/AERMET/SUBST.for bin/met2sci/AERMET/UAMOVE.for \
	  bin/met2sci/AERMET/GETFSL.for bin/met2sci/AERMET/FDPATH.for \
	  bin/met2sci/AERMET/HDPROC.for bin/met2sci/AERMET/OSFILL.for \
	  bin/met2sci/AERMET/OSCARD.for bin/met2sci/AERMET/UAWNDW.for \
	  bin/met2sci/AERMET/GETFIL.for bin/met2sci/AERMET/CHRCRD.for \
	  bin/met2sci/AERMET/SFCHK.for bin/met2sci/AERMET/NETRAD.for \
	  bin/met2sci/AERMET/RDLREC.for bin/met2sci/AERMET/MRHDR.for \
	  bin/met2sci/AERMET/MANDEL.for bin/met2sci/AERMET/CBLHT.for \
	  bin/met2sci/AERMET/SFQASM.for bin/met2sci/AERMET/CALMS.for \
	  bin/met2sci/AERMET/NR_ANG.for bin/met2sci/AERMET/SCNGEN.for \
	  bin/met2sci/AERMET/FLWRK1.for bin/met2sci/AERMET/LATLON.for \
	  bin/met2sci/AERMET/DATER.for bin/met2sci/AERMET/SFCARD.for \
	  bin/met2sci/AERMET/OTHHDR.for bin/met2sci/AERMET/SFTRA.for \
	  bin/met2sci/AERMET/OSDUMP.for bin/met2sci/AERMET/UAEXT.for \
	  bin/met2sci/AERMET/NWSHGT.for bin/met2sci/AERMET/INCRAD.for \
	  bin/met2sci/AERMET/AVGCRD.for bin/met2sci/AERMET/TEST.for \
	  bin/met2sci/AERMET/AERSURF.for bin/met2sci/AERMET/HUSWX.for \
	  bin/met2sci/AERMET/MPCARD.for bin/met2sci/AERMET/HUMID.for \
	  bin/met2sci/AERMET/TDPEST.for bin/met2sci/AERMET/RDSAMS.for \
	  bin/met2sci/AERMET/FLWRK2.for bin/met2sci/AERMET/RDISHD.for \
	  bin/met2sci/AERMET/FLOS.for bin/met2sci/AERMET/MRPATH.for \
	  bin/met2sci/AERMET/SUMHF.for bin/met2sci/AERMET/VARCRD.for \
	  bin/met2sci/AERMET/UACARD.for bin/met2sci/AERMET/ISHWX.for \
	  bin/met2sci/AERMET/SETHUS.for bin/met2sci/AERMET/OSSWAP.for \
	  bin/met2sci/AERMET/MPPROC.for bin/met2sci/AERMET/GET620.for \
	  bin/met2sci/AERMET/MDCARD.for bin/met2sci/AERMET/YR4TOYR2.for \
	  bin/met2sci/AERMET/D3280H.for bin/met2sci/AERMET/SFEXST.for \
	  bin/met2sci/AERMET/FNDCOMDT.for bin/met2sci/AERMET/RHOCAL.for \
	  bin/met2sci/AERMET/OSRANGE.for bin/met2sci/AERMET/SECCRD.for \
	  bin/met2sci/AERMET/D6201H.for bin/met2sci/AERMET/ERRHDL.for \
	  bin/met2sci/AERMET/INTEQA.for bin/met2sci/AERMET/OSNEXT.for \
	  bin/met2sci/AERMET/SAMWX.for bin/met2sci/AERMET/DOCLDS.for \
	  bin/met2sci/AERMET/SAUDIT.for bin/met2sci/AERMET/DEF256.for \
	  bin/met2sci/AERMET/SETUP.for bin/met2sci/AERMET/HGTCRD.for \
	  bin/met2sci/AERMET/GETSFC.for bin/met2sci/AERMET/FLOPEN.for \
	  bin/met2sci/AERMET/OSSUMS.for bin/met2sci/AERMET/MODEL.for \
	  bin/met2sci/AERMET/PTGRAD.for bin/met2sci/AERMET/SUNDAT.for \
	  bin/met2sci/AERMET/AUDIT.for bin/met2sci/AERMET/SBLHT.for \
	  bin/met2sci/AERMET/LWRUPR.for bin/met2sci/AERMET/SFCCRD.for \
	  bin/met2sci/AERMET/MPOUT.for bin/met2sci/AERMET/SFCWXX.for \
	  bin/met2sci/AERMET/MPHEAD.for bin/met2sci/AERMET/SFCCH2.for \
	  bin/met2sci/AERMET/PRESET.for bin/met2sci/AERMET/UAEXST.for \
	  bin/met2sci/AERMET/INTHF.for bin/met2sci/AERMET/UCALCO.for \
	  bin/met2sci/AERMET/CLOUDS.for bin/met2sci/AERMET/MPTEST.for \
	  bin/met2sci/AERMET/AERSURF2.for bin/met2sci/AERMET/HEADER.for \
	  bin/met2sci/AERMET/MRCARD.for bin/met2sci/AERMET/D028LV.for \
	  bin/met2sci/AERMET/FLSDG.for bin/met2sci/AERMET/GEO.for \
	  bin/met2sci/AERMET/MPMET.for bin/met2sci/AERMET/HTKEY.for \
	  bin/met2sci/AERMET/SETSAM.for bin/met2sci/AERMET/MIDNITE.for \
	  bin/met2sci/AERMET/GETASOS.for bin/met2sci/AERMET/STONUM.for \
	  bin/met2sci/AERMET/CUBIC.for bin/met2sci/AERMET/D144LV.for \
	  bin/met2sci/AERMET/DEFINE.for bin/met2sci/AERMET/THRESH1MIN.for \
	  bin/met2sci/AERMET/OSFILL2.for bin/met2sci/AERMET/RNGCRD.for \
	  bin/met2sci/AERMET/SFQATM.for bin/met2sci/AERMET/OSDTCD.for \
	  bin/met2sci/AERMET/SUMRY2.for bin/met2sci/AERMET/FLSFC.for \
	  bin/met2sci/AERMET/UAQASM.for bin/met2sci/AERMET/SFCCH.for \
	  bin/met2sci/AERMET/ASOSREC.for bin/met2sci/AERMET/BULKRI.for \
	  bin/met2sci/AERMET/REALQA.for bin/met2sci/AERMET/SFPATH.for \
	  bin/met2sci/AERMET/YR2TOYR4.for bin/met2sci/AERMET/FDKEY.for \
	  bin/met2sci/AERMET/READRL.for bin/met2sci/AERMET/OSQAST.for \
	  bin/met2sci/AERMET/D144HD.for bin/met2sci/AERMET/GETFLD.for \
	  bin/met2sci/AERMET/SFEXT.for bin/met2sci/AERMET/OSWRTE.for \
	  bin/met2sci/AERMET/UCALST.for bin/met2sci/AERMET/XTNDUA.for \
	  bin/met2sci/AERMET/MPFIN.for bin/met2sci/AERMET/PTAREA.for \
	  bin/met2sci/AERMET/VRCARD.for bin/met2sci/AERMET/HR0024.for

OBJS_for := $(subst .for,.o,$(SRCS_for))

OBJS :=  $(OBJS_f90)  $(OBJS_for) 

DIRS = bin/met2sci/SCIPRE bin/met2sci/AERMET

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LNK) $(LNKFLAGS) $(PROG) $(OBJS) $(LIBS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< 

$(OBJS_for): %.o:$(filter /\%.for,$(SRCS_for))
	$(F77) -o $@ $(F77FLAGS) $< 


bin/met2sci/SCIPRE/MaxChar_fd.o:$(BD)/bin/met2sci/SCIPRE/MaxChar_fd.f90 

bin/met2sci/SCIPRE/error_fd.o:$(BD)/bin/met2sci/SCIPRE/error_fd.f90  \
	  bin/met2sci/SCIPRE/MaxChar_fd.o

bin/met2sci/SCIPRE/FileList_fd.o:$(BD)/bin/met2sci/SCIPRE/FileList_fd.f90  \
	  bin/met2sci/SCIPRE/MaxChar_fd.o

bin/met2sci/SCIPRE/MetSCIparam_fd.o:$(BD)/bin/met2sci/SCIPRE/MetSCIparam_fd.f90 

bin/met2sci/SCIPRE/OSread_fd.o:$(BD)/bin/met2sci/SCIPRE/OSread_fd.f90 

bin/met2sci/SCIPRE/met2sci_fi.o:$(BD)/bin/met2sci/SCIPRE/met2sci_fi.f90  \
	  bin/met2sci/SCIPRE/error_fd.o bin/met2sci/SCIPRE/FileList_fd.o \
	  bin/met2sci/SCIPRE/MaxChar_fd.o bin/met2sci/SCIPRE/MetSCIparam_fd.o \
	  bin/met2sci/SCIPRE/OSread_fd.o

bin/met2sci/SCIPRE/ExtractOSdata.o:$(BD)/bin/met2sci/SCIPRE/ExtractOSdata.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/SetUAQA.o:$(BD)/bin/met2sci/SCIPRE/SetUAQA.for 

bin/met2sci/SCIPRE/SetUA.o:$(BD)/bin/met2sci/SCIPRE/SetUA.for 

bin/met2sci/SCIPRE/GetUAloc.o:$(BD)/bin/met2sci/SCIPRE/GetUAloc.for 

bin/met2sci/SCIPRE/WriteUAheader.o:$(BD)/bin/met2sci/SCIPRE/WriteUAheader.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/ParsePathwayInput.o:$(BD)/bin/met2sci/SCIPRE/ParsePathwayInput.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o bin/met2sci/SCIPRE/MetSCIparam_fd.o

bin/met2sci/SCIPRE/MetSCIutil.o:$(BD)/bin/met2sci/SCIPRE/MetSCIutil.f90 

bin/met2sci/SCIPRE/ExtractSFCdata.o:$(BD)/bin/met2sci/SCIPRE/ExtractSFCdata.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/block2_irnd.o:$(BD)/bin/met2sci/SCIPRE/block2_irnd.f90 

bin/met2sci/SCIPRE/WriteOSdata.o:$(BD)/bin/met2sci/SCIPRE/WriteOSdata.f90  \
	  bin/met2sci/SCIPRE/block2_irnd.o bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/SetAERMETdata.o:$(BD)/bin/met2sci/SCIPRE/SetAERMETdata.for 

bin/met2sci/SCIPRE/ReportError.o:$(BD)/bin/met2sci/SCIPRE/ReportError.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/WriteUAdata.o:$(BD)/bin/met2sci/SCIPRE/WriteUAdata.f90  \
	  bin/met2sci/SCIPRE/block2_irnd.o bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/mod_AsosCommDates.o:$(BD)/bin/met2sci/SCIPRE/mod_AsosCommDates.for 

bin/met2sci/SCIPRE/ExtractUAdata.o:$(BD)/bin/met2sci/SCIPRE/ExtractUAdata.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/SetSFC.o:$(BD)/bin/met2sci/SCIPRE/SetSFC.for 

bin/met2sci/SCIPRE/SetOS.o:$(BD)/bin/met2sci/SCIPRE/SetOS.for 

bin/met2sci/SCIPRE/met2sci.o:$(BD)/bin/met2sci/SCIPRE/met2sci.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/WriteSFCheader.o:$(BD)/bin/met2sci/SCIPRE/WriteSFCheader.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/WriteSFCdata.o:$(BD)/bin/met2sci/SCIPRE/WriteSFCdata.f90  \
	  bin/met2sci/SCIPRE/block2_irnd.o bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/SCIPRE/WriteOSheader.o:$(BD)/bin/met2sci/SCIPRE/WriteOSheader.f90  \
	  bin/met2sci/SCIPRE/met2sci_fi.o

bin/met2sci/AERMET/FMTCRD.o:$(BD)/bin/met2sci/AERMET/FMTCRD.for 

bin/met2sci/AERMET/FLIWK1.o:$(BD)/bin/met2sci/AERMET/FLIWK1.for 

bin/met2sci/AERMET/OSSMRY.o:$(BD)/bin/met2sci/AERMET/OSSMRY.for 

bin/met2sci/AERMET/OSREAD.o:$(BD)/bin/met2sci/AERMET/OSREAD.for 

bin/met2sci/AERMET/OSTEST.o:$(BD)/bin/met2sci/AERMET/OSTEST.for 

bin/met2sci/AERMET/OSPRNT.o:$(BD)/bin/met2sci/AERMET/OSPRNT.for 

bin/met2sci/AERMET/GREG.o:$(BD)/bin/met2sci/AERMET/GREG.for 

bin/met2sci/AERMET/SFCCRD2.o:$(BD)/bin/met2sci/AERMET/SFCCRD2.for 

bin/met2sci/AERMET/CLMCRD.o:$(BD)/bin/met2sci/AERMET/CLMCRD.for 

bin/met2sci/AERMET/UAUDIT.o:$(BD)/bin/met2sci/AERMET/UAUDIT.for 

bin/met2sci/AERMET/GMTLST.o:$(BD)/bin/met2sci/AERMET/GMTLST.for 

bin/met2sci/AERMET/AUTCHK.o:$(BD)/bin/met2sci/AERMET/AUTCHK.for 

bin/met2sci/AERMET/OSPATH.o:$(BD)/bin/met2sci/AERMET/OSPATH.for 

bin/met2sci/AERMET/FLIWK2.o:$(BD)/bin/met2sci/AERMET/FLIWK2.for 

bin/met2sci/AERMET/OSCHK.o:$(BD)/bin/met2sci/AERMET/OSCHK.for 

bin/met2sci/AERMET/BANNER.o:$(BD)/bin/met2sci/AERMET/BANNER.for 

bin/met2sci/AERMET/JBCARD.o:$(BD)/bin/met2sci/AERMET/JBCARD.for 

bin/met2sci/AERMET/UAQAST.o:$(BD)/bin/met2sci/AERMET/UAQAST.for 

bin/met2sci/AERMET/HTCALC.o:$(BD)/bin/met2sci/AERMET/HTCALC.for 

bin/met2sci/AERMET/COMPDT.o:$(BD)/bin/met2sci/AERMET/COMPDT.for 

bin/met2sci/AERMET/P2MSUB.o:$(BD)/bin/met2sci/AERMET/P2MSUB.for 

bin/met2sci/AERMET/MPPBL.o:$(BD)/bin/met2sci/AERMET/MPPBL.for 

bin/met2sci/AERMET/D3280L.o:$(BD)/bin/met2sci/AERMET/D3280L.for 

bin/met2sci/AERMET/ICHRND.o:$(BD)/bin/met2sci/AERMET/ICHRND.for 

bin/met2sci/AERMET/OSTRA.o:$(BD)/bin/met2sci/AERMET/OSTRA.for 

bin/met2sci/AERMET/OAUDIT.o:$(BD)/bin/met2sci/AERMET/OAUDIT.for 

bin/met2sci/AERMET/OSQACK.o:$(BD)/bin/met2sci/AERMET/OSQACK.for 

bin/met2sci/AERMET/DTCRD.o:$(BD)/bin/met2sci/AERMET/DTCRD.for 

bin/met2sci/AERMET/SECCRD2.o:$(BD)/bin/met2sci/AERMET/SECCRD2.for 

bin/met2sci/AERMET/EQ_CCVR.o:$(BD)/bin/met2sci/AERMET/EQ_CCVR.for 

bin/met2sci/AERMET/HEAT.o:$(BD)/bin/met2sci/AERMET/HEAT.for 

bin/met2sci/AERMET/CHRCRD2.o:$(BD)/bin/met2sci/AERMET/CHRCRD2.for 

bin/met2sci/AERMET/FETCH.o:$(BD)/bin/met2sci/AERMET/FETCH.for 

bin/met2sci/AERMET/DATCRD.o:$(BD)/bin/met2sci/AERMET/DATCRD.for 

bin/met2sci/AERMET/MERGE.o:$(BD)/bin/met2sci/AERMET/MERGE.for 

bin/met2sci/AERMET/RDHUSW.o:$(BD)/bin/met2sci/AERMET/RDHUSW.for 

bin/met2sci/AERMET/CVG.o:$(BD)/bin/met2sci/AERMET/CVG.for 

bin/met2sci/AERMET/SUMRY1.o:$(BD)/bin/met2sci/AERMET/SUMRY1.for 

bin/met2sci/AERMET/XDTCRD.o:$(BD)/bin/met2sci/AERMET/XDTCRD.for 

bin/met2sci/AERMET/CHROND.o:$(BD)/bin/met2sci/AERMET/CHROND.for 

bin/met2sci/AERMET/UATRA.o:$(BD)/bin/met2sci/AERMET/UATRA.for 

bin/met2sci/AERMET/SFQAST.o:$(BD)/bin/met2sci/AERMET/SFQAST.for 

bin/met2sci/AERMET/SMTHZI.o:$(BD)/bin/met2sci/AERMET/SMTHZI.for 

bin/met2sci/AERMET/LOCCRD.o:$(BD)/bin/met2sci/AERMET/LOCCRD.for 

bin/met2sci/AERMET/OSHRAV.o:$(BD)/bin/met2sci/AERMET/OSHRAV.for 

bin/met2sci/AERMET/CLHT.o:$(BD)/bin/met2sci/AERMET/CLHT.for 

bin/met2sci/AERMET/UAPATH.o:$(BD)/bin/met2sci/AERMET/UAPATH.for 

bin/met2sci/AERMET/VALCRD.o:$(BD)/bin/met2sci/AERMET/VALCRD.for 

bin/met2sci/AERMET/D6201L.o:$(BD)/bin/met2sci/AERMET/D6201L.for 

bin/met2sci/AERMET/GETWRD.o:$(BD)/bin/met2sci/AERMET/GETWRD.for 

bin/met2sci/AERMET/WRTCRD.o:$(BD)/bin/met2sci/AERMET/WRTCRD.for 

bin/met2sci/AERMET/UACHK.o:$(BD)/bin/met2sci/AERMET/UACHK.for 

bin/met2sci/AERMET/SUBST.o:$(BD)/bin/met2sci/AERMET/SUBST.for 

bin/met2sci/AERMET/UAMOVE.o:$(BD)/bin/met2sci/AERMET/UAMOVE.for 

bin/met2sci/AERMET/GETFSL.o:$(BD)/bin/met2sci/AERMET/GETFSL.for 

bin/met2sci/AERMET/FDPATH.o:$(BD)/bin/met2sci/AERMET/FDPATH.for 

bin/met2sci/AERMET/HDPROC.o:$(BD)/bin/met2sci/AERMET/HDPROC.for 

bin/met2sci/AERMET/OSFILL.o:$(BD)/bin/met2sci/AERMET/OSFILL.for 

bin/met2sci/AERMET/OSCARD.o:$(BD)/bin/met2sci/AERMET/OSCARD.for 

bin/met2sci/AERMET/UAWNDW.o:$(BD)/bin/met2sci/AERMET/UAWNDW.for 

bin/met2sci/AERMET/GETFIL.o:$(BD)/bin/met2sci/AERMET/GETFIL.for 

bin/met2sci/AERMET/CHRCRD.o:$(BD)/bin/met2sci/AERMET/CHRCRD.for 

bin/met2sci/AERMET/SFCHK.o:$(BD)/bin/met2sci/AERMET/SFCHK.for 

bin/met2sci/AERMET/NETRAD.o:$(BD)/bin/met2sci/AERMET/NETRAD.for 

bin/met2sci/AERMET/RDLREC.o:$(BD)/bin/met2sci/AERMET/RDLREC.for 

bin/met2sci/AERMET/MRHDR.o:$(BD)/bin/met2sci/AERMET/MRHDR.for 

bin/met2sci/AERMET/MANDEL.o:$(BD)/bin/met2sci/AERMET/MANDEL.for 

bin/met2sci/AERMET/CBLHT.o:$(BD)/bin/met2sci/AERMET/CBLHT.for 

bin/met2sci/AERMET/SFQASM.o:$(BD)/bin/met2sci/AERMET/SFQASM.for 

bin/met2sci/AERMET/CALMS.o:$(BD)/bin/met2sci/AERMET/CALMS.for 

bin/met2sci/AERMET/NR_ANG.o:$(BD)/bin/met2sci/AERMET/NR_ANG.for 

bin/met2sci/AERMET/SCNGEN.o:$(BD)/bin/met2sci/AERMET/SCNGEN.for 

bin/met2sci/AERMET/FLWRK1.o:$(BD)/bin/met2sci/AERMET/FLWRK1.for 

bin/met2sci/AERMET/LATLON.o:$(BD)/bin/met2sci/AERMET/LATLON.for 

bin/met2sci/AERMET/DATER.o:$(BD)/bin/met2sci/AERMET/DATER.for 

bin/met2sci/AERMET/SFCARD.o:$(BD)/bin/met2sci/AERMET/SFCARD.for 

bin/met2sci/AERMET/OTHHDR.o:$(BD)/bin/met2sci/AERMET/OTHHDR.for 

bin/met2sci/AERMET/SFTRA.o:$(BD)/bin/met2sci/AERMET/SFTRA.for 

bin/met2sci/AERMET/OSDUMP.o:$(BD)/bin/met2sci/AERMET/OSDUMP.for 

bin/met2sci/AERMET/UAEXT.o:$(BD)/bin/met2sci/AERMET/UAEXT.for 

bin/met2sci/AERMET/NWSHGT.o:$(BD)/bin/met2sci/AERMET/NWSHGT.for 

bin/met2sci/AERMET/INCRAD.o:$(BD)/bin/met2sci/AERMET/INCRAD.for 

bin/met2sci/AERMET/AVGCRD.o:$(BD)/bin/met2sci/AERMET/AVGCRD.for 

bin/met2sci/AERMET/TEST.o:$(BD)/bin/met2sci/AERMET/TEST.for 

bin/met2sci/AERMET/AERSURF.o:$(BD)/bin/met2sci/AERMET/AERSURF.for 

bin/met2sci/AERMET/HUSWX.o:$(BD)/bin/met2sci/AERMET/HUSWX.for 

bin/met2sci/AERMET/MPCARD.o:$(BD)/bin/met2sci/AERMET/MPCARD.for 

bin/met2sci/AERMET/HUMID.o:$(BD)/bin/met2sci/AERMET/HUMID.for 

bin/met2sci/AERMET/TDPEST.o:$(BD)/bin/met2sci/AERMET/TDPEST.for 

bin/met2sci/AERMET/RDSAMS.o:$(BD)/bin/met2sci/AERMET/RDSAMS.for 

bin/met2sci/AERMET/FLWRK2.o:$(BD)/bin/met2sci/AERMET/FLWRK2.for 

bin/met2sci/AERMET/RDISHD.o:$(BD)/bin/met2sci/AERMET/RDISHD.for 

bin/met2sci/AERMET/FLOS.o:$(BD)/bin/met2sci/AERMET/FLOS.for 

bin/met2sci/AERMET/MRPATH.o:$(BD)/bin/met2sci/AERMET/MRPATH.for 

bin/met2sci/AERMET/SUMHF.o:$(BD)/bin/met2sci/AERMET/SUMHF.for 

bin/met2sci/AERMET/VARCRD.o:$(BD)/bin/met2sci/AERMET/VARCRD.for 

bin/met2sci/AERMET/UACARD.o:$(BD)/bin/met2sci/AERMET/UACARD.for 

bin/met2sci/AERMET/ISHWX.o:$(BD)/bin/met2sci/AERMET/ISHWX.for 

bin/met2sci/AERMET/SETHUS.o:$(BD)/bin/met2sci/AERMET/SETHUS.for 

bin/met2sci/AERMET/OSSWAP.o:$(BD)/bin/met2sci/AERMET/OSSWAP.for 

bin/met2sci/AERMET/MPPROC.o:$(BD)/bin/met2sci/AERMET/MPPROC.for 

bin/met2sci/AERMET/GET620.o:$(BD)/bin/met2sci/AERMET/GET620.for 

bin/met2sci/AERMET/MDCARD.o:$(BD)/bin/met2sci/AERMET/MDCARD.for 

bin/met2sci/AERMET/YR4TOYR2.o:$(BD)/bin/met2sci/AERMET/YR4TOYR2.for 

bin/met2sci/AERMET/D3280H.o:$(BD)/bin/met2sci/AERMET/D3280H.for 

bin/met2sci/AERMET/SFEXST.o:$(BD)/bin/met2sci/AERMET/SFEXST.for 

bin/met2sci/AERMET/FNDCOMDT.o:$(BD)/bin/met2sci/AERMET/FNDCOMDT.for  \
	  bin/met2sci/SCIPRE/mod_AsosCommDates.o

bin/met2sci/AERMET/RHOCAL.o:$(BD)/bin/met2sci/AERMET/RHOCAL.for 

bin/met2sci/AERMET/OSRANGE.o:$(BD)/bin/met2sci/AERMET/OSRANGE.for 

bin/met2sci/AERMET/SECCRD.o:$(BD)/bin/met2sci/AERMET/SECCRD.for 

bin/met2sci/AERMET/D6201H.o:$(BD)/bin/met2sci/AERMET/D6201H.for 

bin/met2sci/AERMET/ERRHDL.o:$(BD)/bin/met2sci/AERMET/ERRHDL.for 

bin/met2sci/AERMET/INTEQA.o:$(BD)/bin/met2sci/AERMET/INTEQA.for 

bin/met2sci/AERMET/OSNEXT.o:$(BD)/bin/met2sci/AERMET/OSNEXT.for 

bin/met2sci/AERMET/SAMWX.o:$(BD)/bin/met2sci/AERMET/SAMWX.for 

bin/met2sci/AERMET/DOCLDS.o:$(BD)/bin/met2sci/AERMET/DOCLDS.for 

bin/met2sci/AERMET/SAUDIT.o:$(BD)/bin/met2sci/AERMET/SAUDIT.for 

bin/met2sci/AERMET/DEF256.o:$(BD)/bin/met2sci/AERMET/DEF256.for 

bin/met2sci/AERMET/SETUP.o:$(BD)/bin/met2sci/AERMET/SETUP.for 

bin/met2sci/AERMET/HGTCRD.o:$(BD)/bin/met2sci/AERMET/HGTCRD.for 

bin/met2sci/AERMET/GETSFC.o:$(BD)/bin/met2sci/AERMET/GETSFC.for 

bin/met2sci/AERMET/FLOPEN.o:$(BD)/bin/met2sci/AERMET/FLOPEN.for 

bin/met2sci/AERMET/OSSUMS.o:$(BD)/bin/met2sci/AERMET/OSSUMS.for 

bin/met2sci/AERMET/MODEL.o:$(BD)/bin/met2sci/AERMET/MODEL.for 

bin/met2sci/AERMET/PTGRAD.o:$(BD)/bin/met2sci/AERMET/PTGRAD.for 

bin/met2sci/AERMET/SUNDAT.o:$(BD)/bin/met2sci/AERMET/SUNDAT.for 

bin/met2sci/AERMET/AUDIT.o:$(BD)/bin/met2sci/AERMET/AUDIT.for 

bin/met2sci/AERMET/SBLHT.o:$(BD)/bin/met2sci/AERMET/SBLHT.for 

bin/met2sci/AERMET/LWRUPR.o:$(BD)/bin/met2sci/AERMET/LWRUPR.for 

bin/met2sci/AERMET/SFCCRD.o:$(BD)/bin/met2sci/AERMET/SFCCRD.for 

bin/met2sci/AERMET/MPOUT.o:$(BD)/bin/met2sci/AERMET/MPOUT.for 

bin/met2sci/AERMET/SFCWXX.o:$(BD)/bin/met2sci/AERMET/SFCWXX.for 

bin/met2sci/AERMET/MPHEAD.o:$(BD)/bin/met2sci/AERMET/MPHEAD.for 

bin/met2sci/AERMET/SFCCH2.o:$(BD)/bin/met2sci/AERMET/SFCCH2.for 

bin/met2sci/AERMET/PRESET.o:$(BD)/bin/met2sci/AERMET/PRESET.for 

bin/met2sci/AERMET/UAEXST.o:$(BD)/bin/met2sci/AERMET/UAEXST.for 

bin/met2sci/AERMET/INTHF.o:$(BD)/bin/met2sci/AERMET/INTHF.for 

bin/met2sci/AERMET/UCALCO.o:$(BD)/bin/met2sci/AERMET/UCALCO.for 

bin/met2sci/AERMET/CLOUDS.o:$(BD)/bin/met2sci/AERMET/CLOUDS.for 

bin/met2sci/AERMET/MPTEST.o:$(BD)/bin/met2sci/AERMET/MPTEST.for 

bin/met2sci/AERMET/AERSURF2.o:$(BD)/bin/met2sci/AERMET/AERSURF2.for 

bin/met2sci/AERMET/HEADER.o:$(BD)/bin/met2sci/AERMET/HEADER.for 

bin/met2sci/AERMET/MRCARD.o:$(BD)/bin/met2sci/AERMET/MRCARD.for 

bin/met2sci/AERMET/D028LV.o:$(BD)/bin/met2sci/AERMET/D028LV.for 

bin/met2sci/AERMET/FLSDG.o:$(BD)/bin/met2sci/AERMET/FLSDG.for 

bin/met2sci/AERMET/GEO.o:$(BD)/bin/met2sci/AERMET/GEO.for 

bin/met2sci/AERMET/MPMET.o:$(BD)/bin/met2sci/AERMET/MPMET.for 

bin/met2sci/AERMET/HTKEY.o:$(BD)/bin/met2sci/AERMET/HTKEY.for 

bin/met2sci/AERMET/SETSAM.o:$(BD)/bin/met2sci/AERMET/SETSAM.for 

bin/met2sci/AERMET/MIDNITE.o:$(BD)/bin/met2sci/AERMET/MIDNITE.for 

bin/met2sci/AERMET/GETASOS.o:$(BD)/bin/met2sci/AERMET/GETASOS.for 

bin/met2sci/AERMET/STONUM.o:$(BD)/bin/met2sci/AERMET/STONUM.for 

bin/met2sci/AERMET/CUBIC.o:$(BD)/bin/met2sci/AERMET/CUBIC.for 

bin/met2sci/AERMET/D144LV.o:$(BD)/bin/met2sci/AERMET/D144LV.for 

bin/met2sci/AERMET/DEFINE.o:$(BD)/bin/met2sci/AERMET/DEFINE.for 

bin/met2sci/AERMET/THRESH1MIN.o:$(BD)/bin/met2sci/AERMET/THRESH1MIN.for 

bin/met2sci/AERMET/OSFILL2.o:$(BD)/bin/met2sci/AERMET/OSFILL2.for 

bin/met2sci/AERMET/RNGCRD.o:$(BD)/bin/met2sci/AERMET/RNGCRD.for 

bin/met2sci/AERMET/SFQATM.o:$(BD)/bin/met2sci/AERMET/SFQATM.for 

bin/met2sci/AERMET/OSDTCD.o:$(BD)/bin/met2sci/AERMET/OSDTCD.for 

bin/met2sci/AERMET/SUMRY2.o:$(BD)/bin/met2sci/AERMET/SUMRY2.for 

bin/met2sci/AERMET/FLSFC.o:$(BD)/bin/met2sci/AERMET/FLSFC.for 

bin/met2sci/AERMET/UAQASM.o:$(BD)/bin/met2sci/AERMET/UAQASM.for 

bin/met2sci/AERMET/SFCCH.o:$(BD)/bin/met2sci/AERMET/SFCCH.for 

bin/met2sci/AERMET/ASOSREC.o:$(BD)/bin/met2sci/AERMET/ASOSREC.for 

bin/met2sci/AERMET/BULKRI.o:$(BD)/bin/met2sci/AERMET/BULKRI.for 

bin/met2sci/AERMET/REALQA.o:$(BD)/bin/met2sci/AERMET/REALQA.for 

bin/met2sci/AERMET/SFPATH.o:$(BD)/bin/met2sci/AERMET/SFPATH.for 

bin/met2sci/AERMET/YR2TOYR4.o:$(BD)/bin/met2sci/AERMET/YR2TOYR4.for 

bin/met2sci/AERMET/FDKEY.o:$(BD)/bin/met2sci/AERMET/FDKEY.for 

bin/met2sci/AERMET/READRL.o:$(BD)/bin/met2sci/AERMET/READRL.for 

bin/met2sci/AERMET/OSQAST.o:$(BD)/bin/met2sci/AERMET/OSQAST.for 

bin/met2sci/AERMET/D144HD.o:$(BD)/bin/met2sci/AERMET/D144HD.for 

bin/met2sci/AERMET/GETFLD.o:$(BD)/bin/met2sci/AERMET/GETFLD.for 

bin/met2sci/AERMET/SFEXT.o:$(BD)/bin/met2sci/AERMET/SFEXT.for 

bin/met2sci/AERMET/OSWRTE.o:$(BD)/bin/met2sci/AERMET/OSWRTE.for 

bin/met2sci/AERMET/UCALST.o:$(BD)/bin/met2sci/AERMET/UCALST.for 

bin/met2sci/AERMET/XTNDUA.o:$(BD)/bin/met2sci/AERMET/XTNDUA.for 

bin/met2sci/AERMET/MPFIN.o:$(BD)/bin/met2sci/AERMET/MPFIN.for 

bin/met2sci/AERMET/PTAREA.o:$(BD)/bin/met2sci/AERMET/PTAREA.for 

bin/met2sci/AERMET/VRCARD.o:$(BD)/bin/met2sci/AERMET/VRCARD.for 

bin/met2sci/AERMET/HR0024.o:$(BD)/bin/met2sci/AERMET/HR0024.for 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  osread_fd.mod maxchar_fd.mod \
	  metsciparam_fd.mod filelist_fd.mod block2_irnd.mod met2sci_fi.mod \
	  mod_asoscommdates.mod error_fd.mod metscierrorparam_fd.mod