# Main program is metsci

PROG =	metsci

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = bin/met2sci/SCIPRE/MaxChar_fd.f90 \
	  bin/met2sci/SCIPRE/MetSCIparam_fd.f90 \
	  bin/met2sci/SCIPRE/FileList_fd.f90 \
	  bin/met2sci/SCIPRE/WriteUAheader.f90 \
	  bin/met2sci/SCIPRE/ParsePathwayInput.f90 \
	  bin/met2sci/SCIPRE/MetSCIutil.f90 \
	  bin/met2sci/SCIPRE/ExtractSFCdata.f90 \
	  bin/met2sci/SCIPRE/ReportError.f90 \
	  bin/met2sci/SCIPRE/WriteUAdata.f90 \
	  bin/met2sci/SCIPRE/met2sci_fi.f90 \
	  bin/met2sci/SCIPRE/ExtractUAdata.f90 \
	  bin/met2sci/SCIPRE/error_fd.f90 bin/met2sci/SCIPRE/met2sci.f90 \
	  bin/met2sci/SCIPRE/WriteSFCheader.f90 \
	  bin/met2sci/SCIPRE/WriteSFCdata.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

SRCS_for = bin/met2sci/SCIPRE/SetUAQA.for bin/met2sci/SCIPRE/SetUA.for \
	  bin/met2sci/SCIPRE/GetUAloc.for \
	  bin/met2sci/SCIPRE/SetAERMETdata.for \
	  bin/met2sci/SCIPRE/mod_AsosCommDates.for \
	  bin/met2sci/SCIPRE/SetSFC.for bin/met2sci/AERMET/FMTCRD.for \
	  bin/met2sci/AERMET/FLIWK1.for bin/met2sci/AERMET/OSSMRY.for \
	  bin/met2sci/AERMET/OSREAD.for bin/met2sci/AERMET/OSTEST.for \
	  bin/met2sci/AERMET/OSPRNT.for bin/met2sci/AERMET/GREG.for \
	  bin/met2sci/AERMET/SFCCRD2.for bin/met2sci/AERMET/CLMCRD.for \
	  bin/met2sci/AERMET/UAUDIT.for bin/met2sci/AERMET/GMTLST.for \
	  bin/met2sci/AERMET/AUTCHK.for bin/met2sci/AERMET/OSPATH.for \
	  bin/met2sci/AERMET/FLIWK2.for bin/met2sci/AERMET/OSCHK.for \
	  bin/met2sci/AERMET/BANNER.for bin/met2sci/AERMET/JBCARD.for \
	  bin/met2sci/AERMET/UAQAST.for bin/met2sci/AERMET/HTCALC.for \
	  bin/met2sci/AERMET/COMPDT.for bin/met2sci/AERMET/P2MSUB.for \
	  bin/met2sci/AERMET/MPPBL.for bin/met2sci/AERMET/D3280L.for \
	  bin/met2sci/AERMET/ICHRND.for bin/met2sci/AERMET/OSTRA.for \
	  bin/met2sci/AERMET/OAUDIT.for bin/met2sci/AERMET/OSQACK.for \
	  bin/met2sci/AERMET/DTCRD.for bin/met2sci/AERMET/SECCRD2.for \
	  bin/met2sci/AERMET/EQ_CCVR.for bin/met2sci/AERMET/HEAT.for \
	  bin/met2sci/AERMET/CHRCRD2.for bin/met2sci/AERMET/FETCH.for \
	  bin/met2sci/AERMET/DATCRD.for bin/met2sci/AERMET/MERGE.for \
	  bin/met2sci/AERMET/RDHUSW.for bin/met2sci/AERMET/CVG.for \
	  bin/met2sci/AERMET/SUMRY1.for bin/met2sci/AERMET/XDTCRD.for \
	  bin/met2sci/AERMET/CHROND.for bin/met2sci/AERMET/UATRA.for \
	  bin/met2sci/AERMET/SFQAST.for bin/met2sci/AERMET/SMTHZI.for \
	  bin/met2sci/AERMET/LOCCRD.for bin/met2sci/AERMET/OSHRAV.for \
	  bin/met2sci/AERMET/CLHT.for bin/met2sci/AERMET/UAPATH.for \
	  bin/met2sci/AERMET/VALCRD.for bin/met2sci/AERMET/D6201L.for \
	  bin/met2sci/AERMET/GETWRD.for bin/met2sci/AERMET/WRTCRD.for \
	  bin/met2sci/AERMET/UACHK.for bin/met2sci/AERMET/SUBST.for \
	  bin/met2sci/AERMET/UAMOVE.for bin/met2sci/AERMET/GETFSL.for \
	  bin/met2sci/AERMET/FDPATH.for bin/met2sci/AERMET/HDPROC.for \
	  bin/met2sci/AERMET/OSFILL.for bin/met2sci/AERMET/OSCARD.for \
	  bin/met2sci/AERMET/UAWNDW.for bin/met2sci/AERMET/GETFIL.for \
	  bin/met2sci/AERMET/CHRCRD.for bin/met2sci/AERMET/SFCHK.for \
	  bin/met2sci/AERMET/NETRAD.for bin/met2sci/AERMET/RDLREC.for \
	  bin/met2sci/AERMET/MRHDR.for bin/met2sci/AERMET/MANDEL.for \
	  bin/met2sci/AERMET/CBLHT.for bin/met2sci/AERMET/SFQASM.for \
	  bin/met2sci/AERMET/CALMS.for bin/met2sci/AERMET/NR_ANG.for \
	  bin/met2sci/AERMET/SCNGEN.for bin/met2sci/AERMET/FLWRK1.for \
	  bin/met2sci/AERMET/LATLON.for bin/met2sci/AERMET/DATER.for \
	  bin/met2sci/AERMET/SFCARD.for bin/met2sci/AERMET/OTHHDR.for \
	  bin/met2sci/AERMET/SFTRA.for bin/met2sci/AERMET/OSDUMP.for \
	  bin/met2sci/AERMET/UAEXT.for bin/met2sci/AERMET/NWSHGT.for \
	  bin/met2sci/AERMET/INCRAD.for bin/met2sci/AERMET/AVGCRD.for \
	  bin/met2sci/AERMET/TEST.for bin/met2sci/AERMET/AERSURF.for \
	  bin/met2sci/AERMET/HUSWX.for bin/met2sci/AERMET/MPCARD.for \
	  bin/met2sci/AERMET/HUMID.for bin/met2sci/AERMET/TDPEST.for \
	  bin/met2sci/AERMET/RDSAMS.for bin/met2sci/AERMET/FLWRK2.for \
	  bin/met2sci/AERMET/RDISHD.for bin/met2sci/AERMET/FLOS.for \
	  bin/met2sci/AERMET/MRPATH.for bin/met2sci/AERMET/SUMHF.for \
	  bin/met2sci/AERMET/VARCRD.for bin/met2sci/AERMET/UACARD.for \
	  bin/met2sci/AERMET/ISHWX.for bin/met2sci/AERMET/SETHUS.for \
	  bin/met2sci/AERMET/OSSWAP.for bin/met2sci/AERMET/MPPROC.for \
	  bin/met2sci/AERMET/GET620.for bin/met2sci/AERMET/MDCARD.for \
	  bin/met2sci/AERMET/YR4TOYR2.for bin/met2sci/AERMET/D3280H.for \
	  bin/met2sci/AERMET/SFEXST.for bin/met2sci/AERMET/FNDCOMDT.for \
	  bin/met2sci/AERMET/RHOCAL.for bin/met2sci/AERMET/OSRANGE.for \
	  bin/met2sci/AERMET/SECCRD.for bin/met2sci/AERMET/D6201H.for \
	  bin/met2sci/AERMET/ERRHDL.for bin/met2sci/AERMET/INTEQA.for \
	  bin/met2sci/AERMET/OSNEXT.for bin/met2sci/AERMET/SAMWX.for \
	  bin/met2sci/AERMET/DOCLDS.for bin/met2sci/AERMET/SAUDIT.for \
	  bin/met2sci/AERMET/DEF256.for bin/met2sci/AERMET/SETUP.for \
	  bin/met2sci/AERMET/HGTCRD.for bin/met2sci/AERMET/GETSFC.for \
	  bin/met2sci/AERMET/FLOPEN.for bin/met2sci/AERMET/OSSUMS.for \
	  bin/met2sci/AERMET/MODEL.for bin/met2sci/AERMET/PTGRAD.for \
	  bin/met2sci/AERMET/SUNDAT.for bin/met2sci/AERMET/AUDIT.for \
	  bin/met2sci/AERMET/SBLHT.for bin/met2sci/AERMET/LWRUPR.for \
	  bin/met2sci/AERMET/SFCCRD.for bin/met2sci/AERMET/MPOUT.for \
	  bin/met2sci/AERMET/SFCWXX.for bin/met2sci/AERMET/MPHEAD.for \
	  bin/met2sci/AERMET/SFCCH2.for bin/met2sci/AERMET/PRESET.for \
	  bin/met2sci/AERMET/UAEXST.for bin/met2sci/AERMET/INTHF.for \
	  bin/met2sci/AERMET/UCALCO.for bin/met2sci/AERMET/CLOUDS.for \
	  bin/met2sci/AERMET/MPTEST.for bin/met2sci/AERMET/AERSURF2.for \
	  bin/met2sci/AERMET/HEADER.for bin/met2sci/AERMET/MRCARD.for \
	  bin/met2sci/AERMET/D028LV.for bin/met2sci/AERMET/FLSDG.for \
	  bin/met2sci/AERMET/GEO.for bin/met2sci/AERMET/MPMET.for \
	  bin/met2sci/AERMET/HTKEY.for bin/met2sci/AERMET/SETSAM.for \
	  bin/met2sci/AERMET/MIDNITE.for bin/met2sci/AERMET/GETASOS.for \
	  bin/met2sci/AERMET/STONUM.for bin/met2sci/AERMET/CUBIC.for \
	  bin/met2sci/AERMET/D144LV.for bin/met2sci/AERMET/DEFINE.for \
	  bin/met2sci/AERMET/THRESH1MIN.for bin/met2sci/AERMET/OSFILL2.for \
	  bin/met2sci/AERMET/RNGCRD.for bin/met2sci/AERMET/SFQATM.for \
	  bin/met2sci/AERMET/OSDTCD.for bin/met2sci/AERMET/SUMRY2.for \
	  bin/met2sci/AERMET/FLSFC.for bin/met2sci/AERMET/UAQASM.for \
	  bin/met2sci/AERMET/SFCCH.for bin/met2sci/AERMET/ASOSREC.for \
	  bin/met2sci/AERMET/BULKRI.for bin/met2sci/AERMET/REALQA.for \
	  bin/met2sci/AERMET/SFPATH.for bin/met2sci/AERMET/YR2TOYR4.for \
	  bin/met2sci/AERMET/FDKEY.for bin/met2sci/AERMET/READRL.for \
	  bin/met2sci/AERMET/OSQAST.for bin/met2sci/AERMET/D144HD.for \
	  bin/met2sci/AERMET/GETFLD.for bin/met2sci/AERMET/SFEXT.for \
	  bin/met2sci/AERMET/OSWRTE.for bin/met2sci/AERMET/UCALST.for \
	  bin/met2sci/AERMET/XTNDUA.for bin/met2sci/AERMET/MPFIN.for \
	  bin/met2sci/AERMET/PTAREA.for bin/met2sci/AERMET/VRCARD.for \
	  bin/met2sci/AERMET/HR0024.for

OBJS_for := $(notdir $(subst .for,.o,$(SRCS_for)))

OBJS :=  $(OBJS_f90)  $(OBJS_for) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LNK) $(LNKFLAGS) $(PROG) $(OBJS) $(LIBS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 

$(OBJS_for): %.o:$(filter /\%.for,$(SRCS_for))
	$(F77) $(F77FLAGS) $< 


SetUAQA.o:$(BD)/bin/met2sci/SCIPRE/SetUAQA.for 

MaxChar_fd.o:$(BD)/bin/met2sci/SCIPRE/MaxChar_fd.f90 

MetSCIparam_fd.o:$(BD)/bin/met2sci/SCIPRE/MetSCIparam_fd.f90 

FileList_fd.o:$(BD)/bin/met2sci/SCIPRE/FileList_fd.f90  MaxChar_fd.o

SetUA.o:$(BD)/bin/met2sci/SCIPRE/SetUA.for 

GetUAloc.o:$(BD)/bin/met2sci/SCIPRE/GetUAloc.for 

error_fd.o:$(BD)/bin/met2sci/SCIPRE/error_fd.f90  MaxChar_fd.o

met2sci_fi.o:$(BD)/bin/met2sci/SCIPRE/met2sci_fi.f90  error_fd.o \
	  FileList_fd.o MaxChar_fd.o MetSCIparam_fd.o

WriteUAheader.o:$(BD)/bin/met2sci/SCIPRE/WriteUAheader.f90  met2sci_fi.o

ParsePathwayInput.o:$(BD)/bin/met2sci/SCIPRE/ParsePathwayInput.f90  \
	  met2sci_fi.o MetSCIparam_fd.o

MetSCIutil.o:$(BD)/bin/met2sci/SCIPRE/MetSCIutil.f90 

ExtractSFCdata.o:$(BD)/bin/met2sci/SCIPRE/ExtractSFCdata.f90  met2sci_fi.o

SetAERMETdata.o:$(BD)/bin/met2sci/SCIPRE/SetAERMETdata.for 

ReportError.o:$(BD)/bin/met2sci/SCIPRE/ReportError.f90  met2sci_fi.o

WriteUAdata.o:$(BD)/bin/met2sci/SCIPRE/WriteUAdata.f90  met2sci_fi.o

mod_AsosCommDates.o:$(BD)/bin/met2sci/SCIPRE/mod_AsosCommDates.for 

ExtractUAdata.o:$(BD)/bin/met2sci/SCIPRE/ExtractUAdata.f90  met2sci_fi.o

SetSFC.o:$(BD)/bin/met2sci/SCIPRE/SetSFC.for 

met2sci.o:$(BD)/bin/met2sci/SCIPRE/met2sci.f90  met2sci_fi.o

WriteSFCheader.o:$(BD)/bin/met2sci/SCIPRE/WriteSFCheader.f90  met2sci_fi.o

WriteSFCdata.o:$(BD)/bin/met2sci/SCIPRE/WriteSFCdata.f90  met2sci_fi.o

FMTCRD.o:$(BD)/bin/met2sci/AERMET/FMTCRD.for 

FLIWK1.o:$(BD)/bin/met2sci/AERMET/FLIWK1.for 

OSSMRY.o:$(BD)/bin/met2sci/AERMET/OSSMRY.for 

OSREAD.o:$(BD)/bin/met2sci/AERMET/OSREAD.for 

OSTEST.o:$(BD)/bin/met2sci/AERMET/OSTEST.for 

OSPRNT.o:$(BD)/bin/met2sci/AERMET/OSPRNT.for 

GREG.o:$(BD)/bin/met2sci/AERMET/GREG.for 

SFCCRD2.o:$(BD)/bin/met2sci/AERMET/SFCCRD2.for 

CLMCRD.o:$(BD)/bin/met2sci/AERMET/CLMCRD.for 

UAUDIT.o:$(BD)/bin/met2sci/AERMET/UAUDIT.for 

GMTLST.o:$(BD)/bin/met2sci/AERMET/GMTLST.for 

AUTCHK.o:$(BD)/bin/met2sci/AERMET/AUTCHK.for 

OSPATH.o:$(BD)/bin/met2sci/AERMET/OSPATH.for 

FLIWK2.o:$(BD)/bin/met2sci/AERMET/FLIWK2.for 

OSCHK.o:$(BD)/bin/met2sci/AERMET/OSCHK.for 

BANNER.o:$(BD)/bin/met2sci/AERMET/BANNER.for 

JBCARD.o:$(BD)/bin/met2sci/AERMET/JBCARD.for 

UAQAST.o:$(BD)/bin/met2sci/AERMET/UAQAST.for 

HTCALC.o:$(BD)/bin/met2sci/AERMET/HTCALC.for 

COMPDT.o:$(BD)/bin/met2sci/AERMET/COMPDT.for 

P2MSUB.o:$(BD)/bin/met2sci/AERMET/P2MSUB.for 

MPPBL.o:$(BD)/bin/met2sci/AERMET/MPPBL.for 

D3280L.o:$(BD)/bin/met2sci/AERMET/D3280L.for 

ICHRND.o:$(BD)/bin/met2sci/AERMET/ICHRND.for 

OSTRA.o:$(BD)/bin/met2sci/AERMET/OSTRA.for 

OAUDIT.o:$(BD)/bin/met2sci/AERMET/OAUDIT.for 

OSQACK.o:$(BD)/bin/met2sci/AERMET/OSQACK.for 

DTCRD.o:$(BD)/bin/met2sci/AERMET/DTCRD.for 

SECCRD2.o:$(BD)/bin/met2sci/AERMET/SECCRD2.for 

EQ_CCVR.o:$(BD)/bin/met2sci/AERMET/EQ_CCVR.for 

HEAT.o:$(BD)/bin/met2sci/AERMET/HEAT.for 

CHRCRD2.o:$(BD)/bin/met2sci/AERMET/CHRCRD2.for 

FETCH.o:$(BD)/bin/met2sci/AERMET/FETCH.for 

DATCRD.o:$(BD)/bin/met2sci/AERMET/DATCRD.for 

MERGE.o:$(BD)/bin/met2sci/AERMET/MERGE.for 

RDHUSW.o:$(BD)/bin/met2sci/AERMET/RDHUSW.for 

CVG.o:$(BD)/bin/met2sci/AERMET/CVG.for 

SUMRY1.o:$(BD)/bin/met2sci/AERMET/SUMRY1.for 

XDTCRD.o:$(BD)/bin/met2sci/AERMET/XDTCRD.for 

CHROND.o:$(BD)/bin/met2sci/AERMET/CHROND.for 

UATRA.o:$(BD)/bin/met2sci/AERMET/UATRA.for 

SFQAST.o:$(BD)/bin/met2sci/AERMET/SFQAST.for 

SMTHZI.o:$(BD)/bin/met2sci/AERMET/SMTHZI.for 

LOCCRD.o:$(BD)/bin/met2sci/AERMET/LOCCRD.for 

OSHRAV.o:$(BD)/bin/met2sci/AERMET/OSHRAV.for 

CLHT.o:$(BD)/bin/met2sci/AERMET/CLHT.for 

UAPATH.o:$(BD)/bin/met2sci/AERMET/UAPATH.for 

VALCRD.o:$(BD)/bin/met2sci/AERMET/VALCRD.for 

D6201L.o:$(BD)/bin/met2sci/AERMET/D6201L.for 

GETWRD.o:$(BD)/bin/met2sci/AERMET/GETWRD.for 

WRTCRD.o:$(BD)/bin/met2sci/AERMET/WRTCRD.for 

UACHK.o:$(BD)/bin/met2sci/AERMET/UACHK.for 

SUBST.o:$(BD)/bin/met2sci/AERMET/SUBST.for 

UAMOVE.o:$(BD)/bin/met2sci/AERMET/UAMOVE.for 

GETFSL.o:$(BD)/bin/met2sci/AERMET/GETFSL.for 

FDPATH.o:$(BD)/bin/met2sci/AERMET/FDPATH.for 

HDPROC.o:$(BD)/bin/met2sci/AERMET/HDPROC.for 

OSFILL.o:$(BD)/bin/met2sci/AERMET/OSFILL.for 

OSCARD.o:$(BD)/bin/met2sci/AERMET/OSCARD.for 

UAWNDW.o:$(BD)/bin/met2sci/AERMET/UAWNDW.for 

GETFIL.o:$(BD)/bin/met2sci/AERMET/GETFIL.for 

CHRCRD.o:$(BD)/bin/met2sci/AERMET/CHRCRD.for 

SFCHK.o:$(BD)/bin/met2sci/AERMET/SFCHK.for 

NETRAD.o:$(BD)/bin/met2sci/AERMET/NETRAD.for 

RDLREC.o:$(BD)/bin/met2sci/AERMET/RDLREC.for 

MRHDR.o:$(BD)/bin/met2sci/AERMET/MRHDR.for 

MANDEL.o:$(BD)/bin/met2sci/AERMET/MANDEL.for 

CBLHT.o:$(BD)/bin/met2sci/AERMET/CBLHT.for 

SFQASM.o:$(BD)/bin/met2sci/AERMET/SFQASM.for 

CALMS.o:$(BD)/bin/met2sci/AERMET/CALMS.for 

NR_ANG.o:$(BD)/bin/met2sci/AERMET/NR_ANG.for 

SCNGEN.o:$(BD)/bin/met2sci/AERMET/SCNGEN.for 

FLWRK1.o:$(BD)/bin/met2sci/AERMET/FLWRK1.for 

LATLON.o:$(BD)/bin/met2sci/AERMET/LATLON.for 

DATER.o:$(BD)/bin/met2sci/AERMET/DATER.for 

SFCARD.o:$(BD)/bin/met2sci/AERMET/SFCARD.for 

OTHHDR.o:$(BD)/bin/met2sci/AERMET/OTHHDR.for 

SFTRA.o:$(BD)/bin/met2sci/AERMET/SFTRA.for 

OSDUMP.o:$(BD)/bin/met2sci/AERMET/OSDUMP.for 

UAEXT.o:$(BD)/bin/met2sci/AERMET/UAEXT.for 

NWSHGT.o:$(BD)/bin/met2sci/AERMET/NWSHGT.for 

INCRAD.o:$(BD)/bin/met2sci/AERMET/INCRAD.for 

AVGCRD.o:$(BD)/bin/met2sci/AERMET/AVGCRD.for 

TEST.o:$(BD)/bin/met2sci/AERMET/TEST.for 

AERSURF.o:$(BD)/bin/met2sci/AERMET/AERSURF.for 

HUSWX.o:$(BD)/bin/met2sci/AERMET/HUSWX.for 

MPCARD.o:$(BD)/bin/met2sci/AERMET/MPCARD.for 

HUMID.o:$(BD)/bin/met2sci/AERMET/HUMID.for 

TDPEST.o:$(BD)/bin/met2sci/AERMET/TDPEST.for 

RDSAMS.o:$(BD)/bin/met2sci/AERMET/RDSAMS.for 

FLWRK2.o:$(BD)/bin/met2sci/AERMET/FLWRK2.for 

RDISHD.o:$(BD)/bin/met2sci/AERMET/RDISHD.for 

FLOS.o:$(BD)/bin/met2sci/AERMET/FLOS.for 

MRPATH.o:$(BD)/bin/met2sci/AERMET/MRPATH.for 

SUMHF.o:$(BD)/bin/met2sci/AERMET/SUMHF.for 

VARCRD.o:$(BD)/bin/met2sci/AERMET/VARCRD.for 

UACARD.o:$(BD)/bin/met2sci/AERMET/UACARD.for 

ISHWX.o:$(BD)/bin/met2sci/AERMET/ISHWX.for 

SETHUS.o:$(BD)/bin/met2sci/AERMET/SETHUS.for 

OSSWAP.o:$(BD)/bin/met2sci/AERMET/OSSWAP.for 

MPPROC.o:$(BD)/bin/met2sci/AERMET/MPPROC.for 

GET620.o:$(BD)/bin/met2sci/AERMET/GET620.for 

MDCARD.o:$(BD)/bin/met2sci/AERMET/MDCARD.for 

YR4TOYR2.o:$(BD)/bin/met2sci/AERMET/YR4TOYR2.for 

D3280H.o:$(BD)/bin/met2sci/AERMET/D3280H.for 

SFEXST.o:$(BD)/bin/met2sci/AERMET/SFEXST.for 

FNDCOMDT.o:$(BD)/bin/met2sci/AERMET/FNDCOMDT.for  mod_AsosCommDates.o

RHOCAL.o:$(BD)/bin/met2sci/AERMET/RHOCAL.for 

OSRANGE.o:$(BD)/bin/met2sci/AERMET/OSRANGE.for 

SECCRD.o:$(BD)/bin/met2sci/AERMET/SECCRD.for 

D6201H.o:$(BD)/bin/met2sci/AERMET/D6201H.for 

ERRHDL.o:$(BD)/bin/met2sci/AERMET/ERRHDL.for 

INTEQA.o:$(BD)/bin/met2sci/AERMET/INTEQA.for 

OSNEXT.o:$(BD)/bin/met2sci/AERMET/OSNEXT.for 

SAMWX.o:$(BD)/bin/met2sci/AERMET/SAMWX.for 

DOCLDS.o:$(BD)/bin/met2sci/AERMET/DOCLDS.for 

SAUDIT.o:$(BD)/bin/met2sci/AERMET/SAUDIT.for 

DEF256.o:$(BD)/bin/met2sci/AERMET/DEF256.for 

SETUP.o:$(BD)/bin/met2sci/AERMET/SETUP.for 

HGTCRD.o:$(BD)/bin/met2sci/AERMET/HGTCRD.for 

GETSFC.o:$(BD)/bin/met2sci/AERMET/GETSFC.for 

FLOPEN.o:$(BD)/bin/met2sci/AERMET/FLOPEN.for 

OSSUMS.o:$(BD)/bin/met2sci/AERMET/OSSUMS.for 

MODEL.o:$(BD)/bin/met2sci/AERMET/MODEL.for 

PTGRAD.o:$(BD)/bin/met2sci/AERMET/PTGRAD.for 

SUNDAT.o:$(BD)/bin/met2sci/AERMET/SUNDAT.for 

AUDIT.o:$(BD)/bin/met2sci/AERMET/AUDIT.for 

SBLHT.o:$(BD)/bin/met2sci/AERMET/SBLHT.for 

LWRUPR.o:$(BD)/bin/met2sci/AERMET/LWRUPR.for 

SFCCRD.o:$(BD)/bin/met2sci/AERMET/SFCCRD.for 

MPOUT.o:$(BD)/bin/met2sci/AERMET/MPOUT.for 

SFCWXX.o:$(BD)/bin/met2sci/AERMET/SFCWXX.for 

MPHEAD.o:$(BD)/bin/met2sci/AERMET/MPHEAD.for 

SFCCH2.o:$(BD)/bin/met2sci/AERMET/SFCCH2.for 

PRESET.o:$(BD)/bin/met2sci/AERMET/PRESET.for 

UAEXST.o:$(BD)/bin/met2sci/AERMET/UAEXST.for 

INTHF.o:$(BD)/bin/met2sci/AERMET/INTHF.for 

UCALCO.o:$(BD)/bin/met2sci/AERMET/UCALCO.for 

CLOUDS.o:$(BD)/bin/met2sci/AERMET/CLOUDS.for 

MPTEST.o:$(BD)/bin/met2sci/AERMET/MPTEST.for 

AERSURF2.o:$(BD)/bin/met2sci/AERMET/AERSURF2.for 

HEADER.o:$(BD)/bin/met2sci/AERMET/HEADER.for 

MRCARD.o:$(BD)/bin/met2sci/AERMET/MRCARD.for 

D028LV.o:$(BD)/bin/met2sci/AERMET/D028LV.for 

FLSDG.o:$(BD)/bin/met2sci/AERMET/FLSDG.for 

GEO.o:$(BD)/bin/met2sci/AERMET/GEO.for 

MPMET.o:$(BD)/bin/met2sci/AERMET/MPMET.for 

HTKEY.o:$(BD)/bin/met2sci/AERMET/HTKEY.for 

SETSAM.o:$(BD)/bin/met2sci/AERMET/SETSAM.for 

MIDNITE.o:$(BD)/bin/met2sci/AERMET/MIDNITE.for 

GETASOS.o:$(BD)/bin/met2sci/AERMET/GETASOS.for 

STONUM.o:$(BD)/bin/met2sci/AERMET/STONUM.for 

CUBIC.o:$(BD)/bin/met2sci/AERMET/CUBIC.for 

D144LV.o:$(BD)/bin/met2sci/AERMET/D144LV.for 

DEFINE.o:$(BD)/bin/met2sci/AERMET/DEFINE.for 

THRESH1MIN.o:$(BD)/bin/met2sci/AERMET/THRESH1MIN.for 

OSFILL2.o:$(BD)/bin/met2sci/AERMET/OSFILL2.for 

RNGCRD.o:$(BD)/bin/met2sci/AERMET/RNGCRD.for 

SFQATM.o:$(BD)/bin/met2sci/AERMET/SFQATM.for 

OSDTCD.o:$(BD)/bin/met2sci/AERMET/OSDTCD.for 

SUMRY2.o:$(BD)/bin/met2sci/AERMET/SUMRY2.for 

FLSFC.o:$(BD)/bin/met2sci/AERMET/FLSFC.for 

UAQASM.o:$(BD)/bin/met2sci/AERMET/UAQASM.for 

SFCCH.o:$(BD)/bin/met2sci/AERMET/SFCCH.for 

ASOSREC.o:$(BD)/bin/met2sci/AERMET/ASOSREC.for 

BULKRI.o:$(BD)/bin/met2sci/AERMET/BULKRI.for 

REALQA.o:$(BD)/bin/met2sci/AERMET/REALQA.for 

SFPATH.o:$(BD)/bin/met2sci/AERMET/SFPATH.for 

YR2TOYR4.o:$(BD)/bin/met2sci/AERMET/YR2TOYR4.for 

FDKEY.o:$(BD)/bin/met2sci/AERMET/FDKEY.for 

READRL.o:$(BD)/bin/met2sci/AERMET/READRL.for 

OSQAST.o:$(BD)/bin/met2sci/AERMET/OSQAST.for 

D144HD.o:$(BD)/bin/met2sci/AERMET/D144HD.for 

GETFLD.o:$(BD)/bin/met2sci/AERMET/GETFLD.for 

SFEXT.o:$(BD)/bin/met2sci/AERMET/SFEXT.for 

OSWRTE.o:$(BD)/bin/met2sci/AERMET/OSWRTE.for 

UCALST.o:$(BD)/bin/met2sci/AERMET/UCALST.for 

XTNDUA.o:$(BD)/bin/met2sci/AERMET/XTNDUA.for 

MPFIN.o:$(BD)/bin/met2sci/AERMET/MPFIN.for 

PTAREA.o:$(BD)/bin/met2sci/AERMET/PTAREA.for 

VRCARD.o:$(BD)/bin/met2sci/AERMET/VRCARD.for 

HR0024.o:$(BD)/bin/met2sci/AERMET/HR0024.for 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  maxchar_fd.mod metsciparam_fd.mod \
	  filelist_fd.mod met2sci_fi.mod mod_asoscommdates.mod error_fd.mod \
	  metscierrorparam_fd.mod