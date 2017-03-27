#!/bin/bash

CLEAN=$1

CurDir=`pwd`
#LSuf="a"
LSuf="so"
x64='On'
PSrcDir=../..
Compiler_Version=ifort
export Compiler=ifort
export NETCDF_LIB_PATH=../../../lib/linux/netcdf/4.1.2/${Compiler}/x64/static
export HDF_LIB_PATH=../../../lib/linux/hdf/hdf5-1.8.7-linux-x86_64-shared

Makelist="libarap libodepack libsag liblocalMPI libaqaer libscipuff libprime libfdatums libsystool libmessages liblanduse libswim libsciptool libxpp metsci tersci runsci scipp sciDOSpost srf2smp smp2post"

#
# export required variables
#
export Compiler_Version LSuf LD_LIBRARY_PATH
export PSrcDir x64

if [ "$Compiler_Version" == "lahey" ]; then 
  source /usr/local/lf6481/bash_laheyfort_setup
elif [ "$Compiler_Version" == "pg" ]; then 
  export PATH=/opt/pgi/linux86/6.1/bin:$PATH
  echo $PATH
fi

if ! [ -d ${Compiler_Version} ] ; then
  echo 'Creating bin subdirectory ' ${Compiler_Version}
  mkdir ${Compiler_Version}
fi
if [ ${?} != 0 ]; then 
  echo 'Error from mkdir. Exiting ...'
  exit 1
fi
#
# Compiling section
#
if [ "$CLEAN" = clean ]; then
  find ${Compiler_Version} -follow -type f -print -exec rm -f {} \;
else
  echo 'Creating object and mod files ...' 
fi

#Create memcpy if shared library
if [ "$LSuf" == "so" ]; then
  make -f memcpy.mak $CLEAN
fi

#
# library files
#
for Mak in ${Makelist[@]}
do
  cd ${Compiler_Version}
  echo ' Working on '$Mak' in '${Compiler_Version} '...'
  if [ -e ../${Mak}.mak ]; then
    make -f ../${Mak}.mak $CLEAN
  else
    echo 'Error: Cannot find makefile for '$Mak
    exit 1
  fi
  cd ${CurDir}
done
