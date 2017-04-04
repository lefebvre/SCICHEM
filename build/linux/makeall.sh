#!/bin/bash

CLEAN=$1

LSuf="a"
x64='On'

# Set compiler and version
Compiler=ifort
Compiler_Version=ifort

# Setup netcdf and hdf relative library path 
CurDir=`pwd`
NETCDF_LIB_PATH=../../lib/linux/netcdf/4.1.2/${Compiler}/x64/static
HDF_LIB_PATH=../../lib/linux/hdf/hdf5-1.8.7-linux-x86_64-shared
PSrcDir=../..

export HDF_LIB_PATH=`readlink -f $HDF_LIB_PATH`
export NETCDF_LIB_PATH=`readlink -f $NETCDF_LIB_PATH`
export LD_LIBRARY_PATH=$HDF_LIB_PATH:$CurDir/$Compiler_Version
echo "HDF_LIB_PATH=$HDF_LIB_PATH"
echo "NETCDF_LIB_PATH=$NETCDF_LIB_PATH"
echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"

Makelist="libarap libodepack libsag liblocalMPI libaqaer libscipuff libprime libfdatums libsystool libmessages liblanduse libswim libsciptool libxpp metsci tersci runsci scipp sciDOSpost srf2smp smp2post"

#
# export required variables
#
export Compiler_Version
export LSuf PSrcDir x64

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

for fex in metsci tersci runsci sciDOSpost scipp smp2post srf2smp 
do
  if [ -e $Compiler_Version/$fex ];then
    cp -v $Compiler_Version/$fex ../../bin/linux
  fi
done
