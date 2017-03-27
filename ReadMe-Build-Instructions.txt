Date: 15th June 2015

Developer contact information:

 1) Biswanath Chowdhury
    Sage-Xator
    15 Roszel Road
    Suite 102
    Princeton
    NJ 08550
    biswanath.chowdhury@sage-mgt.net
    
 2) Prakash Karamchandani
    Ramboll Environ
    773 San Marin Drive
    Suite 2115
    Novato
    CA 94998
    pkaramchandani@environcorp.com


Source Code and Compilers: 
  Fortran and C 
  Intel Fortran, gcc
  
Third-Party (and/or Open Source) software:
  None for 64 bit systems (Shared libraries for HDF5 1.8.7 are included with the distribution)
  netcdf static library version 4.1.2 or later and HDF5 1.8.7 or later for 32-bit systems
  
Computer system requirements to recompile software:
  Linux 64 bit system (x68_64) with Intel Fortran compiler for standard build
  32/64 bit Windows with Intel compiler for advanced build
  32/64 bit Linux with any standard Fortran and C compiler for advanced build

Steps to recompile software:
 
  Regular build on 64-bit Linux system:
  ====================================
  1) Unzip the tgz file on a Linux system using:
      tar -xzvf SCICHEM-3-0-Binary.tgz
  2) Change to build directory:
      cd build/linux
  3) Run build script:
      bash makeall.sh
  4) The executable and shared library files listed below will be created under the build/linux/ifort directory:
      metsci, tersci, runsci, sciDOSpost, scipp, srf2smp, smp2post, libswim.so, libsystool.so, liblanduse.so, libmessages.so and libsciptool.so

  Advanced build on 32-bit or 64-bit Linux/Unix system*:
  ====================================================
  1) Unzip the tgz file on a Linux/Unix system using:
      tar -xzvf SCICHEM-3-0-Binary.tgz
  2) Download the NETCDF library version 4.1.2 or later as source code or static libraries for the system from http://www.unidata.ucar.edu/software/netcdf. 
     If source code is being used then the user will have to create static library based on the build instructions from the netcdf website.
  3) Download the HDF library version 1.8.7 or later as source code or dynamic libraries for the system from http://www.hdfgroup.org. 
     If source code is being used then the user will have to create dynamic libraries based on the build instructions from the HDF website.
  4) Change to build directory:
      cd build/linux
  5) Edit the build script to set the NETCDF_LIB_PATH and HDF_LIB_PATH based on the location of the netcdf and HDF5 libraries.
  6) If the build is created using a non Intel compiler then the user will have to create a configuration file similar to make.ifort for the compiler being used. 
  7) Change the settings for the Compiler_Version and Compiler in makeall.sh to compiler being used.
  3) Run build script:
      bash makeall.sh
  4) The executable and shared library files listed below will be created under the build/linux/ifort directory:
      metsci, tersci, runsci, sciDOSpost, scipp, srf2smp, smp2post, libswim.so, libsystool.so, liblanduse.so, libmessages.so and libsciptool.so   
      
* The information is provided to the user for convenience. However, the code has not been tested extensively with other compilers or systems.

  Regular build on 64-bit Windows system*:
  ====================================================
  
  Note: Only Intel compiler is supported for building the SCIPUFFgui executable for Windows.
  
  1) Unzip the tgz file "SCICHEM-3-0-Binary.tgz" on the Windows system.
  2) Load the EPRIIntel solution for Visual Studio by double clicking the runVS.bat file in the build/windows/workspace/EPRI directory.
  3) Build the executable files using 'Build Solution' under the Build menu in Visual Studio.
  4) The executable and dynamic library files listed below should be created:
      metsci.exe, tersci.exe, runsci.exe , SCIPUFFgui.exe, sciDOSpost.exe , scipp.exe, srf2smp.exe, smp2post.exe, swim.dll, systool.dll, landuse.dll, messages.dll and sciptool.dll   

 Advanced build on 32-bit or 64-bit Windows system*:
  ====================================================
  
  Note: Only Intel compiler is supported for building the SCIPUFFgui executable for Windows.
  
  1) Unzip the tgz file "SCICHEM-3-0-Binary.tgz" on the Windows system.
  2) Download the NETCDF library version 4.1.2 or later as source code or static libraries for the system from http://www.unidata.ucar.edu/software/netcdf. 
     If source code is being used then the user will have to create static library based on the build instructions from the netcdf website.
  3) Download the HDF library version 1.8.7 or later as source code or dynamic libraries for the system from http://www.hdfgroup.org. 
     If source code is being used then the user will have to create dynamic libraries based on the build instructions from the HDF website.
  4) Create a solution and project files for different source directories using the source files in the build/windows/src directory.
  5) Include the NETCDF and HDF library path in the include path for the projects.
  6) Build the executable files.
  4) The executable and dynamic library files listed below should be created:
      metsci.exe, tersci.exe, runsci.exe , SCIPUFFgui.exe, sciDOSpost.exe , scipp.exe, srf2smp.exe, smp2post.exe, swim.dll, systool.dll, landuse.dll, messages.dll and sciptool.dll   
      
* The information is provided to the user for convenience. However, the code has not been tested extensively on 32 bit systems.


Passwords required:
  None

Lines of code:
  330,000

Signature(s):
  Biswanath Chowdhury
  Prakash Karamchandani
