==============
SCICHEM 3.0 
==============

SCICHEM 3.0 is a reactive puff model that can be used to calculate single or multi-source impacts of emissions at downwind locations. The model can be used for both short-range calculations of 1-hour SO2 or 1-hour NO2 concentrations at fenceline receptors or long-range calculations for primary and secondary pollutant impacts. For 1-hour NO2 applications, the model uses an optimized near-source NO-NO2-O3 chemistry scheme. For long-range applications, the full chemistry option can be used to calculate downwind ozone and PM2.5 concentrations. The full chemistry modules are based on those found in the Community Multiscale Air Quality (CMAQ) Model version 4.7.1. A user-provided input file determines which chemistry option is used. Sample input files for both 1-hour NO2 concentrations and full chemistry options are provided with the case studies in the SCICHEM distribution. In addition to the source code and executable files, the package includes the following:

-	User's Guide, 
-	Technical documentation, 
-	4 case studies and accompanying tutorials, and 
-	This Readme file 

This distribution includes a limited version of a Graphical User Interface (GUI), named "SCIPUFFgui, which is provided for the 64-bit Windows 7 or higher operating system as an aid to the user to visualize model results. The GUI can plot concentration contour plots for surface, horizontal, or vertical slices for all source types. Note that SCIPUFFgui can also be used to create and run SCICHEM 2.0 namelist-type projects, but this capability has not been extended to support keyword-type projects introduced in SCICHEM 3.0 that are required for dealing with some new source types such as area, volume and sources with building effects. Thus, users are recommended to use the GUI primarily for viewing simulation results.

This distribution consists of two readme files viz. ReadMe.txt (this file) and ReadMe-Build-Instructions.txt and four zipped files viz. SCICHEM-3-0-Docs.tgz, SCICHEM-3-0-Binary.tgz, SCICHEM-3-0-Examples.tgz and SCICHEM-3-0-FC_MEDOC.tgz to limit the size of the individual zipped files. All the files should be unzipped in the same directory. For running SCICHEM, the appropriate scipuff.ini file (in the bin/windows/x64 or bin/linux directory) should be edited so that the paths for the sciData directory and landuse.dat file point to the correct directory on the User's system. Details for running the SCIPUFFgui on windows is provided in the User's Guide.

The Mesoscale Model Interface Program (MMIF) on the U.S. EPA SCRAM web site can be used to convert prognostic meteorological model (MM5 and/or WRF) outputs to SCICHEM ready meteorological  inputs. SCICHEM 3.0 requires MMIF version 3.1 or later for compatibility.

This is a full release that has been tested for a number of conditions. Windows and Linux versions of the executables are provided with the distribution. Both the Windows and Linux builds were created using the Intel compiler. For users interested in building the executable files on Linux or Windows machines, build scripts and Visual Fortran project files for the Intel compiler are provided. Users can create builds using other compilers but builds with non Intel compilers have not been tested at this time.

Additional details and user instructions are provided in the documents bundled with the package. Users are requested to offer feedback to EPRI and the model developers on the model, including bug reports, and additional features that would make the model more useful to the air quality modeling community.

==============
BEST PRACTICES
==============
Some guidelines for creating good project input files for SCICHEM are provided below:

1) With observed meteorology, if a terrain file is being provided separately then the terrain grid dimensions should be limited to a grid of less than 100x100 cells. Using more grid cells will result in significant increases in run time, because SCICHEM conducts mass consistenctwind field calculations. For high resolution runs, mass consistent wind fields should be generated using a meteorological model (e.g., WRF) and provided to SCICHEM as gridded meteorological fields. Note that WRF fields can be directly read by SCICHEM or the MMIF processor mentioned above can be used to create gridded meteorological files in SCICHEM format.

2) The model can output time-averaged concentrations at selected receptor locations at runtime. However CPU requirements increase significantly with the number of samplers/receptors used. Hence, for surface samplers/receptors, it is recommended that the user calculate these concentrations as a post-processing step using the provided postprocessor sciDOSpost that can read the surface deposition and dosage files. For samplers that are not at the surface (e.g., at locations corresponding to aircraft measurements), the user can specify a maximum of 200 sampling locations. The resulting sampler output can be imported into a spreadsheet and compared with observed values.

3) The large scale variance type (ENSM_TYPE) should be set to none for small domains (~<150 kms)
