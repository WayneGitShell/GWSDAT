CHANGES IN VERSION 3.1999999
* Implementation of well influence analysis: https://github.com/peterradv/Well-Influence-Analysis
* Updated User Manual for version 3.2. 
* Updated Excel Add-in - more robust to 32 bit versus 64 bit version of Excel. Removed installation of rgdal. 
* Fixed bug Threshold Value does not display in export of time-series graphs: #252
* Functionality to read in R data.frames directly. 
* Beta implementation of api style argument passing to online version.
* Added GW Well Report Functionality.
* Added local save options to CSV generation of Monitoring data file from Excel Add-in. More robust date format identification. 
* Improved messaging describing how GWSDAT behaves when concentration and NAPL data observed together, i.e. prefers to use conc data rather than substitution. 
* Support for Windows Meta File image format output for spatial plot. Useful for rearranging overlapping well labels. 

CHANGES IN VERSION 3.1.1 - Released 2021/07/08

* Cumulative bug fixes since the release of 3.1.0

CHANGES IN VERSION 3.1.0 - Released 2021/07/08

* Well Redundancy Analysis: This allows the user to very conveniently drop a well or a combination of wells from the analysis and investigate the resultant impact. 

* Updated User Manual: http://gwsdat.net/gwsdat_manual. A fully comprehensive description of GWSDAT, updated for version 3.1. 

* Excel Add-in Menu: New Excel menu designed to be clearer to navigate and easier to install. 

* Custom Colour Key:  In response to user feedback, the latest version has the functionality to customise the colour key in the main GWSDAT spatial plot. 

* Updated branding: The Excel data input templates have been updated with more contemporary colour schemes.

* Bug Fixes: Numerous bug fixes and enhancements. More robust data input procedures which report more thoroughly on any potential issues, e.g. missing data, incorrect units, etc.


CHANGES IN VERSION 3.0.6 - Released 2021/02/19

BUG FIXES

* Fixed High resolution model fit in Shiny options menu. 

* Modified to be backwards compatible with tsearch function in geometry package.

* Fixed Model dialog box issue to correctly show Glasgow T+Cs. 

* Fixed csv input date format issue

