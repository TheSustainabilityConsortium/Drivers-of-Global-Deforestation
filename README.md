# Forestry-Model
2018 Curtis et al Forest Loss Driver Classification Model

Download extra data from link: https://thetsc-my.sharepoint.com/:f:/g/personal/forrest_follett_sustainabilityconsortium_org/EvzHtzshe8NIojRHRWaaB_cBmbvukagOE-Uv6uHpg3Ijkg?e=MVcHZU


Extract data into working directory so that the folder 'R_ModelInputs_SecondaryData' and the file 'Goode_LossMask005.tif' are in the root of the working directory.

If you do not have R installed on your system, do that.

Open Supplemental_Data_1.R and find the heading titled 'Set up workspace'.
Edit the line that calls the function setwd() and put the path of your working directory in the parenthesis.  Be sure to use quotation marks. 
For example: setwd("C:/ForestLossModel/")

Run the script from a terminal with the command: Rscript Supplemental_Data_1.R
