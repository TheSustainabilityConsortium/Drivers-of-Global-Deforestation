# Forestry-Model
This is the driver classification model referenced in [Curtis et al. 2018, Classifying Drivers of Global Forest Loss](https://science.sciencemag.org/content/361/6407/1108.editor-summary)

Modifications have been made to the code published with the article to make it more user-friendly, and it is being actively maintained and updated as new information becomes available.

## How to run the model
1. If you do not have R installed on your system, you need to do that first.  See instructions [here](https://www.r-project.org/).

2. Clone this repo.

3. If you want to run the model in its most up to date form, skip to step 4. If you want to run the model only for the initial period that was published in *Science* (2000-2015):
  - Copy the contents of R_ModelInputs_PrimaryData into R_ModelInputs_SecondaryData, overwriting the contents of R_ModelInputs_SecondaryData.
  - Download [this](https://www.sustainabilityconsortium.org/tsc-downloads/driver-model/?wpdmdl=33327&ind=1574462515859) 2015 version of the LossMask005.tif file and use it to overwrite the version that you cloned from this repo in your working directory.

4. Open Supplemental_Data_1.R and look under the heading titled 'Settings'. Edit the line that calls the function setwd() and put the path of your working directory in the parenthesis.  Be sure to use quotation marks. For example: setwd("C:/ForestLossModel/")

5. Run the script from a terminal with the command: Rscript Supplemental_Data_1.R
