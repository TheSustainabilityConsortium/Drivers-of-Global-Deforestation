Drivers of Deforestation 2020

## Update Layers

- - [X] Fire Brightness
- - [X] Fire Count
- - [X] Fire FRP
- - [X] FireLoss (1 day)
- - [X] LossYearDiff (1 day)
- - [X] Loss (1 day)
- - [X] Population2000 (1 day)
- - [X] Population2015 (1 day)
- - [X] PopulationDifference20002015 (1 day)
- - [ ] Goode_NetMean (1 day) - Not sure what this is? Not in the original paper...
- - [ ] Goode_Gain_10kMean (1 day)

## To Do

- - [ ] Fix the issue with 2020 population data in geodatabase on PROC2
- - [ ] User stories for fire map
- - [ ] Run the model in R (1 day)
- - [ ] Package everything nicely (1 day)
- - [ ] Update merge_modis script so there is one function to assemble the database from annual files and one function to add new data to the database
- - [ ] Make the merge_modis script executable
- - [ ] Convert toolbox to Python scripts

## General Overview

Use the "Update Drivers" toolbox in ArcGIS

**Process**
1. Create a snap raster to align all the raster datasets
2. Create the 250 m resolution datasets

    - Fire brightness, count, and FRP
    - Loss and loss year difference
    - Fire and loss overlap
    - Population in 2000, 2020, and difference between years

3. Aggregate the 250 m resolution datasets to 10 km
4. Export 10 km datasets
5. Run the R script

**Requirements**

- Need at least 30 GB of free space on your hard drive
- Need ArcGIS, Python, and R installed

## Snap Raster

**Process (30 minutes)**
1. Run the "0. Create snap raster" tool in the toolbox

    - Use the Create Constant Raster tool
        - Set cell size to 10 km
        - Set coordinate system to World Goode Homolosine Land Projection

## Fire Data

**Process (3-4 hours)**

1. Download MODIS fire alerts from NASA using the [Archive Download Tool](https://firms.modaps.eosdis.nasa.gov/download/)

    - Download the global extent one year at a time as a csv
    - Database of fires from 2001 to 2020 are in the 2020update folder on GitHub

2. Merge the new data and old data using the merge_modis.ipynb script

    - Current script merges each year of fire data
    - Consider making the new data a shapefile and then merging in ArcGIS

3. Create a point shapefile from the fire alert database

4. Run the "1. Create 250m fire count, brightness, and FRP, layers from MODIS" tool in the toolbox

    - Use the Point to Raster tool to convert the shapefile to a raster to get the count, brightness, and FRP layers
        - Set cell assignment to count for the count layer and mean for the brightness and FRP layers
        - Set cell size to 250 m
        - Set coordinate system to World Goode Homolosine Land Projection

## Hansen Data

Run this on the virtual machine because it will take a full 24 hours to run

**Process (24-36 hours)**
1. Download the newest loss data from the gfw2-data bucket

2. Run the "2. Create 250 m loss and loss year difference tiles from Hansen" tool in the toolbox

    - Use the Iterate Raster tool to loop through each loss tile in a folder
    - Create the 250 m loss tiles
        - Use the Reclassify tool to reclassify values 1 through 20 as 1
        - Use the Aggregate tool to change the resolution to ~100 m
            - Set the cell factor to 4
            - Set the aggregation technique to mean
            - Use other default settings
        - Use the Project Raster tool to change the resolution to 50 m
            - Set the resampling technique to bilinear interpolation
            - Set cell size to 50 m
            - Set coordinate system to World Goode Homolosine Land Projection
        - Use the Aggregate tool to change the resolution to 250 m
            - Set the cell factor to 5
            - Set the aggregation technique to mean
            - Use other default settings
        - Use the Delete tool to cleanup the workspace and free memory
    - Create the 250 m loss year difference tiles
        - Use the Reclassify tool to reclassify 0 as NODATA
        - Use the Project Raster tool to change the resolution to 50 m
            - Set the resampling technique to majority resampling
            - Set cell size to 50 m
            - Set coordinate system to World Goode Homolosine Land Projection
        - Use the Aggregate tool to get the max and min years for each 250 m cell
            - Run twice
                - Min and max of the 50 m layer
            - Set the cell factor to 5
            - Use other default settings
        - Use the Minus tool to get the loss year difference
            - Subtract the min year from the max year
        - Use the Delete tool to cleanup the workspace and free memory

3. Run the "3. Create 250 m loss and loss difference mosaics" tool in the toolbox

    - Use the Create Mosaic Dataset tool to create an empty mosaic dataset
        - Repeat twice
            - Loss and loss year difference
        - Set coordinate system to World Goode Homolosine Land Projection
    - Use the Add Raster to Mosaic Dataset to add the loss and loss year difference tiles
        - Repeat twice
            - Loss and loss year difference
        - Set coordinate system to World Goode Homolosine Land Projection
        - Set maximum cell size to 250 m

4. Run the "4. Create 250 m fire loss year" tool in the toolbox

    - Use the Extract by Mask tool to get overlap between fire and loss cells
        - Set input raster to the 250 m loss mosaic dataset
        - Set mask raster to the 250 m fire count dataset

## Population Data

**Process (1 hour)**

1. Download the unadjusted [population density](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download) data from CIESIN

    - Download the data at 30-arc second (~1 km) resolution

2. Run the "5. Create 250 m population layers" tool in the toolbox

    - Use the Project Raster tool to change the resolution of each year to 250 m
        - Repeat for each year
        - Set the resampling technique to bilinear interpolation
        - Set cell size to 250 m
        - Set coordinate system to World Goode Homolosine Land Projection
    - Use the Minus tool to get the population difference between years
        - Subtract the 2000 population data from the 2020 population data

## Aggregate Datasets to 10 km

**Process (1-2 hours)**
1. Run the "A. Aggregate 250 m layers to 10 km layers" tool in the toolbox

    - Use the Aggregate tool to change resolution to 1 km
        - Run three times
            - Sum, mean, max of the 250 m layer
        - Set cell factor to 4
        - Use other default settings
    - Use the Aggregate tool again to change the resolution to 1 km
        - Run seven times total
            - Sum, mean, and max of the 1 km sum
            - Mean and max of the 1 km mean
            - Mean and max of the 1 km max
        - Set cell factor to 10
        - Use other default settings

Alternatively, run the "H. Aggregate 250 m layers to 10 km layers" tool on the toolbox on each 250 m dataset individually

## Export Data

**Process (30 minutes)**
1. Run the "B. Export layers to tif to run model" tool in the toolbox

    - Use the Iterate Raster tool to loop through each 10 km dataset
    - Use the Copy Raster tool to export the raster to a TIF file

## Run R Script

**Process (1 hour)**
**
**