Drivers of Deforestation 2020
## General Overview

Use the "Update Drivers 2020" toolbox in "ArcGIS Tools" folder

**Process**
1. Download 2020 tree cover loss data
2. Create a snap raster to align all the raster datasets
3. Create the 250 m resolution datasets

    - Fire brightness, count, and FRP
    - Loss and loss year difference
    - Fire and loss overlap
    - Population in 2000, 2020, and difference between years

4. Aggregate the 250 m resolution datasets to 10 km
5. Export 10 km datasets
6. Run the R script
7. Expand classification in ArcGIS

**Requirements**

- At least 30 GB of free space on hard drive
- ArcGIS, Python, and R installed

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

2. Run the "2. Create 250m loss and loss year difference tiles from Hansen" tool in the toolbox

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
    - Create the 1 km loss year difference tiles
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

3. Run the "3. Create 250m loss and loss year difference mosaics" tool in the toolbox

    - Use the Create Mosaic Dataset tool to create an empty mosaic dataset
        - Repeat twice
            - Loss and loss year difference
        - Set coordinate system to World Goode Homolosine Land Projection
    - Use the Add Raster to Mosaic Dataset to add the loss and loss year difference tiles
        - Repeat twice
            - Loss and loss year difference
        - Set coordinate system to World Goode Homolosine Land Projection
        - Set maximum cell size to 250 m

4. Run the "4. Create 1k loss year difference mosaic" tool in the toolbox
    - Use the Create Mosaic Dataset tool to create an empty mosaic dataset
        - Set coordinate system to World Goode Homolosine Land Projection
    - Use the Add Raster to Mosaic Dataset to add the loss and loss year difference tiles
        - Set coordinate system to World Goode Homolosine Land Projection
        - Set maximum cell size to 1 km

5. Run the "5. Create 250m fire loss year" tool in the toolbox

    - Use the Extract by Mask tool to get overlap between fire and loss cells
        - Set input raster to the 250 m loss mosaic dataset
        - Set mask raster to the 250 m fire count dataset

## Population Data

**Process (1 hour)**

1. Download the unadjusted [population density](https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-rev11/data-download) data from CIESIN

    - Download the data at 30-arc second (~1 km) resolution

2. Run the "6. Create 250m population layers" tool in the toolbox

    - Use the Project Raster tool to change the resolution of each year to 250 m
        - Repeat for each year
        - Set the resampling technique to bilinear interpolation
        - Set cell size to 250 m
        - Set coordinate system to World Goode Homolosine Land Projection
    - Use the Minus tool to get the population difference between years
        - Subtract the 2000 population data from the 2020 population data

## Aggregate Datasets to 10 km

**Process (1-2 hours)**
1. Run the "A1. Aggregate 250 m layers to 10 km layers" tool in the toolbox

    - Use the Aggregate tool to change resolution to 1 km
        - Run three times
            - Sum, mean, max of the 250 m layers
        - Set cell factor to 4
        - Use other default settings
    - Use the Aggregate tool again to change the resolution to 10 km
        - Run seven times total
            - Sum, mean, and max of the 1 km sum
            - Mean and max of the 1 km mean
            - Mean and max of the 1 km max
        - Set cell factor to 10
        - Use other default settings

Alternatively, run the "H. Aggregate 250m layers to 10km layers" tool on the toolbox on each 250 m dataset individually

2. Run the "A2. Aggregate other 10km layers" tool in the toolbox

    - Use the Aggregate tool to change resolution to 10 km
        - Run three times
            - Sum, mean, max of the 1 km layer
        - Set cell factor to 10
        - Use other default settings
    - Use the Minus tool to subtract the 10 km forest gain layer from the 10 km forest loss layer

## Export Data

**Process (30 minutes)**
1. Run the "B. Export layers to TIF" tool in the toolbox

    - Use the Iterate Raster tool to loop through each 10 km dataset
    - Use the Copy Raster tool to export the raster to a TIF file


## Make forest loss mask

**Process (30 minutes)**
1. Run the "C. Make loss mask" tool in the toolbox

    - Use the Set Null tool to get rid of cells with less than 0.5% forest loss
        - Set the expression to VALUE < 0.005
        - Set the false raster as the forest loss layer 10 km mean

## Run R Script

**Process (1 hour)**
1. Run the R script

## Expand the initial classification
1. Run the "D. Expand initial classificaiton" tool in the toolbox

    - Use the Int tool to convert the initial classification from the R script to an integer raster
    - Use the Expand tool to expand the classification
        - Set the number of cells to 50
        - Set the zone values as 1, 2, 3, 4, 5
        - Set the expand method as morphological
    - Use the Int tool to convert the loss mask to an integer raster
    - Use the Extract by Mask tool to create the final classification layer
        - Set the loss mask as the mask data