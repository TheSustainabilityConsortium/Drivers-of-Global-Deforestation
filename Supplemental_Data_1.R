#---------------
# Load packages #
#---------------
# Packages needed
package.list <- c("rgdal", "raster", "dplyr", "foreign", "tidyr", "readr", "stringr",
                  "rpart", "rpart.plot", "rattle", "crayon")
# If the packages aren't installed, install them
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load the packages
lapply(package.list, library, character.only = TRUE)

#---------------
# Define functions
#---------------
pause <- function() {
  if (interactive()){
    line <- invisible(readline(prompt="Press [enter] to continue..."))
  }
  else {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
    close(file("stdin"))
  }
}

# writes 'words' to terminal in green letters and makes a new line
say <- function(words){
  cat(green(words),"\n")
}

# writes 'words' to the terminal in red letters and makes a new line
bad <- function(words){
  cat(red(words),"\n")
}

# df is a dataframe with minimum of GoodeR.ID and 1 data column, filename and column are char strings
rastOut <- function(df, filename, column) {
  data=1:6961896%>%
    as.data.frame()%>%
    rename("GoodeR.ID"=".")%>%
    left_join(df,by="GoodeR.ID")%>%
    select(GoodeR.ID,column)
  names(data)=c("GoodeR.ID","data")
  list=data%>%
    select(data)
  list=as.vector(t(list))
  m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
  m=(t(m))
  r=raster(m)
  xmin(r)=-20037506.5672
  xmax(r)=20042493.4328
  ymin(r)=-8695798.3918
  ymax(r)=8674201.6082
  crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"
  writeRaster(r,filename=filename,type="GTIFF",overwrite=TRUE)
}

#---------------
# Set up workspace #
#---------------
setwd("E:/Forestry Model/clean/")

sub_dir1 <- "Temp_RModelOutputsTiffs"
sub_dir2 <- "R_FinalOutputs"
temp_dir1 <- file.path(getwd(), sub_dir1)
temp_dir2 <- file.path(getwd(), sub_dir2)

# create output directories if they don't already exist
if (!dir.exists(temp_dir1)){
  say("")
  say("Creating temp directory...")
  say("")
  dir.create(temp_dir1)
} else {
  say("")
  say("Temp directory already exists, so won't be created.")
  say("")
}

if (!dir.exists(temp_dir2)){
  say("")
  say("Creating Final Outputs directory...")
  say("")
  dir.create(temp_dir2)
} else {
  say("")
  say("Final Outputs directory already exists, so won't be created.")
  say("")
}


#---------------
# Load input datasets #
#---------------

# Required:
say("Reading Region Boundaries...")
GoodeR_Boundaries_Region=read_csv("GoodeR_Boundaries_Region.csv", col_types = cols(GoodeR.ID = col_integer(), Region = col_integer()))
say("Reading Training Points...")
TrainingPoints = read_csv("TrainingPointsFull.csv", col_types = cols(GoodeR.ID = col_integer(), Training.Class = col_integer()))
say("Reading Loss Mask...")
LossMaskFull= read_csv("LossMaskFull.csv", col_types = cols(col_integer(), col_number()))
GoodeR=1:6961896%>%
  as.vector()%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")

# Optional:
# Look for GoodeR_SecondaryData in workspace.  Import or notify user it is missing.
if (TRUE %in% (list.files() == 'GoodeR_SecondaryData.csv')) {
  say("Reading GoodeR_SecondaryData...")
  GoodeR_SecondaryData=read_csv("GoodeR_SecondaryData.csv")
  GoodeR_SecondaryData=GoodeR_SecondaryData%>%
    filter(GoodeR_SecondaryData[41]>0)
} else {
  say("Secondary Data not found in workspace.  It will be calculated.")
}

# Look for TrainingPoints_PrimaryData in workspace.  Import or notify user it is missing.
if (TRUE %in% (list.files() == 'TrainingPoints_PrimaryData.csv')) {
  say("Reading Training Points...")
  TrainingPoints_PrimaryData=read_csv("TrainingPoints_PrimaryData.csv") 
} else {
  say("Training Points Primary Data not found in workspace.  It will be calculated.")
}

# Get list of secondary data files
FileList=as.data.frame(list.files(path = "./R_ModelInputs_SecondaryData",
                                  pattern = ".tif$", all.files = FALSE,
                                  full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
names(FileList)=c("FileName")
Variables <- FileList
names(Variables)=c("Name")
# Strip file prefixes and suffixes
Variables$Name <- Variables$Name %>%
  str_replace("Goode_","") %>%
  str_replace(".tif","")

#---------------
# Create Training points with data #
#---------------
  
say("Checking inputs for TrainingPoints_PrimaryData...")

for(NAME in FileList$FileName){
  data=raster(paste("./R_ModelInputs_SecondaryData/",NAME,sep=""))
  
  if(nrow(data)!=1737){
    bad(c(NAME, "has a problem"))
  } else{
    if (ncol(data)!=4008) {
      bad(c(NAME, "has a problem"))
    }else{
      say(c(NAME, "looks good"))
    }
  }
}

say("If all inputs look good, press Enter, if any failed, press Ctrl+C then Enter to quit and fix them.")
pause()

say("Calculating Training Points")
temp=as.data.frame(c(1:nrow(TrainingPoints)))
names(temp)=c("TrainingID")
# Make columns for each driver (1 is that class, 0 is not that class)
TrainingPoints=TrainingPoints%>%
  bind_cols(temp)%>%
  mutate(Deforestation=ifelse(Training.Class==1,1,0))%>%
  mutate(Shifting.Agriculture=ifelse(Training.Class==2,1,0))%>%
  mutate(TreeFarm.ForestryOther=ifelse(Training.Class==3,1,0))%>%
  mutate(Wildfire=ifelse(Training.Class==4,1,0))%>%
  mutate(Urban=ifelse(Training.Class==5,1,0))


TrainingPoints_PrimaryData=TrainingPoints%>%
  filter(Training.Class!=7)
#activate this
#write_csv(TrainingPoints,"TrainingPoints19.csv")

for(NAME in FileList$FileName){
  say(c("Reading", NAME))
  data=raster(paste("./R_ModelInputs_SecondaryData/",NAME,sep=""))
  NAME2=NAME%>%
    str_replace("^Goode_", "")%>%
    str_replace(".tif$", "")
  ## Select GRID data raster ##
  data=data%>%
    as.vector()%>%
    as.data.frame()
  names(data)=c(paste(NAME2))
  ## create R.ID list ##
  GoodeRList=1:6961896%>%
    as.vector()%>%
    as.data.frame()%>%
    rename("GoodeR.ID"=".")%>%
    bind_cols(data)
  TrainingPoints_PrimaryData=TrainingPoints_PrimaryData%>%
    left_join(GoodeRList,by="GoodeR.ID")
}
TrainingPoints_PrimaryData[is.na(TrainingPoints_PrimaryData)]=0
TrainingPoints_PrimaryData=TrainingPoints_PrimaryData%>%
  distinct()
  
  # Disabling this because it just creates the possibility of contamination from old data.  If the model is 
  # being run again, it's worth recalculating the secondary data, because presumably, something has changed.
  #
  # say("Writing TrainingPoints_PrimaryData.csv to working directory...")
  # write_csv(TrainingPoints_PrimaryData,"TrainingPoints_PrimaryData.csv")

#---------------
# Create full list of Secondary Data #
#---------------
## Mask by loss extent to make smaller ##

say("Calculating GoodeR_SecondaryData...")

GoodeR_SecondaryData=LossMaskFull%>%
  filter(LossMaskFull[2] > 0)%>%
  select(GoodeR.ID)
tppd=TrainingPoints_PrimaryData%>%
  select(GoodeR.ID,TrainingID)
GoodeR_SecondaryData=GoodeR_SecondaryData%>%
  left_join(tppd,by="GoodeR.ID")
GoodeR_SecondaryData$TrainingID[is.na(GoodeR_SecondaryData$TrainingID)]=0

for(NAME in FileList$FileName){
  
  data=raster(paste("./R_ModelInputs_SecondaryData/",NAME,sep=""))
  
  say(c("Reading ", NAME))
  
  NAME2=NAME%>%
    str_replace("^Goode_", "")%>%
    str_replace(".tif$", "")
  
  ## Select GRID data raster ##
  data=data%>%
    as.vector()%>%
    as.data.frame()
  names(data)=c(paste(NAME2))
  ## create R.ID list ##
  GoodeRList=GoodeR%>%
    bind_cols(data)
  GoodeR_SecondaryData=GoodeR_SecondaryData%>%
    left_join(GoodeRList,by="GoodeR.ID")
}
GoodeR_SecondaryData[is.na(GoodeR_SecondaryData)]=0
GoodeR_SecondaryData=GoodeR_SecondaryData%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")
GoodeR_SecondaryData=GoodeR_SecondaryData%>%
   filter(!is.na(Region))

# Disabling this because it just creates the possibility of contamination from old data.  If the model is 
# being run again, it's worth recalculating the secondary data, because presumably, something has changed.
#
# say("Writing GoodeR_SecondaryData.csv to working directory...")
# write_csv(GoodeR_SecondaryData, "./GoodeR_SecondaryData.csv")

#----

#-----------------------
## Create Rpart Fits ##
#-----------------------

regions <- c(1, 2, 3, 4, 5, 6, 7)
driverNames <- c("Deforestation", "Shifting.Agriculture", "TreeFarm.ForestryOther", "Wildfire", "Urban")
drivers <- c(1, 2, 3, 4, 5)
ModelOutput <- list()
ModelOutput.Regional <- list()
regionalOutputs <- list()


for (region in regions){
  say("")
  say("********************")
  say(c("* Setting up Region",region))
  say("********************")
  
  # extract only training points within the current region from the full set
  TrainingPoints_Regional <- TrainingPoints_PrimaryData%>%
    left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
    mutate(Region=replace(Region,,as.numeric(Region)))%>%
    filter(Region==c(region))

  for (driver in drivers){
    say(c("Calculating",driverNames[driver],"Tree..."))
    
    # select the column for the current driver, then all variable columns
    InputData <- select(TrainingPoints_Regional,driverNames[driver],Variables$Name[1]:Variables$Name[length(Variables$Name)])
    
    # give rpart a formula where the current driver is defined as the sum of all variables in Variables$Name, pass InputData and define method
    fit <- rpart(as.formula(paste(paste(driverNames[driver],"~",sep=""),paste(Variables$Name, collapse="+"))),data=InputData,method = "anova")
    fit=prune(fit,cp=.02)
    
    # plot the tree and give it a title of 'Region: Driver'
    fancyRpartPlot(fit, main = paste("Region", region, ":", driverNames[driver], sep = " "))
    
    # generate name for model output field
    outName <- paste("Output", driverNames[driver], sep = "_")
    
    # calculate probability of current driver for whole dataset
    # Give tree full dataset and record probability for each (to be used later in regional voting)
    ModelOutput[[driver]] = GoodeR_SecondaryData%>%
      select(GoodeR.ID)%>%
      left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
      # the "!! outName :=" uses the variable value as the new column name, rather than the literal string 'outName'
      mutate(!!outName := predict(fit, type="vector", newdata=GoodeR_SecondaryData))%>%
      filter(Region==paste(region))%>%
      select(-Region)
  }
  # Collect class probabilities from each tree in ModelOutput.Full
  ModelOutput.Regional[[region]] <- as.data.frame(Reduce(function(x, y) merge(x, y, by="GoodeR.ID", all.x=TRUE), ModelOutput))
  
  say("********************")
  say(c("* Region",region,"Complete!"))
  say("********************")
}

# Combine all regions into the final table
ModelOutput.All <- Reduce(function(x, y) bind_rows(x, y), ModelOutput.Regional)

#-----------------------
# Combine into final output list #
#-----------------------
# say("")
# say("Combining Regions...")
# 
# ModelOutput.Final_All=ModelOutput.Final__1%>%
#   bind_rows(ModelOutput.Final__2)%>%
#   bind_rows(ModelOutput.Final__3)%>%
#   bind_rows(ModelOutput.Final__4)%>%
#   bind_rows(ModelOutput.Final__5)%>%
#   bind_rows(ModelOutput.Final__6)%>%
#   bind_rows(ModelOutput.Final__7)
# 
# say("Writing raw model output to ModelOutput.Final_19.csv")
# 
# write_csv(ModelOutput.Final_All,"ModelOutput.Final_19.csv")
#------

#---------------
# view decision tree Rplots
#---------------
# say("Plotting Decision Trees...")
# 
# fancyRpartPlot(Fit_Deforestation__1, main = "Deforestation 1")
# fancyRpartPlot(Fit_Deforestation__2, main = "Deforestation 2")
# fancyRpartPlot(Fit_Deforestation__3, main = "Deforestation 3")
# fancyRpartPlot(Fit_Deforestation__4, main = "Deforestation 4")
# fancyRpartPlot(Fit_Deforestation__5, main = "Deforestation 5")
# fancyRpartPlot(Fit_Deforestation__6, main = "Deforestation 6")
# fancyRpartPlot(Fit_Deforestation__7, main = "Deforestation 7")
# 
# fancyRpartPlot(Fit_Shifting.Agriculture__1, main = "Shifting Ag 1")
# fancyRpartPlot(Fit_Shifting.Agriculture__2, main = "Shifting Ag 2")
# fancyRpartPlot(Fit_Shifting.Agriculture__3, main = "Shifting Ag 3")
# fancyRpartPlot(Fit_Shifting.Agriculture__4, main = "Shifting Ag 4")
# fancyRpartPlot(Fit_Shifting.Agriculture__5, main = "Shifting Ag 5")
# fancyRpartPlot(Fit_Shifting.Agriculture__6, main = "Shifting Ag 6")
# fancyRpartPlot(Fit_Shifting.Agriculture__7, main = "Shifting Ag 7")
# 
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__1, main = "Forestry 1")
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__2, main = "Forestry 2")
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__3, main = "Forestry 3")
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__4, main = "Forestry 4")
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__5, main = "Forestry 5")
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__6, main = "Forestry 6")
# fancyRpartPlot(Fit_TreeFarm.ForestryOther__7, main = "Forestry 7")
# 
# fancyRpartPlot(Fit_Wildfire__1, main = "Wildfire 1")
# fancyRpartPlot(Fit_Wildfire__2, main = "Wildfire 2")
# fancyRpartPlot(Fit_Wildfire__3, main = "Wildfire 3")
# fancyRpartPlot(Fit_Wildfire__4, main = "Wildfire 4")
# fancyRpartPlot(Fit_Wildfire__5, main = "Wildfire 5")
# fancyRpartPlot(Fit_Wildfire__6, main = "Wildfire 6")
# fancyRpartPlot(Fit_Wildfire__7, main = "Wildfire 7")
# 
# fancyRpartPlot(Fit_Urban__1, main = "Urban 1")
# fancyRpartPlot(Fit_Urban__2, main = "Urban 2")
# fancyRpartPlot(Fit_Urban__3, main = "Urban 3")
# fancyRpartPlot(Fit_Urban__4, main = "Urban 4")
# fancyRpartPlot(Fit_Urban__5, main = "Urban 5")
# fancyRpartPlot(Fit_Urban__6, main = "Urban 6")
# fancyRpartPlot(Fit_Urban__7, main = "Urban 7")

#-----

#---------------
#Create initial classification using model output
#---------------

#ModelOutput.Final=read_csv("ModelOutput.Final_19.csv")
say("Trees are voting on each pixel (this takes a while; it may be a good time to go get some coffee)...")

temp=ModelOutput.All%>%
  select(Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)%>%
  rename("out1" = "Output_Deforestation", "out2" = "Output_Shifting.Agriculture", "out3" = "Output_TreeFarm.ForestryOther",
         "out4" = "Output_Wildfire", "out5" = "Output_Urban")
test=as.data.frame(colnames(temp)[apply(temp,1,which.max)])
names(test)=c("MaxClass")

temp=temp%>%
  bind_cols(test)

MaxClass=temp%>%
  rowwise()%>%
  mutate(maxValue=max(out1, out2, out3, out4, out5))

MaxClass$MaxClass <- gsub("out", "", MaxClass$MaxClass)

MaxClass <- within(MaxClass, MaxClass[maxValue < 0.5] <- 0)%>%
  rename("Class" = "MaxClass")
  
MaxClass <- transform(MaxClass, Class = as.numeric(Class))

# MaxClass=MaxClass%>%
#   mutate(Class=ifelse(maxValue<.5,0,
#                       ifelse(MaxClass=="Output_Deforestation",1,
#                              ifelse(MaxClass=="Output_Shifting.Agriculture",2,
#                                     ifelse(MaxClass=="Output_TreeFarm.ForestryOther",3,
#                                            ifelse(MaxClass=="Output_Wildfire",4,
#                                                   ifelse(MaxClass=="Output_Urban",5,0)
#                                            ))))))
MaxClass_Final_19_50uncertain=ModelOutput.All%>%
  select(GoodeR.ID)%>%
  bind_cols(MaxClass)%>%
  left_join(LossMaskFull,by = "GoodeR.ID")%>%
  mutate(Class2=ifelse(as.numeric(Loss)<.005,0,Class))%>%
  select(-Class)%>%
  mutate(Class=Class2)%>%
  select(-Class2)%>%
  select(-Loss)

# Disabling  this because it just creates the possibility of contamination from old data.  If the model is 
# being run again, it's worth recalculating the secondary data, because presumably, something has changed.
#
# say("Outputting initial class selections to MaxClass_Final_19_50uncertain.csv")
# 
# write_csv(MaxClass_Final_19_50uncertain,"MaxClass_Final_19_50uncertain.csv")
#----

#--------------------------------------------------------
# Initial raster 
#--------------------------------------------------------

MaxClass_Final=MaxClass_Final_19_50uncertain%>%
  select(GoodeR.ID,Class)


say("Writing initial class selections to Goode_FinalClassification_19_50uncertain.tiff")

rastOut(MaxClass_Final, "Goode_FinalClassification_19_50uncertain.tiff", "Class")

# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(MaxClass_Final,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Class)
# names(data)=c("GoodeR.ID","data")
# data=distinct(data)
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 

# plot(r)

#writeRaster(r,filename="Goode_FinalClassification_19_50uncertain.tiff",type="GTIFF",overwrite=TRUE)


#---------------
# prepare for expand in arcMap
#---------------

MaxClass_Final <- MaxClass_Final%>%
  filter(Class > 0)

rastOut(MaxClass_Final, "Goode_FinalClassification_19_Excludeduncertain.tif", "Class")

# MaxClass_Final=MaxClass_Final_19_50uncertain%>%
#   select(GoodeR.ID,Class)%>%
#   filter(Class>0)
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(MaxClass_Final,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Class)
# names(data)=c("GoodeR.ID","data")
# data=distinct(data)
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"

# plot(r)

# writeRaster(r,filename="Goode_FinalClassification_19_Excludeduncertain.tif",type="GTIFF",overwrite=TRUE)


# Expand in ArcGIS
#---------------
say("")
say("**************************************************************************")
say("***************************** MODEL PAUSED *******************************")
say("Run 'Expand Final Classification' model, found in 'Forestry Models 2.tbx'.")
say("This uses the Expand tool to classify uncertain pixels (model certainty < 50%) using nearest neighbor technique")
say("When finished with the Expand model, the output is the final classification.")
say("You may press Enter to continue calculating stats, or Ctrl+C, then Enter to quit.")
pause()

#-------------
# Calculate % of loss classified (non-mixed/uncertain) #
#-------------
temp=GoodeR_SecondaryData%>%
  select(!!c(1,41))
Total=sum(temp[2])

LossClassified=temp%>%
  left_join(MaxClass_Final_19_50uncertain,by="GoodeR.ID")%>%
  filter(Class!=0)
LossClassified=sum(LossClassified[2])/Total

LossUnClassified=temp%>%
  left_join(MaxClass_Final_19_50uncertain,by="GoodeR.ID")%>%
  filter(Class==0)
LossUnClassified=sum(LossUnClassified[2])/Total

say("")
say(cat("Loss classified:", LossClassified, "%"))
say(cat("Loss unclassified:", LossUnClassified, "%"))
say("")
#-----

# This step is already done when creating the initial classification raster.  Not necessary, remove after testing.
#-----------------------------------------
# Re-import Expanded classes (no Uncertain class) mask out areas with loss less than 0.5% loss
#-----------------------------------------

data=raster("R_FinalOutputs/Goode_FinalClassification_19_50uncertain_expanded_05pcnt.tif")
data=as.vector(data)%>%
  as.data.frame()
names(data)=c("Class")
# create R.ID list ##
RasterList=1:6961896%>%
  as.vector()%>%
  as.data.frame()
names(RasterList)=c("GoodeR.ID")
temp=RasterList%>%
  bind_cols(data)%>%
  inner_join(LossMaskFull,by="GoodeR.ID")%>%
  mutate(Class2=ifelse(as.numeric(Loss_10kMean_20002016)>0&as.numeric(Loss_10kMean_20002016)<.005,0,Class))%>%
  mutate(Class.Final=round(Class2,0))%>%
  select(-Class,Class2,-Loss_10kMean_20002016)

Goode_FinalClassification19_Expand_05pcnt=temp
writeRaster(Goode_FinalClassification19_Expand_05pcnt, filename = "R_FinalOutputs/TestMask.tif", type="GTIFF", overwrite = TRUE)

write_csv(Goode_FinalClassification19_Expand_05pcnt,"FinalClass_19_05pcnt.csv")
#-----

#--------------------------------------------------------
# Generate Loss Masks for Final classification (weight classification by loss)
#--------------------------------------------------------

say("Generating loss masks for each class")
#FinalClass_19 = read_csv("./FinalClass_19_05pcnt.csv", col_types = cols(Class.Final = col_integer()))
FinalClass_19 = Goode_FinalClassification19_Expand_05pcnt

#GoodeR_SecondaryData=read_csv("GoodeR_SecondaryData.csv")

LossData=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  group_by(GoodeR.ID)%>%
  summarize(Loss_10kMean_20002016=mean(Loss_10kMean_20002016,na.rm=TRUE))%>%
  ungroup()%>%
  left_join(FinalClass_19,by="GoodeR.ID")

LossMask_19_Deforestation=LossData%>%
  filter(Class.Final==1)
write_csv(LossMask_19_Deforestation,"LossMask_19_Deforestation.csv")
rastOut(LossMask_19_Deforestation, "LossMask_19_Deforestation.tif", Loss_10kMean_20002016)

LossMask_19_Shifting.Agriculture=LossData%>%
  filter(Class.Final==2)
write_csv(LossMask_19_Shifting.Agriculture,"LossMask_19_Shifting.Agriculture.csv")
rastOut(LossMask_19_Shifting.Agriculture, "LossMask_19_Shifting.Agriculture.tif", Loss_10kMean_20002016)

LossMask_19_Forestry=LossData%>%
  filter(Class.Final==3)
write_csv(LossMask_19_Forestry,"LossMask_19_Forestry.csv")
rastOut(LossMask_19_Forestry, "LossMask_19_Forestry.tif", Loss_10kMean_20002016)

LossMask_19_Wildfire=LossData%>%
  filter(Class.Final==4)
write_csv(LossMask_19_Wildfire,"LossMask_19_Wildfire.csv")
rastOut(LossMask_19_Wildfire, "LossMask_19_Wildfire.tif", Loss_10kMean_20002016)

LossMask_19_Urban=LossData%>%
  filter(Class.Final==5)
write_csv(LossMask_19_Urban,"LossMask_19_Urban.csv")
rastOut(LossMask_19_Urban, "LossMask_19_Urban.tif", Loss_10kMean_20002016)

LossMask_19_MinorLoss=LossData%>%
  filter(Class.Final==0)
write_csv(LossMask_19_MinorLoss,"LossMask_19_MinorLoss.csv")
rastOut(LossMask_19_MinorLoss, "LossMask_19_MinorLoss.tif", Loss_10kMean_20002016)
#---

#---------------------
# Final Class _19 Raster export
#---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(FinalClass_19,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Class.Final)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="R_FinalOutputs/Goode_FinalClassification_19_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# 
# 
# #---------------------
# # LossMask_19_Deforestation Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(LossMask_19_Deforestation,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_LossMask_19_Deforestation_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# #----
# 
# #---------------------
# # LossMask_19_Shifting.Agriculture Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(LossMask_19_Shifting.Agriculture,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_LossMask_19_Shifting.Agriculture_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# #----
# 
# #---------------------
# # LossMask_19_Forestry Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(LossMask_19_Forestry,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_LossMask_19_Forestry_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# #----
# 
# #---------------------
# # LossMask_19_Wildfire Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(LossMask_19_Wildfire,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_LossMask_19_Wildfire_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# #----
# 
# #---------------------
# # LossMask_19_Urban Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(LossMask_19_Urban,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_LossMask_19_Urban_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# #----
# 
# #---------------------
# # LossMask_19_MinorLoss Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(LossMask_19_MinorLoss,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_LossMask_19_MinorLoss_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# #----

# #--------------------------------------------------------
# # separate output by region
# #--------------------------------------------------------
# 
# data=raster("Goode_Boundaries_Region.tif")
# 
# ## Select GRID data raster ##
# data=data%>%
#   as.vector()%>%
#   as.data.frame()
# 
# names(data)=c("Region")
# ## create R.ID list ##
# GoodeR_Boundaries_Region=1:6961896%>%
#   as.vector()%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   bind_cols(data)
# #write_csv(GoodeR_Boundaries_Region,"GoodeR_Boundaries_Region.csv")
# 
# FinalClass_19=read_csv("FinalClass_19_05pcnt.csv", col_types = cols(Class.Final = col_number()))
# 
# #---------------------
# # add strata field #
# #---------------------
# 
# MaxClass_Final=FinalClass_19%>%
#   filter(!is.na(Class.Final))%>%
#   left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
#   mutate(Strata=Class.Final*10+Region)
# 
# StrataCount=MaxClass_Final%>%
#   mutate(Strata.Name=as.character(round(Strata,0)))%>%
#   group_by(Strata.Name)%>%
#   summarize(Count=n())%>%
#   filter(Strata.Name!="1",Strata.Name!="2",Strata.Name!="3",Strata.Name!="4",Strata.Name!="5",Strata.Name!="6",Strata.Name!="7")
# sum=sum(StrataCount$Count)
# 
# #---------------------
# # experiment with sampling size (how many cells will be required?) #
# #---------------------
# 
# StrataCount=StrataCount%>%
#   ungroup()%>%
#   mutate(Count.pcnt=Count/sum)%>%
#   mutate(num=round(Count.pcnt*750,0))%>%
#   mutate(num30=ifelse(Count<30,Count,ifelse(num<30,30,num)))%>%
#   mutate(num40=ifelse(Count<40,Count,ifelse(num<40,40,num)))%>%
#   mutate(num50=ifelse(Count<50,Count,ifelse(num<50,50,num)))
# 
# num=sum(StrataCount$num)
# num30=sum(StrataCount$num30)
# num40=sum(StrataCount$num40)
# num50=sum(StrataCount$num50)
# 
# temp=MaxClass_Final%>%
#   select(-GoodeR.ID)%>%
#   distinct()%>%
#   mutate(Strata.Name=as.character(Strata))
# 
# StrataCount=StrataCount%>%
#   left_join(temp,by="Strata.Name")%>%
#   select(Strata.Name,Region,Class.Final,num40,Count)
# 
# 
# write_csv(StrataCount,"StrataCount_19.csv")
# 
# #---------------------
# # Strata Raster export
# #---------------------
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(MaxClass_Final,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Strata)%>%
#   distinct()
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_Strata_All_19.tiff",type="GTIFF",overwrite=TRUE)
# #----
# 
# 
# #---------------------
# # Strata Sampling by region
# #---------------------
# temp=StrataCount
# sum(temp$num40)
# 
# StrataName=StrataCount%>%
#   select(Strata.Name)
# 
# Strata.Sample.Final=data.frame()
# 
# for(Strata.ID in StrataName$Strata.Name){
#   
#   Num=StrataCount%>%
#     filter(Strata.Name==Strata.ID)%>%
#     inner_join(StrataName,by="Strata.Name")%>%
#     select(num40)
#   Num=as.numeric(Num)
#   
#   Strata=MaxClass_Final%>%
#     filter(Strata==Strata.ID)%>%
#     select(GoodeR.ID)
#   Strata=as.vector(Strata)
#   
#   Sample=sample_n(Strata,size=Num,replace=FALSE)%>%
#     mutate(Strata.Name=Strata.ID)
#   
#   Strata.Sample.Final=Strata.Sample.Final%>%
#     bind_rows(Sample)
# }
# 
# 
# ROWNAME=as.data.frame(c(1:1537))
# names(ROWNAME)=c("Sample.ID")
# 
# Strata.Sample.Final=Strata.Sample.Final%>%
#   bind_cols(ROWNAME)
# 
# 
# write_csv(Strata.Sample.Final,"Strata.Sample.Final_19_05pcnt.csv")
# 
# # Strata Sample cells Raster export
# 
# data=1:6961896%>%
#   as.data.frame()%>%
#   rename("GoodeR.ID"=".")%>%
#   left_join(Strata.Sample.Final,by="GoodeR.ID")%>%
#   select(GoodeR.ID,Sample.ID)
# names(data)=c("GoodeR.ID","data")
# list=data%>%
#   select(data)
# list=as.vector(t(list))
# m=matrix(data=list,nrow=4008,ncol=1737,byrow=FALSE,dimnames=NULL)
# m=(t(m))
# r=raster(m)
# xmin(r)=-20037506.5672
# xmax(r)=20042493.4328
# ymin(r)=-8695798.3918
# ymax(r)=8674201.6082
# crs(r) = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +" 
# plot(r)
# writeRaster(r,filename="Goode_Strata_SampleCells_StrataSampleID_19_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
# 
# # In ArcMap, create shapefile, sort by Sample.ID
# 
# #---------------------
# # Create KMLs
# #---------------------
# 
# polygon <- readOGR(".", "StrataSampleID_Cells_19_05pcnt_sort")
# crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  
# 
# polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# writeOGR(polygon2, dsn="SampleID_RegionStrata_19_1503_sort.kml", layer="polygon", driver="KML",overwrite=TRUE)
# 
# #----
# # Create KML
# 
# polygon <- readOGR(".", "SampleCell_Loss250m_RegionStrata_19_05pcnt")
# crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  
# 
# polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# writeOGR(polygon2, dsn="SampleCell_Loss250m_RegionStrata_19_05pcnt.kml", layer="polygon", driver="KML")
# 
# #----
# # Create Training KML
# 
# polygon <- readOGR(".", "TrainingCell_Loss250m_19")
# crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  
# 
# polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# writeOGR(polygon2, dsn="TrainingCellsClass17.kml", layer="polygon", driver="KML", overwrite=TRUE)
# 
# polygon <- readOGR(".", "TrainingCellsClass_19")
# crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  
# 
# polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# writeOGR(polygon2, dsn="TrainingCellsClass_19.kml", layer="polygon", driver="KML", overwrite=TRUE)