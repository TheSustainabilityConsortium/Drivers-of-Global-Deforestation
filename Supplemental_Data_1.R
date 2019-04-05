#---------------
# Load packages #
#---------------
# Packages needed
package.list <- c("rgdal", "raster", "dplyr", "foreign", "tidyr", "readr", "stringr",
                  "rpart", "rpart.plot", "rattle", "crayon")

new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
# If the packages aren't isntalled, install them
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

#---------------
# Set workspace #
#---------------
setwd("E:/Forestry Model/Consolidated2/")

#---------------
# Load input datasets #
#---------------

# Required:
say("Reading Region Boundaries...")
GoodeR_Boundaries_Region=read_csv("GoodeR_Boundaries_Region.csv", guess_max = 600000)
say("Reading Training Points...")
TrainingPoints = read_csv("TrainingPoints_19_full.csv")
say("Reading Loss Mask...")
LossMaskFull= read_csv("LossMaskFull_20002016.csv", col_types = cols(Loss_10kMean_20002016 = col_number()))

# Optional:
# Look for GoodeR_SecondaryData in workspace.  Import or notify user it is missing.
if (TRUE %in% (list.files() == 'GoodeR_SecondaryData__.csv')) {
  say("Reading GoodeR_SecondaryData...")
  GoodeR_SecondaryData=read_csv("GoodeR_SecondaryData__.csv")
  GoodeR_SecondaryData=GoodeR_SecondaryData%>%
    filter(Loss_10kMean_20002016>0)
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

# These two objects are never used, so should be removed.
#
# LossMask1pcnt=LossMaskFull%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)%>%
#   filter(Loss_10kMean_20002016>=.01)
# 
# LossMask005=LossMaskFull%>%
#   select(GoodeR.ID,Loss_10kMean_20002016)%>%
#   filter(as.numeric(Loss_10kMean_20002016)>=.005)%>%
#   mutate(Loss_10kMean_20002016=replace(Loss_10kMean_20002016,,as.numeric(Loss_10kMean_20002016)))

#---------------
# Create Training points with data #
#---------------

# If TrainingPoints_PrimaryData doesn't exist (wasn't imported above), compute it
if (exists("TrainingPoints_PrimaryData") == FALSE) {
  
  say("Checking inputs for TrainingPoints_PrimaryData...")
  
  # import data into big list #
  FileList=as.data.frame(list.files(path = "./R_ModelInputs_SecondaryData",
                                    pattern = ".tif$", all.files = FALSE,
                                    full.names = FALSE, recursive = FALSE,
                                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
  names(FileList)=c("FileName")
  
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
  
  pause()
  
  say("Writing TrainingPoints_PrimaryData.csv to working directory...")
  write_csv(TrainingPoints_PrimaryData,"TrainingPoints_PrimaryData.csv")
}

#---------------
# Create full list of Secondary Data #
#---------------
## Mask by loss extent to make smaller ##

# If GoodeR_SecondaryData doesn't exist (wasn't imported above), compute it
if (exists("GoodeR_SecondaryData") == FALSE) {

  say("Calculating GoodeR_SecondaryData...")
  
  GoodeR_SecondaryData=1:6961896%>%
    as.vector()%>%
    as.data.frame()%>%
    rename("GoodeR.ID"=".")%>%
    inner_join(LossMaskFull,by="GoodeR.ID")%>%
    filter(Loss_10kMean_20002016>0)%>%
    select(-Loss_10kMean_20002016)
  temp=TrainingPoints_PrimaryData%>%
    select(GoodeR.ID,TrainingID)
  GoodeR_SecondaryData=GoodeR_SecondaryData%>%
    left_join(temp,by="GoodeR.ID")
  GoodeR_SecondaryData$TrainingID[is.na(GoodeR_SecondaryData$TrainingID)]=0
  
  # import data into big list #
  FileList=as.data.frame(list.files(path = "./R_ModelInputs_SecondaryData",
                                    pattern = ".tif$", all.files = FALSE,
                                    full.names = FALSE, recursive = FALSE,
                                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
  names(FileList)=c("FileName")
  
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
    GoodeRList=1:6961896%>%
      as.vector()%>%
      as.data.frame()%>%
      rename("GoodeR.ID"=".")%>%
      #look into different function
      bind_cols(data)
    GoodeR_SecondaryData=GoodeR_SecondaryData%>%
      left_join(GoodeRList,by="GoodeR.ID")
  }
  GoodeR_SecondaryData[is.na(GoodeR_SecondaryData)]=0
  GoodeR_SecondaryData=GoodeR_SecondaryData%>%
    left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")
  GoodeR_SecondaryData=GoodeR_SecondaryData%>%
    filter(!is.na(Region))
  
  say("Writing GoodeR_SecondaryData.csv to working directory...")
  write_csv(GoodeR_SecondaryData, "./GoodeR_SecondaryData.csv")
}
#---------------
# Old tool, do not run - This should be removed in a future version, after additional testing.
#---------------
# 
# temp=raster(paste("./R_ModelInputs_SecondaryData/Goode_PopulationDifference20002015_10kMax1kMean.tif"))
# data=temp%>%
#   as.vector()%>%
#   as.data.frame()
# names(data)=c("PopulationDifference20002015_10kMax1kMean")
# ## create R.ID list ##
# GoodeRList=1:6961896%>%
#   as.vector()%>%
#   as.data.frame()%>%
#   rename_("GoodeR.ID"=".")%>%
#   bind_cols(data)
# 
# TrainingPoints_PrimaryData=TrainingPoints_PrimaryData%>%
#   left_join(GoodeRList,by="GoodeR.ID")%>%
#   select(GoodeR.ID,TrainingID,
#          FireBrightness_80_10kMax_20002015,FireBrightness_80_10kMax1kMean_20002015,
#          FireBrightness_80_10kMax1kSum_20002015,FireBrightness_80_10kMean_20002015,
#          FireBrightness_80_10kMean1kMax_20002015,FireBrightness_80_10kMean1kSum_20002015,
#          FireBrightness_80_10kSum_20002015,FireCount_80_10kMax_20002015,
#          FireCount_80_10kMax1kMean_20002015,FireCount_80_10kMax1kSum_20002015,
#          FireCount_80_10kMean_20002015,FireCount_80_10kMean1kMax_20002015,
#          FireCount_80_10kMean1kSum_20002015,FireCount_80_10kSum_20002015,
#          FireFRP_80_10kMax_20002015,FireFRP_80_10kMax1kMean_20002015,
#          FireFRP_80_10kMax1kSum_230002015,FireFRP_80_10kMean_20002015,
#          FireFRP_80_10kMean1kMax_20002015,FireFRP_80_10kMean1kSum_20002015,
#          FireFRP_80_10kSum_20002015,FireLoss_10kMax_20002016,
#          FireLoss_10kMax1kMean_20002016,FireLoss_10kMax1kSum_20002016,
#          FireLoss_10kMean_20002016,FireLoss_10kMean1kMax_20002016,
#          FireLoss_10kMean1kSum_20002016,FireLoss_10kSum_20002016,
#          Gain_10kMax,Gain_10kMax1kMean,
#          Gain_10kMax1kSum,Gain_10kMean,
#          Gain_10kMean1kMax,Gain_10kMean1kSum,
#          Gain_10kSum,LandCover_DeciduousBroadleaf_3,
#          LandCover_EvergreenBroadleaf_2,LandCover_MixedOther_4,
#          LandCover_Needleleaf_1,Loss_10kMax_20002016,
#          Loss_10kMax1kMean_20002016,Loss_10kMax1kSum_20002016,
#          Loss_10kMean_20002016,Loss_10kMean1kMax_20002016,
#          Loss_10kMean1kSum_20002016,Loss_10kSum_20002016,
#          Loss_NetMean,LossYearDiff_10kMax_20002016,
#          LossYearDiff_10kMax1kMean_20002016,LossYearDiff_10kMax1kSum_20002016,
#          LossYearDiff_10kMean_20002016,LossYearDiff_10kMean1kMax_20002016,
#          LossYearDiff_10kMean1kSum_20002016,LossYearDiff_10kSum_20002016,
#          LossYearDiff1k_10kMax1kDiff_20002016,LossYearDiff1k_10kMean1kDiff_20002016,
#          LossYearDiff1k_10kSum1kDiff_20002016,Population2000_10kMax,
#          Population2000_10kMax1kMean,Population2000_10kMax1kSum,
#          Population2000_10kMean,Population2000_10kMean1kMax,
#          Population2000_10kMean1kSum,Population2000_10kSum,
#          Population2015_10kMax,Population2015_10kMax1kMean,
#          Population2015_10kMax1kSum,Population2015_10kMean,
#          Population2015_10kMean1kMax,Population2015_10kMean1kSum,
#          Population2015_10kSum,PopulationDifference20002015_10kMax,
#          PopulationDifference20002015_10kMax1kSum,PopulationDifference20002015_10kMean,
#          PopulationDifference20002015_10kMean1kMax,PopulationDifference20002015_10kMean1kSum,
#          PopulationDifference20002015_10kSum,PopulationDifference20002015_10kMax1kMean,TreeCover_10kMax,
#          TreeCover_10kMax1kMean,TreeCover_10kMax1kSum,TreeCover_10kMean,TreeCover_10kMean1kMax,TreeCover_10kMean1kSum,TreeCover_10kSum)
# 
# names(TrainingPoints_PrimaryData)
# 
# TrainingPoints_PrimaryData=TrainingPoints_PrimaryData%>%
#   select(-Region)

#write_csv(TrainingPoints_PrimaryData,"TrainingPoints_PrimaryData.csv")

#-----------------------
## Create Rpart Fits ##
#-----------------------

regions <- c(1, 2, 3, 4, 5, 6, 7)
drivers <- c(1, 2, 3, 4, 5)

#----------------------------------------------------------------------------------------
#Region 1
#----------------------------------------------------------------------------------------
say("Setting up Region 1")

TrainingPoints_PrimaryData__1=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==1)

#-----------------------
#Deforestation__1#
#-----------------------
say("Calculating Deforestation Tree...")

InputData=select(TrainingPoints_PrimaryData__1,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            # disable for all  but SE Asia?
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__1=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__1, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==1)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__1.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__1#
#-----------------------
say("Calculating Shifting Ag Tree...")

InputData=select(TrainingPoints_PrimaryData__1,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__1=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__1, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==1)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__1.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__1#
#-----------------------
say("Calculating Forestry Tree...")

InputData=select(TrainingPoints_PrimaryData__1,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__1=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__1, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==1)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__1.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__1#
#-----------------------
say("Calculating Wildfire Tree...")

InputData=select(TrainingPoints_PrimaryData__1,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__1=fit
#fancyRpartPlot(Fit_Wildfire__1)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__1, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==1)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__1.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__1#
#-----------------------
say("Calculating Urban Tree...")

InputData=select(TrainingPoints_PrimaryData__1,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__1=fit
#fancyRpartPlot(Fit_Urban__1)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__1, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==1)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__1.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Completing Region 1...")

ModelOutput.Final__1=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#Region__2
#----------------------------------------------------------------------------------------
say("Setting up Region 2...")
TrainingPoints_PrimaryData__2=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==2)

#-----------------------
#Deforestation__2#
#-----------------------
say("Calculating Deforestation Tree...")

InputData=select(TrainingPoints_PrimaryData__2,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__2=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__2, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==2)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__2.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__2#
#-----------------------
say("Calculating Shifting Ag Tree...")

InputData=select(TrainingPoints_PrimaryData__2,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__2=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__2, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==2)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__2.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__2#
#-----------------------
say("Calculating Forestry Tree...")

InputData=select(TrainingPoints_PrimaryData__2,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__2=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__2, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==2)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__2.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__2#
#-----------------------
say("Calculating Wildfire Tree...")

InputData=select(TrainingPoints_PrimaryData__2,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__2=fit
#fancyRpartPlot(Fit_Wildfire__2)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__2, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==2)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__2.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__2#
#-----------------------
say("Calculating Urban Tree...")

InputData=select(TrainingPoints_PrimaryData__2,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__2=fit
#fancyRpartPlot(Fit_Urban__2)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__2, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==2)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__2.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Completing Region 2...")
ModelOutput.Final__2=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------



#----------------------------------------------------------------------------------------
#Region__3
#----------------------------------------------------------------------------------------
say("Setting Up Region 3...")
TrainingPoints_PrimaryData__3=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==3)

#-----------------------
#Deforestation__3#
#-----------------------
say("Calculating Deforestation Tree...")

InputData=select(TrainingPoints_PrimaryData__3,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__3=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__3, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==3)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__3.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__3#
#-----------------------
say("Calculating Shifting Ag Tree...")
InputData=select(TrainingPoints_PrimaryData__3,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__3=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__3, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==3)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__3.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__3#
#-----------------------
say("Calculating Forestry Tree...")
InputData=select(TrainingPoints_PrimaryData__3,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__3=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__3, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==3)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__3.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__3#
#-----------------------
say("Calculating Wildfire Tree...")
InputData=select(TrainingPoints_PrimaryData__3,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__3=fit
#fancyRpartPlot(Fit_Wildfire__3)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__3, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==3)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__3.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__3#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__3,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__3=fit
#fancyRpartPlot(Fit_Urban__3)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__3, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==3)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__3.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Completing Region 3...")
ModelOutput.Final__3=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------


#----------------------------------------------------------------------------------------
#Region__4
#----------------------------------------------------------------------------------------
say("Setting up Region 4...")

TrainingPoints_PrimaryData__4=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==4)

#-----------------------
#Deforestation__4#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__4,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__4=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__4, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==4)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__4.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__4#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__4,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__4=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__4, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==4)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__4.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__4#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__4,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__4=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__4, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==4)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__4.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__4#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__4,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__4=fit
#fancyRpartPlot(Fit_Wildfire__4)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__4, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==4)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__4.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__4#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__4,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__4=fit
#fancyRpartPlot(Fit_Urban__4)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__4, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==4)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__4.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Finishing Region 4...")

ModelOutput.Final__4=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------

#----------------------------------------------------------------------------------------
#Region__5
#----------------------------------------------------------------------------------------
say("Setting up Region 5")

TrainingPoints_PrimaryData__5=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==5)

#-----------------------
#Deforestation__5#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__5,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__5=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__5, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==5)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__5.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__5#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__5,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__5=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__5, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==5)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__5.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__5#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__5,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__5=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__5, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==5)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__5.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__5#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__5,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__5=fit
#fancyRpartPlot(Fit_Wildfire__5)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__5, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==5)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__5.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__5#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__5,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__5=fit
#fancyRpartPlot(Fit_Urban__5)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__5, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==5)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__5.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Completing Region 5...")

ModelOutput.Final__5=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------

#----------------------------------------------------------------------------------------
#Region__6
#----------------------------------------------------------------------------------------
say("Setting up Region 6...")

TrainingPoints_PrimaryData__6=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==6)

#-----------------------
#Deforestation__6#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__6,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016+LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__6=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__6, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==6)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__6.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__6#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__6,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__6=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__6, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==6)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__6.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__6#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__6,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__6=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__6, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==6)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__6.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__6#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__6,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__6=fit
#fancyRpartPlot(Fit_Wildfire__6)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__6, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==6)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__6.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__6#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__6,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__6=fit
#fancyRpartPlot(Fit_Urban__6)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__6, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==6)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__6.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Completing Region 6...")

ModelOutput.Final__6=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------


#----------------------------------------------------------------------------------------
#Region__7
#----------------------------------------------------------------------------------------
say("Setting up Region 7...")

TrainingPoints_PrimaryData__7=TrainingPoints_PrimaryData%>%
  left_join(GoodeR_Boundaries_Region, by="GoodeR.ID")%>%
  mutate(Region=replace(Region,,as.numeric(Region)))%>%
  filter(Region==7)

#-----------------------
#Deforestation__7#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__7,Deforestation,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Deforestation~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Deforestation__7=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Deforestation=predict(Fit_Deforestation__7, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==7)%>%
  select(-Region)

ModelOutput.Final=ModelOutput

temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Deforestation__7.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Shifting.Agriculture__7#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__7,Shifting.Agriculture,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Shifting.Agriculture~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Shifting.Agriculture__7=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Shifting.Agriculture=predict(Fit_Shifting.Agriculture__7, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==7)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Shifting.Agriculture)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Shifting.Agriculture__7.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#TreeFarm.ForestryOther__7#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__7,TreeFarm.ForestryOther,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(TreeFarm.ForestryOther~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_TreeFarm.ForestryOther__7=fit
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_TreeFarm.ForestryOther=predict(Fit_TreeFarm.ForestryOther__7, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==7)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_TreeFarm.ForestryOther)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_TreeFarm.ForestryOther__7.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Wildfire__7#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__7,Wildfire,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Wildfire~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Wildfire__7=fit
#fancyRpartPlot(Fit_Wildfire__7)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Wildfire=predict(Fit_Wildfire__7, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==7)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Wildfire)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Wildfire)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Wildfire__7.tiff",type="GTIFF",overwrite=TRUE)

#---------------

#-----------------------
#Urban__7#
#-----------------------
InputData=select(TrainingPoints_PrimaryData__7,Urban,FireBrightness_80_10kMax_20002015:TreeCover_10kSum)
fit=rpart(Urban~
            FireBrightness_80_10kMax_20002015+ FireBrightness_80_10kMax1kMean_20002015+ FireBrightness_80_10kMax1kSum_20002015+ FireBrightness_80_10kMean_20002015+ FireBrightness_80_10kMean1kMax_20002015+ FireBrightness_80_10kMean1kSum_20002015+ FireBrightness_80_10kSum_20002015+     
            FireCount_80_10kMax_20002015+   FireCount_80_10kMax1kMean_20002015+ FireCount_80_10kMax1kSum_20002015+ FireCount_80_10kMean_20002015+ FireCount_80_10kMean1kMax_20002015+ FireCount_80_10kMean1kSum_20002015+ FireCount_80_10kSum_20002015+          
            FireFRP_80_10kMax_20002015+ FireFRP_80_10kMax1kMean_20002015+ FireFRP_80_10kMax1kSum_230002015+ FireFRP_80_10kMean_20002015+ FireFRP_80_10kMean1kMax_20002015+ FireFRP_80_10kMean1kSum_20002015+ FireFRP_80_10kSum_20002015+            
            FireLoss_10kMax_20002016+FireLoss_10kMax1kMean_20002016+FireLoss_10kMax1kSum_20002016+FireLoss_10kMean_20002016+FireLoss_10kMean1kMax_20002016+FireLoss_10kMean1kSum_20002016+FireLoss_10kSum_20002016+
            Gain_10kMax+Gain_10kMax1kMean+Gain_10kMax1kSum+Gain_10kMean+Gain_10kMean1kMax+Gain_10kMean1kSum+Gain_10kSum+
            LandCover_Needleleaf_1+
            LandCover_EvergreenBroadleaf_2+
            LandCover_DeciduousBroadleaf_3+
            LandCover_MixedOther_4+
            Loss_10kMax_20002016+Loss_10kMax1kMean_20002016+Loss_10kMax1kSum_20002016+Loss_10kMean_20002016+Loss_10kMean1kMax_20002016+Loss_10kMean1kSum_20002016+Loss_10kSum_20002016+
            Loss_NetMean+
            LossYearDiff_10kMax_20002016+LossYearDiff_10kMax1kMean_20002016+LossYearDiff_10kMax1kSum_20002016+LossYearDiff_10kMean_20002016++LossYearDiff_10kMean1kMax_20002016+LossYearDiff_10kMean1kSum_20002016+LossYearDiff_10kSum_20002016+
            LossYearDiff1k_10kMax1kDiff_20002016++LossYearDiff1k_10kMean1kDiff_20002016+LossYearDiff1k_10kSum1kDiff_20002016+
            Population2000_10kMax+Population2000_10kMax1kMean+Population2000_10kMax1kSum+Population2000_10kMean+Population2000_10kMean1kMax+Population2000_10kMean1kSum+Population2000_10kSum+
            Population2015_10kMax+Population2015_10kMax1kMean+Population2015_10kMax1kSum+Population2015_10kMean+Population2015_10kMean1kMax+Population2015_10kMean1kSum+Population2015_10kSum+
            PopulationDifference20002015_10kMax+PopulationDifference20002015_10kMax1kMean+PopulationDifference20002015_10kMax1kSum+PopulationDifference20002015_10kMean+PopulationDifference20002015_10kMean1kMax+PopulationDifference20002015_10kMean1kSum+
            TreeCover_10kMax+ TreeCover_10kMax1kMean+ TreeCover_10kMax1kSum+ TreeCover_10kMean+ TreeCover_10kMean1kMax+ TreeCover_10kMean1kSum+ TreeCover_10kSum,
          data=InputData, method="anova")
fit=prune(fit, cp=.02)
#fit=prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(fit)
Fit_Urban__7=fit
#fancyRpartPlot(Fit_Urban__7)
## Fit Trees to full GoodeR data ##
ModelOutput=GoodeR_SecondaryData%>%
  select(GoodeR.ID)%>%
  mutate(Output_Urban=predict(Fit_Urban__7, type="vector", newdata=GoodeR_SecondaryData))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  filter(Region==7)%>%
  select(-Region)

ModelOutput.Final=ModelOutput.Final%>%
  distinct()%>%
  left_join(ModelOutput,by="GoodeR.ID")

temp2=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  filter(Loss_10kMean_20002016>0)
temp=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Urban)%>%
  inner_join(temp2,by="GoodeR.ID")%>%
  select(GoodeR.ID,Output_Urban)
names(temp)=c("GoodeR.ID","data")
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(temp,by="GoodeR.ID")%>%
  select(GoodeR.ID,data)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Temp_RModelOutputsTiffs/TEST_Output_Urban__7.tiff",type="GTIFF",overwrite=TRUE)

#---------------
say("Completing Region 7...")

ModelOutput.Final__7=ModelOutput.Final%>%
  select(GoodeR.ID,Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)

#-------

#-----------------------
# Combine into final output list #
#-----------------------
say("Combining Regions...")

ModelOutput.Final_All=ModelOutput.Final__1%>%
  bind_rows(ModelOutput.Final__2)%>%
  bind_rows(ModelOutput.Final__3)%>%
  bind_rows(ModelOutput.Final__4)%>%
  bind_rows(ModelOutput.Final__5)%>%
  bind_rows(ModelOutput.Final__6)%>%
  bind_rows(ModelOutput.Final__7)

say("Writing raw model output to ModelOutput.Final_19.csv")

write_csv(ModelOutput.Final_All,"ModelOutput.Final_19.csv")
#------

#-----------------------
# mark decision trees and save - used for testing purposes.  Remove prior to release.
#-----------------------

# Fit_Deforestation__1_19=Fit_Deforestation__1
# Fit_Deforestation__1_19=Fit_Deforestation__1
# Fit_Deforestation__2_19=Fit_Deforestation__2
# Fit_Deforestation__3_19=Fit_Deforestation__3
# Fit_Deforestation__4_19=Fit_Deforestation__4
# Fit_Deforestation__5_19=Fit_Deforestation__5
# Fit_Deforestation__6_19=Fit_Deforestation__6
# Fit_Deforestation__7_19=Fit_Deforestation__7
# 
# Fit_Shifting.Agriculture__1_19=Fit_Shifting.Agriculture__1
# Fit_Shifting.Agriculture__1_19=Fit_Shifting.Agriculture__1
# Fit_Shifting.Agriculture__2_19=Fit_Shifting.Agriculture__2
# Fit_Shifting.Agriculture__3_19=Fit_Shifting.Agriculture__3
# Fit_Shifting.Agriculture__4_19=Fit_Shifting.Agriculture__4
# Fit_Shifting.Agriculture__5_19=Fit_Shifting.Agriculture__5
# Fit_Shifting.Agriculture__6_19=Fit_Shifting.Agriculture__6
# Fit_Shifting.Agriculture__7_19=Fit_Shifting.Agriculture__7
# 
# Fit_TreeFarm.ForestryOther__1_19=Fit_TreeFarm.ForestryOther__1
# Fit_TreeFarm.ForestryOther__1_19=Fit_TreeFarm.ForestryOther__1
# Fit_TreeFarm.ForestryOther__2_19=Fit_TreeFarm.ForestryOther__2
# Fit_TreeFarm.ForestryOther__3_19=Fit_TreeFarm.ForestryOther__3
# Fit_TreeFarm.ForestryOther__4_19=Fit_TreeFarm.ForestryOther__4
# Fit_TreeFarm.ForestryOther__5_19=Fit_TreeFarm.ForestryOther__5
# Fit_TreeFarm.ForestryOther__6_19=Fit_TreeFarm.ForestryOther__6
# Fit_TreeFarm.ForestryOther__7_19=Fit_TreeFarm.ForestryOther__7
# 
# Fit_Wildfire__1_19=Fit_Wildfire__1
# Fit_Wildfire__1_19=Fit_Wildfire__1
# Fit_Wildfire__2_19=Fit_Wildfire__2
# Fit_Wildfire__3_19=Fit_Wildfire__3
# Fit_Wildfire__4_19=Fit_Wildfire__4
# Fit_Wildfire__5_19=Fit_Wildfire__5
# Fit_Wildfire__6_19=Fit_Wildfire__6
# Fit_Wildfire__7_19=Fit_Wildfire__7
# 
# Fit_Urban__1_19=Fit_Urban__1
# Fit_Urban__1_19=Fit_Urban__1
# Fit_Urban__2_19=Fit_Urban__2
# Fit_Urban__3_19=Fit_Urban__3
# Fit_Urban__4_19=Fit_Urban__4
# Fit_Urban__5_19=Fit_Urban__5
# Fit_Urban__6_19=Fit_Urban__6
# Fit_Urban__7_19=Fit_Urban__7
#------

#---------------
# view decision tree Rplots
#---------------
say("Plotting Decision Trees...")

fancyRpartPlot(Fit_Deforestation__1, main = "Deforestation 1")
fancyRpartPlot(Fit_Deforestation__2, main = "Deforestation 2")
fancyRpartPlot(Fit_Deforestation__3, main = "Deforestation 3")
fancyRpartPlot(Fit_Deforestation__4, main = "Deforestation 4")
fancyRpartPlot(Fit_Deforestation__5, main = "Deforestation 5")
fancyRpartPlot(Fit_Deforestation__6, main = "Deforestation 6")
fancyRpartPlot(Fit_Deforestation__7, main = "Deforestation 7")

fancyRpartPlot(Fit_Shifting.Agriculture__1, main = "Shifting Ag 1")
fancyRpartPlot(Fit_Shifting.Agriculture__2, main = "Shifting Ag 2")
fancyRpartPlot(Fit_Shifting.Agriculture__3, main = "Shifting Ag 3")
fancyRpartPlot(Fit_Shifting.Agriculture__4, main = "Shifting Ag 4")
fancyRpartPlot(Fit_Shifting.Agriculture__5, main = "Shifting Ag 5")
fancyRpartPlot(Fit_Shifting.Agriculture__6, main = "Shifting Ag 6")
fancyRpartPlot(Fit_Shifting.Agriculture__7, main = "Shifting Ag 7")

fancyRpartPlot(Fit_TreeFarm.ForestryOther__1, main = "Forestry 1")
fancyRpartPlot(Fit_TreeFarm.ForestryOther__2, main = "Forestry 2")
fancyRpartPlot(Fit_TreeFarm.ForestryOther__3, main = "Forestry 3")
fancyRpartPlot(Fit_TreeFarm.ForestryOther__4, main = "Forestry 4")
fancyRpartPlot(Fit_TreeFarm.ForestryOther__5, main = "Forestry 5")
fancyRpartPlot(Fit_TreeFarm.ForestryOther__6, main = "Forestry 6")
fancyRpartPlot(Fit_TreeFarm.ForestryOther__7, main = "Forestry 7")

fancyRpartPlot(Fit_Wildfire__1, main = "Wildfire 1")
fancyRpartPlot(Fit_Wildfire__2, main = "Wildfire 2")
fancyRpartPlot(Fit_Wildfire__3, main = "Wildfire 3")
fancyRpartPlot(Fit_Wildfire__4, main = "Wildfire 4")
fancyRpartPlot(Fit_Wildfire__5, main = "Wildfire 5")
fancyRpartPlot(Fit_Wildfire__6, main = "Wildfire 6")
fancyRpartPlot(Fit_Wildfire__7, main = "Wildfire 7")

fancyRpartPlot(Fit_Urban__1, main = "Urban 1")
fancyRpartPlot(Fit_Urban__2, main = "Urban 2")
fancyRpartPlot(Fit_Urban__3, main = "Urban 3")
fancyRpartPlot(Fit_Urban__4, main = "Urban 4")
fancyRpartPlot(Fit_Urban__5, main = "Urban 5")
fancyRpartPlot(Fit_Urban__6, main = "Urban 6")
fancyRpartPlot(Fit_Urban__7, main = "Urban 7")

#-----

#---------------
#Create initial classification using model output
#---------------

#ModelOutput.Final=read_csv("ModelOutput.Final_19.csv")
say("Trees are voting on each pixel...")

temp=ModelOutput.Final%>%
  select(Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban)
test=as.data.frame(colnames(temp)[apply(temp,1,which.max)])
names(test)=c("MaxClass")

temp=temp%>%
  bind_cols(test)

MaxClass=temp%>%
  rowwise()%>%
  mutate(maxValue=max(Output_Deforestation,Output_Shifting.Agriculture,Output_TreeFarm.ForestryOther,Output_Wildfire,Output_Urban))
MaxClass=MaxClass%>%
  mutate(Class=ifelse(maxValue<.5,0,
                      ifelse(MaxClass=="Output_Deforestation",1,
                             ifelse(MaxClass=="Output_Shifting.Agriculture",2,
                                    ifelse(MaxClass=="Output_TreeFarm.ForestryOther",3,
                                           ifelse(MaxClass=="Output_Wildfire",4,
                                                  ifelse(MaxClass=="Output_Urban",5,0)
                                           ))))))
MaxClass_Final_19_50uncertain=ModelOutput.Final%>%
  select(GoodeR.ID)%>%
  bind_cols(MaxClass)%>%
  left_join(LossMaskFull,by = "GoodeR.ID")%>%
  mutate(Class2=ifelse(as.numeric(Loss_10kMean_20002016)<.005,0,Class))%>%
  select(-Class)%>%
  mutate(Class=Class2)%>%
  select(-Class2)%>%
  select(-Loss_10kMean_20002016)

say("Outputting initial class selections to MaxClass_Final_19_50uncertain.csv")

write_csv(MaxClass_Final_19_50uncertain,"MaxClass_Final_19_50uncertain.csv")
#----

#--------------------------------------------------------
# Initial raster 
#--------------------------------------------------------

#MaxClass_Final_19_50uncertain=read_csv("MaxClass_Final_19_50uncertain.csv")

MaxClass_Final=MaxClass_Final_19_50uncertain%>%
  select(GoodeR.ID,Class)
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(MaxClass_Final,by="GoodeR.ID")%>%
  select(GoodeR.ID,Class)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

say("Writing initial class selections to Goode_FinalClassification_19_50uncertain.tiff")

writeRaster(r,filename="Goode_FinalClassification_19_50uncertain.tiff",type="GTIFF",overwrite=TRUE)


#---------------
# prepare for expand in arcMap
#---------------

MaxClass_Final=MaxClass_Final_19_50uncertain%>%
  select(GoodeR.ID,Class)%>%
  filter(Class>0)
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(MaxClass_Final,by="GoodeR.ID")%>%
  select(GoodeR.ID,Class)
names(data)=c("GoodeR.ID","data")
data=distinct(data)
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
plot(r)

writeRaster(r,filename="Goode_FinalClassification_19_Excludeduncertain.tif",type="GTIFF",overwrite=TRUE)


#IMPORTANT
#---------------
say("The model is now paused.")
say("Run 'Expand Final Classification' model, found in 'Forestry Models 2.tbx'.")
say("This uses the Expand tool to classify uncertain pixels (model certainty < 50%) using nearest neighbor technique")
say("When finished with the Expand model, the output is the final classification.")
say("You may press Enter to continue calculating stats, or Ctrl+C, then Enter to quit.")
pause()

#-------------
# Calculate % of loss classified (non-mixed/uncertain) #
#-------------
temp=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kSum_20002016)
Total=sum(temp$Loss_10kSum_20002016)

LossClassified=temp%>%
  left_join(MaxClass_Final_19_50uncertain,by="GoodeR.ID")%>%
  filter(Class!=0)
LossClassified=sum(LossClassified$Loss_10kSum_20002016)/Total

LossUnClassified=temp%>%
  left_join(MaxClass_Final_19_50uncertain,by="GoodeR.ID")%>%
  filter(Class==0)
LossUnClassified=sum(LossUnClassified$Loss_10kSum_20002016)/Total

LossClassified
LossUnClassified
#-----


#-----------------------------------------
#Re-import Expanded classes (no Uncertain class) mask out areas with loss less than 0.5% loss
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

write_csv(Goode_FinalClassification19_Expand_05pcnt,"FinalClass_19_05pcnt.csv")

#-----
#--------------------------------------------------------
# Generate Loss Masks for Final classification (weight classification by loss)
#--------------------------------------------------------

#FinalClass_19 = read_csv("./FinalClass_19_05pcnt.csv", col_types = cols(Class.Final = col_integer()))
FinalClass_19 = Goode_FinalClassification19_Expand_05pcnt

#GoodeR_SecondaryData=read_csv("GoodeR_SecondaryData__.csv")

LossData=GoodeR_SecondaryData%>%
  select(GoodeR.ID,Loss_10kMean_20002016)%>%
  group_by(GoodeR.ID)%>%
  summarize(Loss_10kMean_20002016=mean(Loss_10kMean_20002016,na.rm=TRUE))%>%
  ungroup()%>%
  left_join(FinalClass_19,by="GoodeR.ID")

LossMask_19_Deforestation=LossData%>%
  filter(Class.Final==1)
write_csv(LossMask_19_Deforestation,"LossMask_19_Deforestation.csv")

LossMask_19_Shifting.Agriculture=LossData%>%
  filter(Class.Final==2)
write_csv(LossMask_19_Shifting.Agriculture,"LossMask_19_Shifting.Agriculture.csv")

LossMask_19_Forestry=LossData%>%
  filter(Class.Final==3)
write_csv(LossMask_19_Forestry,"LossMask_19_Forestry.csv")

LossMask_19_Wildfire=LossData%>%
  filter(Class.Final==4)
write_csv(LossMask_19_Wildfire,"LossMask_19_Wildfire.csv")

LossMask_19_Urban=LossData%>%
  filter(Class.Final==5)
write_csv(LossMask_19_Urban,"LossMask_19_Urban.csv")

LossMask_19_MinorLoss=LossData%>%
  filter(Class.Final==0)
write_csv(LossMask_19_MinorLoss,"LossMask_19_MinorLoss.csv")
#---

#---------------------
# Final Class _19 Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(FinalClass_19,by="GoodeR.ID")%>%
  select(GoodeR.ID,Class.Final)
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
plot(r)
writeRaster(r,filename="R_FinalOutputs/Goode_FinalClassification_19_05pcnt.tiff",type="GTIFF",overwrite=TRUE)


#---------------------
# LossMask_19_Deforestation Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(LossMask_19_Deforestation,by="GoodeR.ID")%>%
  select(GoodeR.ID,Loss_10kMean_20002016)
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
plot(r)
writeRaster(r,filename="Goode_LossMask_19_Deforestation_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
#----

#---------------------
# LossMask_19_Shifting.Agriculture Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(LossMask_19_Shifting.Agriculture,by="GoodeR.ID")%>%
  select(GoodeR.ID,Loss_10kMean_20002016)
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
plot(r)
writeRaster(r,filename="Goode_LossMask_19_Shifting.Agriculture_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
#----

#---------------------
# LossMask_19_Forestry Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(LossMask_19_Forestry,by="GoodeR.ID")%>%
  select(GoodeR.ID,Loss_10kMean_20002016)
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
plot(r)
writeRaster(r,filename="Goode_LossMask_19_Forestry_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
#----

#---------------------
# LossMask_19_Wildfire Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(LossMask_19_Wildfire,by="GoodeR.ID")%>%
  select(GoodeR.ID,Loss_10kMean_20002016)
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
plot(r)
writeRaster(r,filename="Goode_LossMask_19_Wildfire_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
#----

#---------------------
# LossMask_19_Urban Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(LossMask_19_Urban,by="GoodeR.ID")%>%
  select(GoodeR.ID,Loss_10kMean_20002016)
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
plot(r)
writeRaster(r,filename="Goode_LossMask_19_Urban_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
#----

#---------------------
# LossMask_19_MinorLoss Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(LossMask_19_MinorLoss,by="GoodeR.ID")%>%
  select(GoodeR.ID,Loss_10kMean_20002016)
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
plot(r)
writeRaster(r,filename="Goode_LossMask_19_MinorLoss_05pcnt.tiff",type="GTIFF",overwrite=TRUE)
#----

#--------------------------------------------------------
#separate output by region
#--------------------------------------------------------

data=raster("Goode_Boundaries_Region.tif")

## Select GRID data raster ##
data=data%>%
  as.vector()%>%
  as.data.frame()

names(data)=c("Region")
## create R.ID list ##
GoodeR_Boundaries_Region=1:6961896%>%
  as.vector()%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  bind_cols(data)
#write_csv(GoodeR_Boundaries_Region,"GoodeR_Boundaries_Region.csv")

FinalClass_19=read_csv("FinalClass_19_05pcnt.csv", col_types = cols(Class.Final = col_number()))

#---------------------
# add strata field #
#---------------------

MaxClass_Final=FinalClass_19%>%
  filter(!is.na(Class.Final))%>%
  left_join(GoodeR_Boundaries_Region,by="GoodeR.ID")%>%
  mutate(Strata=Class.Final*10+Region)

StrataCount=MaxClass_Final%>%
  mutate(Strata.Name=as.character(round(Strata,0)))%>%
  group_by(Strata.Name)%>%
  summarize(Count=n())%>%
  filter(Strata.Name!="1",Strata.Name!="2",Strata.Name!="3",Strata.Name!="4",Strata.Name!="5",Strata.Name!="6",Strata.Name!="7")
sum=sum(StrataCount$Count)

#---------------------
# experiment with sampling size (how many cells will be required?) #
#---------------------

StrataCount=StrataCount%>%
  ungroup()%>%
  mutate(Count.pcnt=Count/sum)%>%
  mutate(num=round(Count.pcnt*750,0))%>%
  mutate(num30=ifelse(Count<30,Count,ifelse(num<30,30,num)))%>%
  mutate(num40=ifelse(Count<40,Count,ifelse(num<40,40,num)))%>%
  mutate(num50=ifelse(Count<50,Count,ifelse(num<50,50,num)))

num=sum(StrataCount$num)
num30=sum(StrataCount$num30)
num40=sum(StrataCount$num40)
num50=sum(StrataCount$num50)

temp=MaxClass_Final%>%
  select(-GoodeR.ID)%>%
  distinct()%>%
  mutate(Strata.Name=as.character(Strata))

StrataCount=StrataCount%>%
  left_join(temp,by="Strata.Name")%>%
  select(Strata.Name,Region,Class.Final,num40,Count)


write_csv(StrataCount,"StrataCount_19.csv")

#---------------------
# Strata Raster export
#---------------------
data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(MaxClass_Final,by="GoodeR.ID")%>%
  select(GoodeR.ID,Strata)%>%
  distinct()
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
plot(r)
writeRaster(r,filename="Goode_Strata_All_19.tiff",type="GTIFF",overwrite=TRUE)
#----


#---------------------
# Strata Sampling by region
#---------------------
temp=StrataCount
sum(temp$num40)

StrataName=StrataCount%>%
  select(Strata.Name)

Strata.Sample.Final=data.frame()

for(Strata.ID in StrataName$Strata.Name){
  
  Num=StrataCount%>%
    filter(Strata.Name==Strata.ID)%>%
    inner_join(StrataName,by="Strata.Name")%>%
    select(num40)
  Num=as.numeric(Num)
  
  Strata=MaxClass_Final%>%
    filter(Strata==Strata.ID)%>%
    select(GoodeR.ID)
  Strata=as.vector(Strata)
  
  Sample=sample_n(Strata,size=Num,replace=FALSE)%>%
    mutate(Strata.Name=Strata.ID)
  
  Strata.Sample.Final=Strata.Sample.Final%>%
    bind_rows(Sample)
}


ROWNAME=as.data.frame(c(1:1537))
names(ROWNAME)=c("Sample.ID")

Strata.Sample.Final=Strata.Sample.Final%>%
  bind_cols(ROWNAME)


write_csv(Strata.Sample.Final,"Strata.Sample.Final_19_05pcnt.csv")

# Strata Sample cells Raster export

data=1:6961896%>%
  as.data.frame()%>%
  rename("GoodeR.ID"=".")%>%
  left_join(Strata.Sample.Final,by="GoodeR.ID")%>%
  select(GoodeR.ID,Sample.ID)
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
plot(r)
writeRaster(r,filename="Goode_Strata_SampleCells_StrataSampleID_19_05pcnt.tiff",type="GTIFF",overwrite=TRUE)

# In ArcMap, create shapefile, sort by Sample.ID

#---------------------
# Create KMLs
#---------------------

polygon <- readOGR(".", "StrataSampleID_Cells_19_05pcnt_sort")
crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  

polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(polygon2, dsn="SampleID_RegionStrata_19_1503_sort.kml", layer="polygon", driver="KML",overwrite=TRUE)

#----
# Create KML

polygon <- readOGR(".", "SampleCell_Loss250m_RegionStrata_19_05pcnt")
crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  

polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(polygon2, dsn="SampleCell_Loss250m_RegionStrata_19_05pcnt.kml", layer="polygon", driver="KML")

#----
# Create Training KML

polygon <- readOGR(".", "TrainingCell_Loss250m_19")
crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  

polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(polygon2, dsn="TrainingCellsClass17.kml", layer="polygon", driver="KML", overwrite=TRUE)

polygon <- readOGR(".", "TrainingCellsClass_19")
crs(polygon)= "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"  

polygon2=spTransform(polygon,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
writeOGR(polygon2, dsn="TrainingCellsClass_19.kml", layer="polygon", driver="KML", overwrite=TRUE)