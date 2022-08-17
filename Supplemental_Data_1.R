#---------------
# Settings #
#---------------
# set to true if you want the model to output all the decision trees
rplots <- TRUE

# set a repo to use for installing dependencies
# (repo list: https://cran.r-project.org/mirrors.html)
repo <- "http://cran.us.r-project.org"


#---------------
# Load packages #
#---------------
# list dependencies
package_list <- c("rgdal", "raster", "dplyr", "foreign", "tidyr", "readr",
                 "stringr", "rpart", "rpart.plot", "rattle", "crayon")
# if packages in package.list aren't installed, put their name in new.packages
new_packages <- package_list[
  !(package_list %in% installed.packages()[, "Package"])]
# if new.packages has items in it, install them
if (length(new.packages)) install.packages(new_packages, repos = repo)
# Load the packages
lapply(package_list, library, character.only = TRUE)


#---------------
# Define functions
#---------------
pause <- function() {
  if (interactive()) {
    line <- invisible(readline(prompt = "Press [enter] to continue..."))
  } else {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
    close(file("stdin"))
  }
}

# writes 'words' to terminal in green letters and makes a new line
say <- function(words) {
  cat(words, "\n")
}

# writes 'words' to the terminal in red letters and makes a new line
bad <- function(words) {
  cat(words, "\n")
}

# df is a dataframe with minimum of GoodeR.ID and 1 data column,
# filename and column are char strings
rast_out <- function(df, filename, column) {
  data <- 1:695959816 %>%
    as.data.frame() %>%
    rename("GoodeR.ID" = ".") %>%
    left_join(df, by = "GoodeR.ID") %>%
    select(GoodeR.ID, column)
  names(data) <- c("GoodeR.ID", "data")
  list <- data %>%
    select(data)
  list <- as.vector(t(list))
  m <- matrix(data = list, nrow = 40076, ncol = 17366,
             byrow = FALSE, dimnames = NULL)
  m <- (t(m))
  r <- raster(m)
  xmin(r) <- -20037506.5671
  xmax(r) <- 20038493.4329
  ymin(r) <- -8683205.0209
  ymax(r) <- 8682794.9791
  crs(r) <-
    "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"
  writeRaster(r, filename = filename, type = "GTIFF", overwrite = TRUE)
}

#---------------
# Set up workspace #
#---------------
sub_dir1 <- "Temp_RModelOutputsTiffs"
sub_dir2 <- "R_FinalOutputs"
temp_dir1 <- file.path(getwd(), sub_dir1)
temp_dir2 <- file.path(getwd(), sub_dir2)

# create output directories if they don't already exist
if (!dir.exists(temp_dir1)) {
  say("")
  say("Creating temp directory...")
  say("")
  dir.create(temp_dir1)
} else {
  say("")
  say("Temp directory already exists, so won't be created.")
  say("")
}

if (!dir.exists(temp_dir2)) {
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
goode_r_boundaries_region <- read_csv("GoodeR_Boundaries_Region.csv",
  col_types = cols(GoodeR.ID = col_integer(), Region = col_integer()))
say("Reading Training Points...")
training_points <- read_csv("TrainingPointsFull.csv",
  col_types = cols(GoodeR.ID = col_integer(), Training.Class = col_integer()))
say("Reading Loss Mask...")
loss_mask_full <- read_csv("LossMaskFull.csv",
  col_types = cols(col_integer(), col_number()))
goode_r <- 1:695959816 %>%
  as.vector() %>%
  as.data.frame() %>%
  rename("GoodeR.ID" = ".")

# Optional:
# Look for GoodeR_SecondaryData in workspace.
# Import or notify user it is missing.
if (TRUE %in% (list.files() == "GoodeR_SecondaryData.csv")) {
  say("Reading GoodeR_SecondaryData...")
  goode_r_secondary_data <- read_csv("GoodeR_SecondaryData.csv")
  goode_r_secondary_data <- goode_r_secondary_data %>%
    filter(goode_r_secondary_data[41] > 0)
} else {
  say("Secondary Data not found in workspace.  It will be calculated.")
}

# Look for TrainingPoints_PrimaryData in workspace.
# Import or notify user it is missing.
if (TRUE %in% (list.files() == "TrainingPoints_PrimaryData.csv")) {
  say("Reading Training Points...")
  training_points_primary_data <- read_csv("TrainingPoints_PrimaryData.csv")
} else {
  say(paste("Training Points Primary Data not found in workspace. ",
            "It will be calculated.", sep = ""))
}

# Get list of primary data files
primary_file_list <- as.data.frame(
  list.files(path = "./R_ModelInputs_PrimaryData",
             pattern = ".tif$", all.files = FALSE,
             full.names = FALSE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
names(primary_file_list) <- c("FileName")
primary_variables <- primary_file_list
names(primary_variables) <- c("Name")
# Strip file prefixes and suffixes
primary_variables$Name <- primary_variables$Name %>%
  str_replace("Goode_", "") %>%
  str_replace(".tif", "")

# Get list of secondary data files
secondary_file_list <- as.data.frame(
  list.files(path = "./R_ModelInputs_SecondaryData",
             pattern = ".tif$", all.files = FALSE,
             full.names = FALSE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE,
             no.. = FALSE))
names(secondary_file_list) <- c("FileName")
secondary_variables <- secondary_file_list
names(secondary_variables) <- c("Name")
# Strip file prefix and suffix
secondary_variables$Name <- secondary_variables$Name %>%
  str_replace("Goode_", "") %>%
  str_replace(".tif", "")

#---------------
# Create Training points with data #
#---------------

say("Checking inputs for TrainingPoints_PrimaryData...")

for (NAME in primary_file_list$FileName){
  data <- raster(paste("./R_ModelInputs_PrimaryData/", NAME, sep = ""))

  if (nrow(data) != 17366) {
    bad(c(NAME, "has a problem"))
  } else {
    if (ncol(data) != 40076) {
      bad(c(NAME, "has a problem"))
    } else {
      say(c(NAME, "looks good"))
    }
  }
}

say(paste("If all inputs look good, press Enter, if any failed,",
    " press Ctrl+C then Enter to quit and fix them.", sep = ""))
pause()

say("Calculating Training Points")
training_ids <- as.data.frame(c(1:nrow(training_points)))
names(training_ids) <- c("TrainingID")
# Make columns for each driver (1 is that class, 0 is not that class)
training_points <- training_points %>%
  bind_cols(training_ids) %>%
  mutate(Deforestation = ifelse(Training.Class == 1, 1, 0)) %>%
  mutate(Shifting.Agriculture = ifelse(Training.Class == 2, 1, 0)) %>%
  mutate(TreeFarm.ForestryOther = ifelse(Training.Class == 3, 1, 0)) %>%
  mutate(Wildfire = ifelse(Training.Class == 4, 1, 0)) %>%
  mutate(Urban = ifelse(Training.Class == 5, 1, 0))

# this isn't filtering out class 6 (flooding?)
training_points_primary_data <- training_points %>%
  filter(Training.Class != 7)
#activate this
#write_csv(TrainingPoints,"TrainingPoints19.csv")

for (name_1 in primary_file_list$FileName){
  say(c("Reading", name_1))
  data <- raster(paste("./R_ModelInputs_PrimaryData/", name_1, sep = ""))
  name_2 <- name_1 %>%
    str_replace("^Goode_", "") %>%
    str_replace(".tif$", "")
  ## Select GRID data raster ##
  data <- data %>%
    as.vector() %>%
    as.data.frame()
  names(data) <- c(paste(name_2))
  ## create R.ID list ##
  goode_r_list <- 1:695959816 %>%
    as.vector() %>%
    as.data.frame() %>%
    rename("GoodeR.ID" = ".") %>%
    bind_cols(data)
  training_points_primary_data <- training_points_primary_data %>%
    left_join(goode_r_list, by = "GoodeR.ID")
}
training_points_primary_data[is.na(training_points_primary_data)] <- 0
training_points_primary_data <- training_points_primary_data %>%
  distinct()

#---------------
# Create full list of Secondary Data #
#---------------
## Mask by loss extent to make smaller ##

say("Calculating GoodeR_SecondaryData...")

goode_r_secondary_data <- loss_mask_full %>%
  filter(loss_mask_full[2] > 0) %>%
  select(GoodeR.ID)
tppd <- training_points_primary_data %>%
  select(GoodeR.ID, TrainingID)
goode_r_secondary_data <- goode_r_secondary_data %>%
  left_join(tppd, by = "GoodeR.ID")
goode_r_secondary_data$TrainingID[is.na(goode_r_secondary_data$TrainingID)] <- 0

for (name_1 in secondary_file_list$FileName){

  data <- raster(paste("./R_ModelInputs_SecondaryData/", name_1, sep = ""))

  say(c("Reading ", name_1))

  name_2 <- name_1 %>%
    str_replace("^Goode_", "") %>%
    str_replace(".tif$", "")

  ## Select GRID data raster ##
  data <- data %>%
    as.vector() %>%
    as.data.frame()
  names(data) <- c(paste(name_2))
  ## create R.ID list ##
  goode_r_list <- goode_r %>%
    bind_cols(data)
  goode_r_secondary_data <- goode_r_secondary_data %>%
    left_join(goode_r_list, by = "GoodeR.ID")
}
goode_r_secondary_data[is.na(goode_r_secondary_data)] <- 0
goode_r_secondary_data <- goode_r_secondary_data %>%
  left_join(goode_r_boundaries_region, by = "GoodeR.ID")
goode_r_secondary_data <- goode_r_secondary_data %>%
   filter(!is.na(Region))

#----

#-----------------------
## Create Rpart Fits ##
#-----------------------

regions <- c(1, 2, 3, 4, 5, 6, 7)
driver_names <- c("Deforestation",
                 "Shifting.Agriculture",
                 "TreeFarm.ForestryOther",
                 "Wildfire",
                 "Urban")
drivers <- c(1, 2, 3, 4, 5)
model_output <- list()
model_output_regional <- list()
if (rplots) {
  pdf(paste(sub_dir2, "DecisionTrees.pdf", sep = "/"))
}
importance <- data.frame(
  matrix(vector(),
  ncol = length(primary_variables$Name)))
names(importance) <- primary_variables$Name
#regionalOutputs <- list()


for (region in regions){
  say("")
  say("********************")
  say(c("* Setting up Region", region))
  say("********************")

  # extract only training points within the current region from the full set
  training_points_regional <- training_points_primary_data %>%
    left_join(goode_r_boundaries_region, by = "GoodeR.ID") %>%
    mutate(Region = replace(Region, , as.numeric(Region))) %>%
    filter(Region == c(region))

  for (driver in drivers){
    say(c("Calculating", driver_names[driver], "Tree..."))

    # select the column for the current driver, then all variable columns
    num_variables <- length(primary_variables$Name)
    input_data <- select(training_points_regional,
                        driver_names[driver],
                        primary_variables$Name[1]:primary_variables$Name[num_variables])

    # give rpart a formula where the current driver is defined
    # as the sum of all variables in Variables$Name,
    # pass InputData and define method
    fit <- rpart(as.formula(paste(paste(driver_names[driver], "~", sep = ""),
      paste(primary_variables$Name, collapse = "+"))),
      data = input_data,
      method = "anova")
    fit <- prune(fit, cp = .02)

    # plot the tree and give it a title of 'Region: Driver'
    if (rplots) {
      name <- paste("Region", region, ":", driver_names[driver], sep = " ")
      fancyRpartPlot(fit, main = name)
      importance <- bind_rows(importance, fit$variable.importance)
    }
    #cat(name)

    # generate name for model output field
    out_name <- paste("Output", driver_names[driver], sep = "_")

    # calculate probability of current driver for whole dataset
    # Give tree full dataset and record probability for each (to be
    # used later in regional voting)
    model_output[[driver]] <- goode_r_secondary_data %>%
      select(GoodeR.ID) %>%
      left_join(goode_r_boundaries_region, by = "GoodeR.ID") %>%
      # the "!! outName :=" uses the variable value as the new
      # column name, rather than the literal string 'outName'
      mutate(!!out_name := predict(fit,
                                   type = "vector",
                                   newdata = goode_r_secondary_data)) %>%
      filter(Region == paste(region)) %>%
      select(-Region)
  }
  # Collect class probabilities from each tree in ModelOutput.Full
  model_output_regional[[region]] <- as.data.frame(
    Reduce(function(x, y) {
      merge(x, y, by = "GoodeR.ID", all.x = TRUE)
    }, model_output))

  say("********************")
  say(c("* Region", region, "Complete!"))
  say("********************")
}

# Close and write pdf to file
if (rplots) {
  dev.off()
}

# Combine all regions into the final table
model_output_all <- Reduce(function(x, y) {
  bind_rows(x, y)}, model_output_regional) %>%
  left_join(goode_r_boundaries_region, by = "GoodeR.ID")

# combine scores for variable importance and write to file
total_importance <- summarise_each(importance, list(~ sum(., na.rm = TRUE)))
say("Writing variable importance")
write_csv(total_importance,
          paste(sub_dir2, "Variable.Importance.csv", sep = "/"))


#---------------
#Create initial classification using model output
#---------------

#ModelOutput.Final=read_csv("ModelOutput.Final_19.csv")
say("Trees are voting on each pixel...")

# make column names machine readable
temp <- model_output_all %>%
  select(Output_Deforestation,
         Output_Shifting.Agriculture,
         Output_TreeFarm.ForestryOther,
         Output_Wildfire,
         Output_Urban) %>%
  rename("out1" = "Output_Deforestation",
         "out2" = "Output_Shifting.Agriculture",
         "out3" = "Output_TreeFarm.ForestryOther",
         "out4" = "Output_Wildfire",
         "out5" = "Output_Urban")
# get max value for cell
test <- as.data.frame(colnames(temp)[apply(temp, 1, which.max)])
names(test) <- c("MaxClass")
# reattach assigned drivers to full output list
temp <- temp %>%
  bind_cols(test)
# Add column for the confidence of the top driver
max_class <- temp %>%
  rowwise() %>%
  mutate(maxValue = max(out1, out2, out3, out4, out5))
# drop 'out' from $MaxClass column, leaving just numbers
max_class$MaxClass <- gsub("out", "", max_class$MaxClass)
# eliminate any cells whose top score was <50% by setting their value to 0
max_class <- within(max_class, max_class[maxValue < 0.5] <- 0) %>%
  rename("Class" = "MaxClass")
# convert columns to numeric type
max_class <- transform(max_class, Class = as.numeric(Class))

model_output_all <- model_output_all %>%
  bind_cols(max_class) %>%
  select(-out1, -out2, -out3, -out4, -out5)

by_region <- model_output_all %>% group_by(Region)

write_csv(model_output_all, paste(sub_dir2, "ModelOutput.All.csv", sep = "/"))

# MaxClass=MaxClass%>%
#   mutate(Class=ifelse(maxValue<.5,0,
#                       ifelse(MaxClass=="Output_Deforestation",1,
#                              ifelse(MaxClass=="Output_Shifting.Agriculture",2,
#                                     ifelse(MaxClass=="Output_TreeFarm.ForestryOther",3,
#                                            ifelse(MaxClass=="Output_Wildfire",4,
#                                                   ifelse(MaxClass=="Output_Urban",5,0)
#                                            ))))))
maxclass_final_19_50_uncertain <- model_output_all %>%
  select(GoodeR.ID) %>%
  bind_cols(max_class) %>%
  left_join(loss_mask_full, by = "GoodeR.ID") %>%
  mutate(Class2 = ifelse(as.numeric(Loss) < .005, 0, Class)) %>%
  select(-Class) %>%
  mutate(Class = Class2) %>%
  select(-Class2) %>%
  select(-Loss)

# Disabling  these because it just creates the possibility of contamination
# from old data.  If the model is being run again, it's worth recalculating
# the secondary data, because presumably, something has changed.

# say("Outputting initial class selections to MaxClass_Final_19_50uncertain.csv")

# write_csv(MaxClass_Final_19_50uncertain,"MaxClass_Final_19_50uncertain.csv")
#----

#--------------------------------------------------------
# Initial raster
#--------------------------------------------------------

max_class_final <- maxclass_final_19_50_uncertain %>%
  select(GoodeR.ID, Class)


say(paste("Writing initial class selections to ",
  "Goode_FinalClassification_19_50uncertain.tiff", sep = ""))

rast_out(max_class_final,
         "Goode_FinalClassification_19_50uncertain.tiff",
         "Class")


#---------------
# prepare for expand in arcMap
#---------------

max_class_final <- max_class_final %>%
  filter(Class > 0)

rast_out(max_class_final,
         "Goode_FinalClassification_19_Excludeduncertain.tif",
         "Class")



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
temp <- goode_r_secondary_data %>%
  select(!!c(1, 41))
total <- sum(temp[2])

loss_classified <- temp %>%
  left_join(maxclass_final_19_50_uncertain, by = "GoodeR.ID") %>%
  filter(Class != 0)
loss_classified <- sum(loss_classified[2]) / total

loss_unclassified <- temp %>%
  left_join(maxclass_final_19_50_uncertain, by = "GoodeR.ID") %>%
  filter(Class == 0)
loss_unclassified <- sum(loss_unclassified[2]) / total

say("")
say(cat("Loss classified:", loss_classified, "%"))
say(cat("Loss unclassified:", loss_unclassified, "%"))
say("")
#-----

# This step is already done when creating the initial classification raster.
# Not necessary, remove after testing.
#-----------------------------------------
# Re-import Expanded classes (no Uncertain class) mask out areas with
# loss less than 0.5% loss
#-----------------------------------------

data <- raster(paste("R_FinalOutputs/",
  "Goode_FinalClassification_19_50uncertain_expanded_05pcnt.tif", sep = ""))
data <- as.vector(data) %>%
  as.data.frame()
names(data) <- c("Class")
# create R.ID list ##
raster_list <- 1:695959816 %>%
  as.vector() %>%
  as.data.frame()
names(raster_list) <- c("GoodeR.ID")
temp <- raster_list %>%
  bind_cols(data) %>%
  inner_join(loss_mask_full, by = "GoodeR.ID") %>%
  loss_numeric <- as.numeric(Loss_10kMean_20002016)
  mutate(class_2 <- ifelse(
      loss_numeric > 0 & loss_numeric < .005, 0, Class)) %>%
  mutate(class_final <- round(Class2, 0)) %>%
  select(-Class, Class2, -Loss_10kMean_20002016)

Goode_FinalClassification19_Expand_05pcnt <- temp
writeRaster(Goode_FinalClassification19_Expand_05pcnt,
            filename = "R_FinalOutputs/TestMask.tif",
            type = "GTIFF",
            overwrite = TRUE)

write_csv(Goode_FinalClassification19_Expand_05pcnt, "FinalClass_19_05pcnt.csv")
#-----

#--------------------------------------------------------
# Generate Loss Masks for Final classification (weight classification by loss)
#--------------------------------------------------------

say("Generating loss masks for each class")
#FinalClass_19 = read_csv("./FinalClass_19_05pcnt.csv", col_types = cols(Class.Final = col_integer()))
final_class_19 <- Goode_FinalClassification19_Expand_05pcnt

#GoodeR_SecondaryData=read_csv("GoodeR_SecondaryData.csv")

loss_data <- goode_r_secondary_data %>%
  select(GoodeR.ID, Loss_10kMean_20002016) %>%
  group_by(GoodeR.ID) %>%
  summarize(Loss_10kMean_20002016 =
    mean(Loss_10kMean_20002016, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(final_class_19, by = "GoodeR.ID")

loss_mask_19_deforestation <- loss_data %>%
  filter(Class.Final == 1)
write_csv(loss_mask_19_deforestation, "LossMask_19_Deforestation.csv")
rast_out(loss_mask_19_deforestation,
         "LossMask_19_Deforestation.tif",
         Loss_10kMean_20002016)

loss_mask_19_shifting_ag <- loss_data %>%
  filter(Class.Final == 2)
write_csv(loss_mask_19_shifting_ag,
          "LossMask_19_Shifting.Agriculture.csv")
rast_out(loss_mask_19_shifting_ag,
         "LossMask_19_Shifting.Agriculture.tif",
         Loss_10kMean_20002016)

loss_mask_19_forestry <- loss_data %>%
  filter(Class.Final == 3)
write_csv(loss_mask_19_forestry,
          "LossMask_19_Forestry.csv")
rast_out(loss_mask_19_forestry,
         "LossMask_19_Forestry.tif",
         Loss_10kMean_20002016)

loss_mask_19_wildfire <- loss_data %>%
  filter(Class.Final == 4)
write_csv(loss_mask_19_wildfire,
          "LossMask_19_Wildfire.csv")
rast_out(loss_mask_19_wildfire,
         "LossMask_19_Wildfire.tif",
         Loss_10kMean_20002016)

loss_mask_19_urban <- loss_data %>%
  filter(Class.Final == 5)
write_csv(loss_mask_19_urban,
          "LossMask_19_Urban.csv")
rast_out(loss_mask_19_urban,
         "LossMask_19_Urban.tif",
         Loss_10kMean_20002016)

loss_mask_19_minor_loss <- loss_data %>%
  filter(Class.Final == 0)
write_csv(loss_mask_19_minor_loss,
          "LossMask_19_MinorLoss.csv")
rast_out(loss_mask_19_minor_loss,
         "LossMask_19_MinorLoss.tif",
         Loss_10kMean_20002016)
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