# WILDOCEANS SHARK & RAY CONSERVATION PLAN 

## Project description
* This repository contains all the necessary code used to develop the shark and ray conservation plan developed for WILDOCEANS
* This is currently a LIVE project, so the code may still change
* A report of this project is available by contacting ninab@wildtrust.co.za OR jennifer@wildtrust.co.za. The report contains a list of all data collaborators.
* A paper is being written for publication and will be referenced here when applicable.

## Description of each folder contents
* There are three folders containing scripts in this repository, as well as additional folders which contain data and layers necessary to run the scripts.
* The *Outputs* folder will contain all the outputs from the modelling and planning code (some outputs will remain confidential until publication of the manuscript
* See below for a detailed description of the contents of each folder

### 1. scripts_occurencedata folder
* Initial data cleaning and exploration (raw occurrence data are confidential) 
* `merging_datasets.R` is the script used to combine data from the various datasets and output one file per species. These output files are used as input in the modelling scripts.

### 2. scripts_modelling folder
* Producing the species distribution models (SDMs). This folder itself contains three subfolders
* All the distribution models produced are availabe as .JPG or as .tif files in the **outputs** folder

- **scripts_envtvariablprep folder**
  + Preparation and processing of environmental variable rasters
  + Outputs are raster files of environmental variables at 10x10km resolution which are available in the main WILDOCEANS-scripts repository in folders  *ALLLAYERS10kmresolution*
  
- **scripts_modelling folder**
  + Running the species distribution models
  + To run, start with `mainscript-modelling.R` as this is the parent script through which the other scripts are run. This script contains the orders in which to run the scripts and also describes what each one is for.  
  
**!!IMPORTANT 1!! how to run scripts on your own data or example data provided**:  
* The occurrence data used to run the conservation plan is confidential and not available for download
* If you wish to run the code there is an **example.csv** file provided which allows for the modelling scripts to be run on freely available data from GBIF and OBIS for *Acroteriobatus annulatus*. Steps to use the example.csv file are as follows:  
  + make sure exampledata = "yes" in `mainscript-modelling.R` - this ensures that `species_data.R` runs on the example data
  + the script is built to run on a loop going through each species and calling all subscripts in order, however this is not useful if wanting to learn what each script does, so i suggest manullay setting i=1 and running through each line and subscript individually rather than running the whole loop
* If you wish to run the code on your own data, ensure your data has the same headings as the example.csv file and simply replace example_data.csv with your filename.

**!!IMPORTANT 2!! installing the mecofun package to run the variable selection procedure**:  
Currently, all models are run on the same subset of environmental variables detailed in *selectedvariables_all.csv*. These were chosen by testing for collinearity using the `independentvariableselection.R`. If you wish to test for collinearity at the actual presence points of your data, you can use `variableselection.R`. In this case the `mecofun` package needs to be installed as follows:  
`library(devtools)`  
`devtools::install_git("https://gitup.uni-potsdam.de/macroecology/mecofun.git")`

**IMPORTANT 3: installing java may be required**:  
To run the modelling scripts as is, java is required to run MaxEnt as one of the modelling algorithms. If you do not wish to install java, then it may be that maxent will not be able to run.

### 3. scripts_planning folder
This folder contains the code to run the spatial planning algorithm *prioritizr*. The *prioritizr* package has an [online tutorial](https://prioritizr.net/articles/prioritizr.html) and [github page](https://github.com/prioritizr/prioritizr).  
Similar to the modelling scripts, start with`mainscript-planning.R` as this is the parent script through which the other scripts are run. All the input species distributon files to run the plannign script are available in the **Outputs** folder

### 4. IUCN folder
Contains range maps for several shark and ray species in South Africa. This script can be used to run the spatial planning scripts instead of using the modelled distributions.

### 5. ALLLAYERS 5 & 10 kmresolution folder
This folder contains the environmental rasters to run the distribution models at 5 or 10 km resolution. A reference to these data is available in the project report.

### 6. plotting_features folder
This folder contains the layers necessary to produce plots with outputs from the modelling and planning scripts. The modelling and planning scripts produce rasters which can be plotted as annotated maps using the features from plotting parameters.


