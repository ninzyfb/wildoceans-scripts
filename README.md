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
* Producing the species distribution models (SDMs)
* To run, start with `mainscript-modelling.R` as this is the parent script through which the other scripts are run. This script contains the orders in which to run the scripts and also describes what each one is for.
* All models were run on the same subset of environmental variables detailed in *selectedvariables_all.csv*. These were chosen by testing for collinearity using the `independentvariableselection.R` script and are available in the folder  *ALLLAYERS10kmresolution*
* All the distribution models produced are availabe as .tif files in the **outputs** folder or as .JPG from a [dropbox folder](https://www.dropbox.com/sh/ranc3035p3jn5hv/AADWr_6ef47qrpfR1HwVUCNpa?dl=0)
  
**!!IMPORTANT 1!! how to run scripts on your own data or example data provided**:  
* The occurrence data used to run the conservation plan is confidential and not available for download
* If you wish to run the code there is an **example.csv** file provided which allows for the modelling scripts to be run on freely available data from GBIF and OBIS for *Acroteriobatus annulatus*. Steps to use the example.csv file are as follows:  
  + make sure exampledata = "yes" in `mainscript-modelling.R` - this ensures that `species_data.R` runs on the example data
  + the script is built to run on a loop going through each species and calling all subscripts in order, however this is not useful if wanting to learn what each script does, so i suggest manullay setting i=1 and running through each line and subscript individually rather than running the whole loop
* If you wish to run the code on your own data, ensure your data has the same headings as the example.csv file and simply replace example_data.csv with your filename.

**!!IMPORTANT 2!! installing java may be required**:  
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


