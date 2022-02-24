# WILDOCEANS SHARK & RAY CONSERVATION PLAN 

## About
This repository contains all the necessary code used to develop the shark and ray conservation plan developed for WILDOCEANS. This is currently a LIVE project, so the code may still change.

# scripts
There are three folders containing scripts in this repository, as well as additional folders which contain data and layers necessary to run the scripts. 

**IMPORTANT: how to run scripts on your own data or example data provided**: the occurrence data used to run the conservation plan is confidential and not available for download. If you wish to run the code and  **example.csv** file is provied which  allows for the modelling scripts to be run on freely available data from GBIF and OBIS for *Acroteriobatus annulatus*.  Steps to use the example.csv file are as follows:  

1. open `species_data.R` in the **scripts_modelling folder**.  
2. Change *FILENAME=paste(toupper(target),".rds",sep="")* to *FILENAME = "example_data.csv"* 
3. Change *FILELOCATION=paste0(path,"Dropbox/6-WILDOCEANS/Modelling/speciesdata")* to *FILELOCATION=my.directory* or to the location where the example_data.csv file is saved.
This should be sufficient for the other scripts to run. If you wish to run the code on your own data, ensure your data has the same headings as the example.csv file and simply replace example_data.csv with your filename.

### 1. scripts_occurencedata
This folder contains two scripts that were used for initial data cleaning and data exploration of the raw occurrence data. The raw occurrence data are confidential but the scripts are provided to show how it was cleaned and formatted.  
`merging_datasets.R` is the script used to combine data from the various different data providers and output one file per species. These output files are used as input in the modelling scripts.

### 2. scripts_modelling
This folder contains scripts that were used to produce and plot the species distribution models (SDMs). This folder itself contains two subfolders.

- **scripts_envtvariablprep**
  Code for the preparation and processing of the environmental variable rasters. The outputs from these scripts were raster files used to build a rasterstack of environmental variables at 5x5km and 10x10km resolution. These prepared raster files are available in the main WILDOCEANS-scripts repository in folders named *ALLLAYERS5kmresolution* and *ALLLAYERS10kmresolution*. These are sufficient to run the modelling scripts. 
  
- **scripts_modelling**
  Code to run all the distribution models. Start with `mainscript-modelling.R` as this is the parent script through which the other scripts are run. This script contains the orders in which to run the scripts and also describes what each one is for. 

### 3. scripts_planning
This folder contains the code to run the spatial planning algorithm *prioritizr*. The *prioritizr* package has an [online tutorial](https://prioritizr.net/articles/prioritizr.html) and [github page](https://github.com/prioritizr/prioritizr), and the authors are very responsive to issues or questions that may come up when you use the package.  
Similar to the modelling scripts, start with`mainscript-planning.R` as this is the parent script through which the other scripts are run.

### 4. IUCN
Contains range maps for several shark and ray species in South Africa. This script can be used to run the spatial planning scripts instead of using the modelled distributions.

### 5. ALLLAYERS 5 & 10 kmresolution
This folder contains the environmental rasters to run the distribution models at 5 or 10 km resolution.

### 6. plotting_features
This folder contains the layers necessary to produce plots with outputs from the modelling and planning scripts. The modelling and planning scripts produce rasters which can be plotted as annotated maps using the features from plotting parameters.


