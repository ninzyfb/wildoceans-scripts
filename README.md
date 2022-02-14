# wildoceans-scripts
This repository contains all the necessary code to build the shark and ray conservation plan developed for WILDOCEANS.  
There are three folders containing scripts in this repository, as well as additional folders which contain data and layers necessary to run the scripts.  
**!!IMPORTANT!!**: the occurrence data to run the models is confidential and not available to download. If you wish to run the code on example data, use the  **example.csv** file. This  allows for the modelling scripts to be run on freely available data from GBIF and OBIS for *Acroteriobatus annulatus*.  
For the modelling scripts to run on this data, go to `species_data.R` in the **scripts_modelling folder**. Change *FILENAME = paste(toupper(target),".rds",sep="")* to *FILENAME = "example.csv"*.  
This will be sufficient for the other scripts to run. If you wish to run the code on your own data then make sure your data has the same headings as the example.csv file and simply replace example.csv with your filename.

### 1. scripts_occurencedata
This folder contains two scripts that were used for initial data cleaning and data exploration of the raw occurrence data. The occurrence data in itself is confidential but the scripts may be useful to see how it was cleaned and formatted.  
`merging_datasets.R` is the script used to combine data from the various different data providers and output one file per species. These output files are used as input in the modelling scripts.

### 2. scripts_modelling
This folder contains scripts that were used to produce and plot the species distribution models (SDMs). This folder itself contains two subfolders.

- **scripts_envtvariablprep**
  Code for the preparation and processing of the environmental variable rasters. The outputs from these scripts were raster files used to build a rasterstack of environmental variables at 5x5km and 10x10km resolution. These prepared raster files are available in the main wildoceans-scripts repository in folders named *ALLLAYERS5kmresolution* and *ALLLAYERS10kmresolution*. These are sufficient to run the modelling scripts. 
  
- **scripts_modelling**
  Code to run all the distribution models. Start with `mainscript-modelling.R` as this is the parent script through which the other scripts are run. This script has the orders in which to run the scripts and also describes what each one is for. 

### 3. scripts_planning
This folder contains the code to run the spatial planning algorithm *prioritizr*. The *prioritizr* package has an incredible [online tutorial](https://prioritizr.net/articles/prioritizr.html) and [github page](https://github.com/prioritizr/prioritizr), and the authors are very responsive to issues or questions that may come up when you use the package.  
Similarly to the modelling scripts, start with`mainscript-planning.R` as this is the parent script through which the other scripts are run.

### 4. IUCN
Contains range maps for a number of shark and ray species in South Africa. Can be used to run the spatial planning scripts instea dof

### 5. ALLLAYERS 5 & 10 kmresolution
This folder contains the environmental rasters to run the distribution models at 5 or 10 km resolution.

### 6. plotting_features
This folder contains the layers necessary to produce plots with outputs from the modelling and planning scripts. The modelling and planning scripts produce rasters which can be plotted as annotated maps using the features from plotting parameters.


