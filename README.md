# wildoceans-scripts
<p>This repository contains all the necessary code to build the shark and ray conservation plan developed for WILDOCEANS.</p>
<p>There are __three folders containing scripts__ in this repository, as well as additional folders which contain data and layers necessary to run the scripts. Below is an outline of the contents of each folder</p>
<p>There is also an **example.csv** file in this repository. This  allows for the modelling scripts to be run on some example data from GBIF and OBIS for *Acroteriobatus annulatus*.</p>

### 1. scripts_occurencedata
<p>This folder contains two scripts that were used for initial data cleaning and data exploration of the raw occurrence data. The occurrence data in itself is confidential but the scripts may be useful to see how it was cleaned and formatted.</p>
<p>*merging_datasets.R* is the script used to combine data from the various different data providers and output one file per species. These output files are used as input in the modelling scripts.</p>

### 2. scripts_modelling
<p>This folder contains scripts that were used to produce and plot the species distribution models (SDMs). This folder itself contains two subfolders.</p>

- **scripts_envtvariablprep**
  Code for the preparation and processing of the environmental variable rasters. The outputs from these scripts were raster files used to build a rasterstack of environmental variables at 5x5km and 10x10km resolution. These prepared raster files are available in the main wildoceans-scripts repository in folders named *ALLLAYERS5kmresolution* and *ALLLAYERS10kmresolution*. These are sufficient to run the modelling scripts. 
  
- **scripts_modelling**
  Code to run all the distribution models. Start with *mainscript-modelling.R* as this is the parent script through which the other scripts are run. This script has the orders in which to run the scripts and also describes what each one is for. 

### 3. scripts_planning
<p>This folder contains the code to run the spatial planning algorithm *prioritizr*. The *prioritizr* package has an incredible [online tutorial](https://prioritizr.net/articles/prioritizr.html) and [github page](https://github.com/prioritizr/prioritizr), and the authors are very responsive to issues or questions that may come up when you use the package.</p>
<p>Similarly to the modelling scripts, start with *mainscript-planning.R* as this is the parent script through which the other scripts are run.</p>

### 4. IUCN

### 5. ALLLAYERS 5 & 10 kmresolution

### 6. plotting_features
This folder contains the layers necessary to produce plots with outputs from the modelling and planning scripts. The modelling and planning scripts produce rasters which can be plotted as annotated maps using the features from plotting parameters.


