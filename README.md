# Neighbours shapes pollination network topology

Pollinator visitation was tracked to flowering shrubs. We used visitation networks to explore the importance of floral display size and the local floral neighbourhood on the shape of network as a whole.


## Getting Started

These instructions describe the contents of this repository and will get you a copy of the project up and running on your local machine. 

See https://jennabraun.github.io/foundation-pollination/ for final work up 

### Data descriptions

* density_estimates.csv - Estimates of site-level flowering shrub and cacti density measured throught out the study period. Measured in transects
* focal_shrub_covariates.csv - Covariates for each individual plant sampled including floral display size, time, species, date, neighbouring shrub density. "WP" is gps waypoint and plant ID
* functionalgroups.csv - List of functional groups measured in this study
* visitation_data.csv - Identity and quantity of floral visitors to shrub/cacti in this study

Folders

* GPX - hold .gpx files for focal plants. Waypoints correspond to plant ID/WP
* Output - these are the cleaned and combined datasets output by visits.R and index.RMD
* Randomization Outputs - these are the output calculations from the randomization functions. The functions take a long time to run.
* Raw Data - Excel workbook holding raw data entry and a shapefile showing locations of plants.

* Raw Data folder holds 
* Clean Data folder 

### Script descriptions
* visits.R cleans visitation data from Data/visitation_data.csv and outputs visitation_cleaned.csv to be used in the index.rmd




