# This runs everything that we need! Libraries are unique to each sub file. 

# This gets the data from the github
source("ObtainData.R")

# This generates the Data.rds file that is cleaned
source("PrepData.R")

# This runs the modeling and analysis
source("Analysis.R")

# Visualization functions once analysis is run
source("Visualizations.R")