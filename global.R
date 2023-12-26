

### Load all available datasets
available_datasets <- list()
for(f in list.files("data")){
  available_datasets[[f]] <- read.csv(file.path("data",f))
}
