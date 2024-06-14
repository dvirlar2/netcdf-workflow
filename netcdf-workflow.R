## -- load libraries -- ##
library(dataone)
library(datapack)
library(arcticdatautils)
library(EML)
library(ncdf4)

## -- get dataset -- ##
d1c <- dataone::D1Client("PROD", "urn:node:ARCTIC")

packageId <- "resource_map_pid"
dp <- getDataPackage(d1c, identifier = packageId, lazyLoad=TRUE, quiet=FALSE)

xml <- selectMember(dp, "sysmeta@fileName", ".xml")

doc <- read_eml(getObject(d1c@mn, xml))



## -- load ncdf function -- ##
# this function has not yet been pushed to production, so make sure you run it!
# the input for the "data" argument is a file path. So you'll need the netCDF files
# either loaded into your folder ('home/username/...), or the visitor folder ('home/visitor/...)
netcdf_test <- function(data = NULL, missingValue = NULL){ # input must be file path to .nc
  # load netcdf data from arcticdatautils
  netcdf_atts <- arcticdatautils::get_ncdf4_attributes(data)
  
  # load netcdf data from nc_open 
  nc_data <- nc_open(data)
  
  # fill in attribute Name from attributeName column generated 
  attributeName <- netcdf_atts$attributeName
  
  # fill in attribute definition
  attributeDefinition <- netcdf_atts$long_name
  
  
  # fill in units
  unit <- netcdf_atts$units
  
  
  # fill in number type
  numberType <- rep(NA, length.out = length(netcdf_atts$attributeName))
  
  # find numberType from variables in $var from netcdf object
  for(i in 1:length(names(nc_data$var))){
    numberType[i] <- EML:::get_numberType(ncvar_get(nc_data, names(nc_data$var)[i]))
    # triple ':' used for non-exported functions w/in package
  }
  
  # find numberType from variables in $dim from netcdf object
  for(i in 1:length(numberType)){
    if(is.na(numberType[i])){
      if(!length(which(names(nc_data$dim) %in% netcdf_atts$attributeName[i])) == 0){
        j <- which(names(nc_data$dim) %in% netcdf_atts$attributeName[i])
        numberType[i] <- EML:::get_numberType(nc_data$dim[[j]]$vals) 
      }
    }
  }
  
  # fill in domain
  domain <- rep(NA, length.out = length(netcdf_atts$attributeName))
  
  # domain values for variables from $var from netcdf object
  for(i in 1:length(names(nc_data$var))){
    if(is.numeric(ncvar_get(nc_data, names(nc_data$var)[i]))){
      domain[i] <- "numericDomain"
    }
  }
  
  # domain values for variables from $dim from netcdf object
  for(i in 1:length(domain)){
    if(is.na(domain[i])){
      # NA values should be from dims in the netcdf object
      # need to find those names from the output of netcdf_atts$attributeName
      if(!length(which(names(nc_data$dim) %in% netcdf_atts$attributeName[i])) == 0){
        j <- which(names(nc_data$dim) %in% netcdf_atts$attributeName[i])
        
        if(is.numeric(nc_data$dim[[j]]$vals)){
          domain[i] <- "numericDomain"
        }
      }
    }
  }
  
  
  # fill in measurement scale
  measurementScale <- rep(NA, length.out = length(netcdf_atts$attributeName))
  
  for(i in 1:length(domain)){
    measurementScale[i] <- ifelse(domain[i] == "numericDomain", "ratio", NA)
  }
  
  
  # fill in missing value code
  missingValueCode <- missingValue
  
  # fill in missing value code explanation
  missingValueCodeExplanation <- rep(NA, length.out = length(netcdf_atts$attributeName))
  
  for(i in 1:length(missingValueCode)){
    if(!is.na(missingValueCode[i])){
      missingValueCodeExplanation[i] <- "Value not recorded"
    }
  }
  
  
  # bind all of the above together
  as.data.frame(cbind(attributeName,
                      attributeDefinition,
                      unit,
                      numberType,
                      domain,
                      measurementScale,
                      missingValueCode,
                      missingValueCodeExplanation))
}


## -- fill out netcdf attributes -- ##
# get atts from arcticdatautils so you know what the missingValue column is 
# (this is not uniform across files, or even researchers)


# retrieve the attributes from the netCDF file you need to process
adu_atts <- arcticdatautils::get_ncdf4_attributes("path/to/file.nc")

# use the output of the above function to find the missing value column
# use that as the input for the second argument in the function below
ncdf_atts <- netcdf_test("path/to/file.nc", adu_atts$missing_value)

# convert ratio to interval for lat/lon, and celsius
atts <- EML::shiny_attributes(data = NULL, attributes = ncdf_atts)

attList <- EML::set_attributes(attributes = atts$attributes)

doc$dataset$otherEntity[[i]]$attributeList <- attList

eml_validate(doc)



## -- convert OEs to DTs -- ##
doc <- eml_otherEntity_to_dataTable(doc, c(i:j), validate_eml = FALSE)


## -- add physicals -- ##
# don't forget about otherEntities, if there are any present in dataset
for (i in seq_along(doc$dataset$dataTable)) {
  dataTable <- doc$dataset$dataTable[[i]]
  id <- dataTable$id
  
  if (!grepl("urn-uuid-", id)) {
    warning("dataTable ", i, " is not a pid")
    
  } else {
    id <- gsub("urn-uuid-", "urn:uuid:", id)
    physical <- arcticdatautils::pid_to_eml_physical(d1c@mn, id)
    doc$dataset$dataTable[[i]]$physical <- physical
  }
}




## -- update package -- ##
eml_path <- "~/Scratch/xml-file-here.xml"
write_eml(doc, eml_path)

dp <- replaceMember(dp, xml, replacement = eml_path)

myAccessRules <- data.frame(subject="CN=arctic-data-admins,DC=dataone,DC=org", 
                            permission="changePermission")

newPackageId <- uploadDataPackage(d1c, dp, public = FALSE, 
                                  accessRules = myAccessRules, quiet = FALSE)
