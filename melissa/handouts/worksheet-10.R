# Documenting and Publishing your Data Worksheet

# Preparing Data for Publication
library(...)

stm_dat <- ...("data/StormEvents.csv")

...(stm_dat)
...(stm_dat)

...(stm_dat$EVENT_NARRATIVE) 

...('storm_project', showWarnings = FALSE)
...(stm_dat, "storm_project/StormEvents_d2006.csv")

# Creating metadata
library(...) ; library(...)

...(dir = "storm_project")

...(stm_dat$YEAR)
...(stm_dat$BEGIN_LAT, na.rm=TRUE)
...(stm_dat$BEGIN_LON, na.rm=TRUE)

...(metadata_dir = here("storm_project", "metadata"))

...(metadata_dir = here("storm_project", "metadata"))

...(data_path = here("storm_project"),
            access_path = here("storm_project", "metadata", "..."))
...(metadata_dir = here("storm_project", "metadata"))

...(data_path = here("storm_project"),
                attributes_path = here("storm_project", "metadata", "..."))
...(metadata_dir = here("storm_project", "metadata"))

...(path = here("storm_project", "metadata"))

library(...) ; library(...) ; library(...)

json <- ...("storm_project/metadata/dataspice.json")
eml <- ...(json)
...(eml, "storm_project/metadata/dataspice.xml")

# Creating a data package
library(...) ; library(...)

dp <- ...("DataPackage") # create empty data package

... <- "storm_project/metadata/dataspice.xml"
... <- paste("urn:uuid:", UUIDgenerate(), sep = "")

... <- new("DataObject", id = ..., format = "eml://ecoinformatics.org/eml-2.1.1", file = ...)

dp <- ...(dp, ...)  # add metadata file to data package

... <- "storm_project/StormEvents_d2006.csv"
... <- paste("urn:uuid:", UUIDgenerate(), sep = "")

... <- new("DataObject", id = ..., format = "text/csv", filename = ...) 

dp <- ...(dp, ...) # add data file to data package

dp <- ...(dp, subjectID = ..., objectIDs = ...)

serializationId <- paste("resourceMap", UUIDgenerate(), sep = "")
filePath <- file.path(sprintf("%s/%s.rdf", tempdir(), serializationId))
status <- serializePackage(..., filePath, id=serializationId, resolveURI = "")

... <- serializeToBagIt(...) # right now this creates a zipped file in the tmp directory
file.copy(..., "storm_project/Storm_dp.zip") # now we have to move the file out of the tmp directory

# this is a static copy of the DataONE member nodes as of July, 2019
read.csv("data/Nodes.csv")






