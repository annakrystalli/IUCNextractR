# IUCNextractR

### #rstats functions to extract environmental data across IUCN red list species ranges. 

- Functions are in the R/ directory

### extraction

The repo is also associated with the extraction of range wide enviromental parameters (tmp & pre using high-resolution gridded [CRU TS3.24.01](http://catalogue.ceda.ac.uk/uuid/3df7562727314bab963282e6a0284f24) timeseries data, [accessed](https://crudata.uea.ac.uk/cru/data/hrg/) *2017/05/31*) for birds (using [BirdLife range shp files](http://datazone.birdlife.org/species/requestdis)) and reptiles (using [IUCN range shp files](http://www.iucnredlist.org/technical-documents/spatial-data)). Data not included in the repo.

The analyses where parallelised and run on the [UoS ShARC HPC cluster](https://www.sheffield.ac.uk/cics/research/hpc/sharc).  For each extraction there is, in the repo root: 

- a scheduler submission bash script (`\*. sge`) file 
- an extraction script (`\*.R`). 

Scripts associated with the reptiles extraction are prefixed `02_` and with birds `03_`. There's also a pre-processing script (`01_*`).  

In the Google drive directory that holds the extracted data includes a `logs/` dir containing logs of the output from the extraction on the cluster.
