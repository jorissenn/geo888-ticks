# Linking exposure, climate and land cover data for seasonal modeling of the potential tick bite risk across the Swiss plateau
## GEO888 - GIS for Environmental Monitoring

This repository contains the data and scripts necessary to reproduce the implementation and the findings of the tick bite indicator. The associated report can be accessed [here](https://jorissenn.shinyapps.io/geo888_ticks/).

The script *data_processing.Rmd* is used to obtain the final distribution of tick bite risk from the raw data. A separate script (*climate_model_data_processing.Rmd*) is used for intermediary processing of the climate change scenario data supplied by the National Centre for Climate Services.

The following datasets used during the analysis were omitted from the repository due **data storage constraints**:

* [Mean daily temperature high in July, period 1981-2010](https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.wald-lufttemperatur_juli_1981_2010?.language=en)
* [Relative air humidity in July, period 1981-2010](https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.wald-relative_luftfeuchte_juli_1981_2010?.language=en)
* [Land use statistics](https://www.bfs.admin.ch/bfs/en/home/services/geostat/swiss-federal-statistics-geodata/land-use-cover-suitability/swiss-land-use-statistics.assetdetail.20104753.html)
* [Population and household statistics](https://www.bfs.admin.ch/bfs/en/home.assetdetail.23528269.html)

The following datasets used during the analysis were omitted from the repository due **confidentiality agreements**:

* [Swiss Climate Change Scenarios CH2018](https://www.nccs.admin.ch/nccs/en/home/climate-change-and-impacts/swiss-climate-change-scenarios/ch2018---climate-scenarios-for-switzerland.html), available upon request.
* Tick bites ZHAW
