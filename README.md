<!-- This comment enables badge extraction to pkgdown site -->

[![Travis-CI Build Status](https://travis-ci.com/IMCR-Hackathon/ggplotgui.svg?branch=master)](https://travis-ci.org/IMCR-Hackathon/ggplotgui)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/IMCR-Hackathon/ggplotgui?branch=master&svg=true)](https://ci.appveyor.com/project/IMCR-Hackathon/ggplotgui)

# ggplotgui

This package allows users to explore the suitibility of a data package for further inquiry via a R Shiny.
Users identify data packages of interest by passing a DOI to a GUI interface and then browse summary reports and generate exploritory plots.
The package also attempts to address some common shortcomings of data formats to aid suitability evaluation.
Data is highly diverse and we provide no guarantee this will actually work for a given data package, but it's worth a shot!

## Background

Data can be a valuable scientific resource.
However the inherent existance of information is not sufficeint to valuable, data must also be [Findable Accesible Interoperable Reusable](https://www.force11.org/group/fairgroup/fairprinciples).

This project assists researchers and other data users who wish to reuse existing data packages that are archived on [DataOne](https://www.dataone.org/) member notes.
We provide a R shiny application (built off of the framework provided by [ggplotgui](https://github.com/gertstulp/ggplotgui)) that 1) downloads identified DOIs registared on DataOne member notes, 2) reads in the data into a light weight viewer, 3) provides summary statistics and basic graphics describing the data package, and 4) generates a more robust report describing the data.
This is not intented to replace a full analysis in R or comparble statistical packages, but instead intended to allow the user to quickly access if the data is suitable for their needs.

Access to data packages on DataOne member notes is provided through the package [metajam](https://github.com/NCEAS/metajam).

## Install

TBD

## Usage

TBD

## Road map

TBD

## Contributing

TBD


## Versioning

This project uses [semantic versioning](https://semver.org).

## Authors

TBD

## Acknowledgements

This package was the product of the 2019 Information Management Code Registry Hackathon sponcered by the [Environmental Data Initiative](https://environmentaldatainitiative.org/).

We are very greatful to [Gert Stulp](https://www.gertstulp.com/) for their [ggplotgui](https://github.com/gertstulp/ggplotgui) package form which this repository is forked.

Dr Stulp's orginal acknowledgement follows:
> I am grateful to the people who made [R](https://www.r-project.org/), and to [Hadley Wicham](http://hadley.nz/) for making such good packages (and open access books describing them), that allow even low-skilled and low-talented programmers like myself to be able to contribute to R. This package makes use of: [ggplot2](http://ggplot2.tidyverse.org/), [Shiny](http://shiny.rstudio.com/), [stringr](http://stringr.tidyverse.org/), [plotly](https://plot.ly/r/), [readr](http://readr.tidyverse.org/), [readxl](http://readxl.tidyverse.org/), [haven](http://haven.tidyverse.org/), and [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf). Package development through [RStudio](https://www.rstudio.com/) and [Github](https://github.com/), and with the help of [R Markdown](http://rmarkdown.rstudio.com) and [devtools](https://www.rstudio.com/products/rpackages/devtools/). The code that allows for online data input was based somewhat on the [BoxPlotR Shiny app](https://github.com/VizWizard/BoxPlotR.shiny). Many thanks to Wilmer Joling who set up the illustrous "[docker](https://www.docker.com/)" and [website](https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/) where the online version of this packages runs.   
