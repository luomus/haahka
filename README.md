# Halias Browser

[Shiny](https://shiny.rstudio.com)-based app for browsing bird observation data from [Hanko Bird Observatory](https://www.tringa.fi/hangon-lintuasema/hankodata/).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

#### R

You will need the following:

+ [R](https://cran.r-project.org) (tested on versions >= 3.5.1)
+ [RStudio](https://www.rstudio.com) (optional, but useful for development)

#### System libraries

The following system packages are needed by the R-packages:

**Ubuntu 16.04**

TBA

#### R packages

The R package management is handled by [`packrat`](https://rstudio.github.io/packrat/). 
Packrat overloads the system R library path and uses a local library of packages
instead. This way, required packages can be automatically handled by packrat
and the installed packages will not conflict with the system library. To see
whichi packages will be installed by packrat, have a look at [here](https://gitlab.com/tringa-ry/halias-browser/blob/master/packrat/packrat.lock).

Packrat will automatically start installing the required packages once you
start R (or RStudio) at the root of this repository. If you need to update
packages at any time later, do the following from R:

```
packrat::restore()
```

In case you introduce new packages, packrat will automatically know about them.
You will still have to update packrat by:

```
packrat::snapshot()
```

## Installing

### Release version

TBA.

### Development version

Following instructions help you to get everything up and running on your local
machine. If you are interested in deplying the app on a remote server, please
see ["Deployment"](https://gitlab.com/tringa-ry/halias-browser/blob/master/README.md#deployment) below.

#### 1. Get the source code

Using git:

```
git clone https://gitlab.com/tringa-ry/halias-browser.git
```

Alternatively, [download](https://gitlab.com/tringa-ry/halias-browser/-/archive/master/halias-browser-master.zip) the repository as zip-file and decompress the zip-file. 

#### 2. Download additional content

Species photos and descriptions are not included in this repository. They are
managed by a number volunteers on Google Drive and so the updating procedure is
independent to this repository. Photos (jpg/png), description files (docx) and
additional metadata (csv) can be downloaded directly from Google Drive given
that you have permissions to access the necessary folder on Google Drive.

I.e., to download the necessary files, you 1) must have access to the necessary 
directories on Google Drive and 2) grant permission (authenticate) the code to 
download content. If you are working on a local machine, the code will direct
you to a browser window where you can grant the necessary permissions. If you 
are deploying the system on a remote system, please see ["Deployment"](https://gitlab.com/tringa-ry/halias-browser/blob/master/README.md#deployment) below.

Run the following R scripts from the root of the repository:

```
# Download original photos from Google Drive
./R/01_download_photos.R

# Re-scale photos to a smaller size
./R/02_resize_photos.R

# Download description files (docx)
./R/03_download_descriptions.R

# Download additional metadata. This file is also inculded in the repository
# and you can find it in the "data" subdir.
./R/04_download_metadata.R
```

Alternatively, you can source the files from your RStudio project.

#### 3. Launch shiny

Make sure you have [the dependencies](https://gitlab.com/tringa-ry/halias-browser/blob/master/README.md#r-packages) installed. Fire up R, navigate to the root of `halias-browser` project and start the Shiny app:

```R
shiny::runApp()
```

The development version of this app can also be found at https://jlehtoma.shinyapps.io/halias-browser/

## Updating the observation data

**NOTE** You will only have to do this whenever the actual observation data updates, not to get the app up and running.

The data used by this Shiny app lives in the `data` subfolder and is based on [the long term data from the Hanko Bird Observatory](https://www.tringa.fi/hangon-lintuasema/hankodata/). However, all the data pre-processing is done by code living in a different repository called `halias-observations` (see [here](https://gitlab.com/tringa-ry/halias-observations)). Whenever the source data is updated, the updates must be manually migrated into this repository. Do the following:

### 1. Download the updated data and run the pre-processing scripts

TBA

### 2. Download the generated RData file

TBA

### 3. Document the updated data in the Shiny app

TBA

## Using the translations system

TBA

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Shiny](https://shiny.rstudio.com) - The web framework used
* [highcharter](http://jkunst.com/highcharter/) - Plotting library (NOTE: the underlying Highcharts JS library is not free for commercial or government use, see [here](https://github.com/jbkunst/highcharter#licence) for more information )

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://gitlab.com/tringa-ry/halias-browser/tags). 

## Authors

* **Joona Lehtomäki** (<joona.lehtomaki@iki.fi>) - *Initial work* - https://gitlab.com/jlehtoma

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

* Vilppu Välimäki
* Aleksi Lehikoinen
