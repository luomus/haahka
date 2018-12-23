# Halias Browser

[Shiny](https://shiny.rstudio.com)-based app for browsing bird observation data from [Hanko Bird Observatory](https://www.tringa.fi/hangon-lintuasema/hankodata/).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

What things you need to install the software and how to install them

+ [R](https://cran.r-project.org) ( tested on versions >= 3.5.1)
+ [RStudio](https://www.rstudio.com) (optional, but useful for development)

In addition, you will need the following R packages:

```R
install.packages(shiny)
install.packages(shinydashboard)
install.packages(tidyverse)

# Needed to install the development versions of highcharter
install.packages(devtools)
# Highcharter >= 0.6.0 is needed for compatability with dplyr
devtools::install_github("jbkunst/highcharter")
```

### Installing

#### Release version

TBA.

#### Development version

Using git:

```
git clone https://gitlab.com/tringa-ry/halias-browser.git
```

Alternatively, [download](https://gitlab.com/tringa-ry/halias-browser/-/archive/master/halias-browser-master.zip) the repository as zip-file and decompress the zip-file. 

Make sure you have the dependencies installed. Fire up R, navigate to the root of `halias-browser` project and start the Shiny app:

```R
shiny::runApp()
```



## Updating the observation data

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Joona Lehtomäki** (<joona.lehtomaki@iki.fi>) - *Initial work* - https://gitlab.com/jlehtoma

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

* Vilppu Välimäki
* Aleksi Lehikoinen
