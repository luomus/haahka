## 0.2.0 (2019-XX-XX)

- Minor number should've been increased already a while back, but it's better
late than never-

### Bug fixes

- Fix app title.
- Median dates were in wrong order, fixed now.

### New features

- 3+3 species abbreviation can be passed as an URL parameter, e.g. 
  https://jlehtoma.shinyapps.io/halias-browser?species=picpic will bring up 
  the data for the Common Magpie (Pica pica). 


### UI changes

- Trophy icons are golden.

## 0.1.5 (2019-01-02)

- New box giving information on long and short term trend changes, and average
abundance numbers for each of the three time periods.
- New plot showing the median dates for spring and autumn migration for three
different time periods.
- New box showing the observer recors (sum and date) for each species.
- Add (temporary) controls to show the effects of different series and line
drawing options. Following options are supported:
  - The size of the (non-overlapping) averaging (tiling) window
  - The line type
  - Showing shading (plot bands) for months in the background

## 0.1.4 (2018-12-28)

- Select only species, not species groups.
- Use common names and scientific names in the species selector.
- Move species image and description on the left side.
- Translations updated.
- Implement an image slider, i.e. support multiple images for a single species.
  (Disabled for now)
  
## 0.1.3 (2018-12-28)

- Make more UI components reactive.
- Start using translations with package `shiny.i18n`.


## 0.1.2 (2018-12-27)

- Exporting graphs as images or data (CSV and Excel) enabled.
- Zooming in graphs enabled.
- First version of loading text from a description docx. Includes further
translation into HTML elements and associated CSS
- First version of displaying images in the app.

## 0.1.1 (2018-12-27)

- Implement graph 5 using highcharter; all graphs (3) now in initial state.

## 0.1.0 (2018-12-22)

- New layout
- Graphs 2 and 3 implemened using highcharts
- Whole project migrated to GitLab
- Add README, LICENSE and CONTRIBUTING
