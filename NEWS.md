## 0.3.3 (2019-02-04)

- App name changed to "Haahka"

### Bug fixes

- Internally, default species is now enforced more strongly.

### New features

- Google Analytics tracking added.

### UI changes

- Species selector layout narrower than before.
- Species selector larger with larger text.
- Change feedback email addresses.

## 0.3.2 (2019-02-03)

### Bug fixes

- Parsing styles from the description docx-files works correctly.
- Selecting species through the URL query parameters was broken, but fixed now. 
  NOTE: species selection and setting the default species is still not completely
  solid and needs more work.

### New features

- New script files for:
  1. Downloading description files (docx) from Google Drive
  2. Downloading metadata spreadsheet from Google Drive
- It is now possible to select (in code) the species that is shown when the 
  app is first loaded. For now, the default species is the Mute Swan.
- The logo now links to www.halias.fi

### UI changes

- Give a more hopefule note if no description is found.
- Inline citations in description texts are formatted in italics.

## 0.3.1 (2019-01-31)

- Photos of 311 species now present.

- Package dependencies are now managed with `packrat` to facilitate 
  deployment.

### Bug fixes

### New features

- Info popups translated to Swedish.
- Hyperlinks (URLs) in reference lists are now active.
- New script files for:
  1. Downloading species photos from Google Drive
  2. Down-scaling (resizing) photos to 900 pixel width

### UI changes

## 0.3.0 (2019-01-16)

- Major update content-wise: 10 new species added and the app is ready for some
  demo time.

### Bug fixes

- Remove tons of redundant functions and code.

### New features

- Swedish translations added (info popups still missing).
- English translations for graph info popups added.
- Help page content added.
- Metadata (description author and photographer identities) are now read
  from a separate CSV file. This data is used to generate correct citation 
  guide.
- Add help text on data (not on the app yet).

### UI changes

- Author name, email and license now in sidebar footer.
- Citation guide now refers to the written description, not data itself.
- Show description only if language is Finnish.

## 0.2.1 (2019-01-10)

### Bug fixes

- Median day graph height is now dynamically adjusted based on the viewport
  size. This way it actually looks nice on mobile screens.

### New features

- Info buttons added for each graph. Clicking the button brings up a popup
  containing more information on the graph.
- Add citation instructions.

### UI changes

- Re-arrange the observation records layout.
- Color-schemes changed for "change" and "median dates".
- Species photos are now slightly smaller, because they were slightly too 
  wide for mobile layout.


## 0.2.0 (2019-01-06)

- Minor number should've been increased already a while back, but it's better
late than never-

### Bug fixes

- Fix app title.
- Median dates were in wrong order, fixed now.
- Chart x-axis labels (months) are now correctly translated.
- Translations updated.
- Tooltip translations in median migration days handled correctly

### New features

- 3+3 species abbreviation can be passed as an URL parameter, e.g. 
  https://jlehtoma.shinyapps.io/halias-browser?species=picpic will bring up 
  the data for the Common Magpie (Pica pica). 
- Language code can also be used an URL parameter, e.g.
  https://jlehtoma.shinyapps.io/halias-browser?species=accnis&language=en
  to share links directly to a given species and in correct language.
- Two new helper funtions:
    - `get_months()`: get short ot long month names in a give language.
    - `make_date_label()`: use for constracting multi-language date labels.
- New tab: help. Content is still largely missing.

### UI changes

- Species observations content is now places in a tab and the sidebarmenu
  is adjusted accordingly.
- New information tab contains information on the app.
- Trophy icons are golden.
- Temporary controls box removed.
- Color schemes changed to darker hues.
- App and data versions displayed in the sidebar footer.
- Proper implementation of tabs.

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
