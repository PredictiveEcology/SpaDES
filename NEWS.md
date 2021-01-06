Known issues: https://github.com/PredictiveEcology/SpaDES/issues

version 2.0.6
=============

* fix CRAN check errors
* fix discrepancy in vignette title

version 2.0.5
============= 

* add dependency package `SpaDES.experiment` to facilitate running simulation experiments.
* new package hexsticker and cheatsheet

version 2.0.4
============= 

* drop support for R 3.3 and 3.4, as these are no longer supported by several dependencies.

version 2.0.3
============= 

* move caching vignette to `SpaDES.core` package and remove unused dependencies `archivist`, `devtools`, `hunspell`, `igraph`
* add `RandomFields` to Suggests (needed for vignettes etc. in `SpaDES.core` and `SpaDES.tools`)
* new 'Getting Started' vignette describing the `SpaDES`-ecosystem packages

version 2.0.2
============= 

* change maintainer email address.
* update LandWeb app URL in README.
* update cache vignette.
* update package dependencies to include those used in tests.

version 2.0.1
============= 
 
* minor bugfix concerning imports 

version 2.0.0
=============

## Dependency changes

* `SpaDES` package split into several (#198):

    - caching moved out of `SpaDES` and into new package [`reproducible`](https://github.com/PredictiveEcology/reproducible). `reproducible` added to Imports.
    - plotting moved out of `SpaDES` and into new package [`quickPlot`](https://github.com/PredictiveEcology/quickPlot). `quickPlot` added to Imports.
    - all RStudio addins moved out of `SpaDES` and into new package [`SpaDES.addins`](https://github.com/PredictiveEcology/SpaDES.addins).
    - core DES components moved out of `SpaDES` and into new package [`SpaDES.core`](https://github.com/PredictiveEcology/SpaDES.core). `SpaDES.core` added to Imports.
    - additional modelling utilities (non-core components) moved out of `SpaDES` and into new package [`SpaDES.tools`](https://github.com/PredictiveEcology/SpaDES.tools). `SpaDES.tools` added to Imports.
    - `shiny`-related components moved out of `SpaDES` and into new package [`SpaDES.shiny`](https://github.com/PredictiveEcology/SpaDES.shiny).

* minimum R version increased from `3.2.2` to `3.3.3` as required by several dependencies.
* added `bit`, `fastmatch`, `fastdigest` and `Rcpp` packages to Imports.
* removed `stringr` package from Imports.

## Defunct

* `getFileName`. Wasn't used.
* `p`. Use `params` or `P` instead.
* `shine` moved out of `SpaDES` into the [`SpaDES.shiny`](https://github.com/PredictiveEcology/SpaDES.shiny) package.
* `versionWarning`. Wasn't used.

## New vignette

* `iii-cache` which shows many of the ways to use `Cache` and build it into a larger reproducible workflow.

## New functionality

* namespaced module functions have now been implemented via a nested environment in the `simList@.envir` slot. This means that module functions can be called by their name only, *without* a `sim$` prefix. Also, there should not be any name clashes between modules, so each module can have its own `init` function, say. This has been implemented in a backwards compatible way, but the old way may be deprecated down the road.
* new functions `saveSimList` which saves all environments recursively and file-backed objects, such as `Raster` objects (currently only one implemented).
* Created a generic for `Copy`, moved it to `reproducible`, and here added a method for `simList` objects that deep copies all environments recursively.
* change default value for `speedup` in `gaussMap`; now 1.
* new function `getPaths()` to return the list of working dirs from the options
* new function `setPaths()` as wrapper for setting the options for working dirs; uses `~/SpaDES` as default base path
* `downloadModule` checks for and uses environment variable `GITHUB_PAT` if it exists (alleviates 403 download errors caused by GitHub download limits)
* `spread2`: new function that is more robust than spread, slightly slower under some situations, but faster in many others. The function is designed to be used as a building block for more complex spreading, where the user can wrap `spread2` inside a custom function that iteratively calls `spread2`, while optionally changing any of the input arguments.
* adj - has a new argument `returnDT`, which slightly improves speed in cases where the user wants the output to become a `data.table`. This simply prevents a call to `as.matrix` in the `return()`, which was occurring if the return value was a `data.table`.
* new function `copyModule` for creating a copy of an existing module

## Performance

* add C++ internal functions for speed
* event queues (event, completed, current) now do shallow copies of pre-existing data.tables, rather than `rblindlist( )`. This improved DES speed by > 40%. Benchmarking of 1400 events in 1.6 seconds now; previously 3.0 seconds.
* add `Copy` (capital C) as replacement for `copy` which conflicts with `data.table::copy`, and add `queues` argument to do a deep copy of the event queues.
* `spread`: minor speedups, plus 3 new parameters `relativeSpreadProb`, `numNeighs` & `exactSizes`
* `distanceFromEachPoint`: uses faster Rcpp code for some use cases & can now take arbitrary columns in either `from` or `to` arguments
* use `%fin%` as faster replacement for `%in%`

## Bug fixes

* workaround issues with RStudio graphics on Linux (with #116): use `dev.noRSGD()` to bypass the RStudio graphics device for your current session (sets the `device` option for your platform).
* `checksums(..., write = TRUE)` ignores the contents of `CHECKSUMS.txt`, overwriting that file with the checksums of all files in the module's `data/` directory. This makes it easier to update the checksum file, *e.g.*, when adding new data (#332).
* improved module versioning (#321)
* minor bugfixes for unusual cases
* some broken examples were fixed

## Other updates

* improved documentation
* modules are now run with their required packages (`reqdPkgs` in metadata) temporarily bumped to the top of the search path (*i.e.*, `search()`) during each event. `search` path is restored `on.exit` from `spades` or `simInit` call
* change order of parsing of modules: `defineModule` is last, so can use objects defined within module for parameters
* improved `simInit` debug mechanism -- passing character string of module name will enter into a `browser` call inside `doEvent`
* improvements to caching --> these are moved to `reproducible` package:

    - improved caching for `Raster*` objects & S4 methods - now it normally persists across sessions;
    - add `Cache` (upper case) which derives `cacheRepo` arg automatically from either the `cachePath(sim)`, if used within a module, or `getPath()$cachePath` if not within a module. Also, the upper case removes the name conflict with `archivist::cache`;
    - uses `fastdigest::fastdigest` for RAM objects and `digest::digest` for disk-backed objects;
    - add caching mechanisms at the module-level and event-level (via new `.useCache` parameter, which can be logical indicating whole module or character indicating individual events);
    - add caching for `.inputObjects` function in `simInit`, via `.useCache` parameter in module
    - detailed caching overview now in cache help: `?Cache` for details.
    - strips dirname for outputs and inputs, *i.e.*, only keeps the filename, not absolute paths. This may not be stringent enough in some cases.
    - New cache-related function: `keepCache`

* implemented `checkModuleLocal()` to check for presence of module files in the module dir before attempting download from remote module repository
* improve module template to auto fill module author info using `devtools.desc.author` option if set.
* `zipModule` now has `data` argument, allowing data to be omitted from zipped module.
* improved `moduleDiagram` to show `_INPUT_` node in different colour from the other modules

version 1.3.1
=============
* Default module path (set via `spades.modulePath` option) is now set to a temporary location to avoid unintentionally writing to the user's home directory.
* add options for cache, input, output dirs (defaults to a temp dir until changed by user)
* rename `spades.modulesPath` and `spades.modulesRepo` options to `spades.modulePath` and `spades.moduleRepo`
* New cache-related functions: `showCache`, `clearCache`

version 1.3.0
=============
* stricter package version dependencies in Imports and Suggests
* `debug` argument in `spades()` can now take any expression, character strings. `TRUE`/`FALSE` changed behavior to show only current event.
* timeunits: when there are parent and grandparent modules, if timeunit is defined, it overrides the "smallest unit" rule. Thus, a parent module can force a timeunit.
* Plot - enhancements and fixes:

    - more robust base plotting and many visual tests on Windows;
    - re-add `col` arg to `Plot` (mimicks `cols`). Was lost from version 1.1.2;
    - allow any arbitrary function to be used internally to `Plot` (*e.g.*, barplot, plot, etc.);
    - add `arr` argument to `Plot()`, allowing passing of arrangement;
    - allow `title` arg in `Plot()` to accept character for plot title;
    - `Plot` can use character passed to `title` as a title;
    - some additional functionality for plotting of factor rasters, incl. `clickValues`, legends correct for wide variety of types;
    - change `new` arg in `Plot()`. Now it does one plot at a time, not whole device. Use `clearPlot()` to wipe whole device.

* Add `modulesGraph`, showing parent and child module relationships.
* Add `filesOnly` arg to `shine()`. This can be in preparation for publishing to www.shinyapps.io or other pages. Currently still alpha.
* Add POM: Pattern Oriented Modeling (#269). A simple interface to a `simList` object, allowing fitting parameters to data.
* add `.inputObjects` functionality -- function that runs during `simInit()` to create default `inputObjects`
* add `P` as a namespaced shortcut for `params`: `P(sim)` would replace `params(sim)$moduleName` when called from within that module
* allow `params(sim)` & `start(sim)` & others in `defineModule()` by changing parse order in module metadata
* add explicit `cl` arg to parallel aware functions, for more control
* `newModule` gains new arguments `type = c("child", "parent")` and `children = c()`. See `?newModule` (#300).
* module structure now includes an R subfolder for R scripts. These will be parsed during `simInit`.
* `checksums` updated to use faster hashing algorithm (`xxhash64`) and now only requires a single hash value per file (#295)
* fixed bugs in module template
* fixed bug in `cir` (#288, #311)
* improved use of package options; added new option `spades.modulesPath`.
* improved `downloadModule` to use option `spades.modulesPath` (#309)
* improved specification of module inputs and outputs (#189, #214, PR #310)
* remove module version warning (#312)
* other tweaks and fixes

version 1.2.0
=============
* fix bug associated with forthcoming `dplyr` update
* remove `gtools` and `secr` from Imports
* `spread` enhancements: circular spreading, landscape-based functions, allow overlapping events
* enhancements to `cir` and bug fix (#290)
* new function `rings` whose argument names closer match to `cir`
* performance enhancements in `spread`, `rings`, `cir`
* add cache and progress args to spades and experiment functions
* `Plot` now clips symbols (generally points) to plotting area. This will allow future "wiping" of plot area.
* `dev()` returns `dev.cur` invisibly, allowing for finer control of plotting devices
* add `distanceFromEachPoint`, a multipoint version of `raster::distanceFromPoints`
* improve `splitRaster` (#276, #284)
* new function `mergeRaster` (#282, #283)
* changed and performance boost to `randomPolygons` (uses `spread` internally now)
* minor performance enhancements
* many more and improved unit tests

version 1.1.4
=============
* fix OSX CRAN check errors caused by inconsistent use of `normalizePath`

version 1.1.3
=============
* add `gtools` to Imports
* bug fix in `experiment` that did not allow parallel spawning on some systems and crashed with empty `outputs` argument* fix minor bugs in `sampleModules`
* lots of documentation enhancements, esp. `?inputs` and `?outputs`
* add `.plotInitialTime` and `.saveInitialTime` arguments to `spades()` to easily allow turning off plotting and saving
* more robust `inputs` and `outputs`, including extension-based automated outputs, using `data.frame` instead of `data.table` allowing lists to be passed for "arguments"
* allow vectorized `n` in `setColors` (#70), and partial `n` if named
* many more unit tests (#139)
* other bug fixes

version 1.1.2
=============
* fix issues associated with upcoming `dplyr` updates
* performance enhancements: much faster simulation execution
* `Plot` accepts colour column in `SpatialPointDataFrame` objects
* add `col` arg to `Plot` (mimicks `cols`)
* add `experiment` function, a wrapper around `spades` for running multiple simulations (#265)
* add `shine` function, with `simList` signature (#261)
* add `copy` function, which does a deep copy of a `simList` object
* add `RColorBrewer` to Imports
* allow `Plot` colours to be set in the `Plot` call using `cols` argument
* allow `Plot` colours to be set in the `Plot` call using `RColorBrewer` palettes
* fix bug when plotting certain real-numbered rasters
* fix bug in `Plot` legends
* fix bug in assigning vectors as default module parameter values
* fix bug that prevented printing simulation times
* fix bug in event list sorting by `eventPriority`
* new slot in `simList` object: `current`, to store the current event
* new accessors `current` and `current<-` to get and set the current event
* `defineParameter` now coerces the `default` value to match the type `class`
* export `objectNames()` for external use
* `outputPath` updates `output(sim)$file` file paths, in addition to just `paths(sim)$outputPath`
* other bug fixes

version 1.1.1
=============
* require `archivist` version 2.0 or greater
* improved `moduleCoverage` testing  and template (PR #257)
* correct legends from rasters so that `is.factor(raster)` is `TRUE`
* user defined time units can be used in module metadata "timeunit".
* add module timeunits to `simList` show method (#260)

version 1.1.0
=============
* require R version 3.2.2 or higher
* remove `downloader` from Imports (#203)
* add `covr` to Suggests and `lazyeval` to Imports
* require `DiagrammeR` version 0.8.2 or higher which fixes mermaid/knitr/pandoc error (https://github.com/rich-iannone/DiagrammeR/issues/139)
* add data source info module metadata (#205)
* new function `downloadData` to download module data (with #205)
* new function `checksums` to verify MD5 hashes of data files (with #205)
* warn instead of error when using *e.g.*, `newModule` with RStudio on Windows (#209)
* more control of caching behaviour: added `cachePath` to paths slot (#227)
* only download data if file doesn't exist or checksum mismatch (#229)
* allow multiple checksums per file in `checksum` (#230)
* fix error caused by coercing `NA` to arbitrary class for which no suitable `NA` type exists (#231)
* use warnings instead of errors for missing modules metadata (#233)
* add event priorities (#236)
* enhanced functionality of `spread()` (#237)
* add unit tests and coverage to `newModule` (PR #242, PR #245)
* objects passed to `simInit` can be named differently from their objects (#247)
* `downloadModule` and `downloadData` now also download children modules/data
* new function `divergentColors` to generate divergent colour palettes for legends
* improve efficiency of `loadPackages`
* change `.spatialObjects` class to `spatialObjects` and export
* add .Rdata and .rds files to automatic loading.
* warn user if `SpaDES` package version doesn't match module version
* allow more signatures in `simInit` (modules can be character)
* other bug fixes

version 1.0.3
=============
* fully fix `inputs` data.frame construction in `simInit` (#225)

version 1.0.2
=============
* update maintainer's and authors' email addresses
* fix bug in `.parseModule` (#215)
* improve dependency graph & module diagram (#216)
* `simList` accessors now work with `.simList` superclass (#217)
* fix `%>%` bug in demo (#218)
* use `rmarkdown::render` for vignettes (with #219)
* improve documentation (including #219)
* reduce sizes of built vignettes (#222)
* add slot `documentation` to module metadata (see `?defineModule`) (#224)
* fix `inputs` data.frame construction in `simInit` (#225)
* various other bug fixes

version 1.0.1
=============
* no longer `attach` the simulation environment (#212)
* improve documentation
* bug fixes

version 1.0.0
=============
* no longer rely on side effects to update objects in global environment; instead uses simulation environments (#185) (not backwards compatible!)
* sample modules rewritten to use simulation environments (#185) (not backwards compatible!).
* redefined `simList` class:

    - new superclass `.simList` contains all previous `simList` slots except `.envir`
    - class `simList` extends `.simList` by adding slot `.envir`
    - new class `simList_` extends `.simList` by adding slot `.list`
    - `simList_` can be used internally to save a `simList` object,
    because saving lists of objects to file is more reliable that saving environments.

* `fastshp` can be installed from Rforge as a CRAN-like repository (instead of relying on `devtools::install_github`)
* software requirements changed: depends R (>=3.2.0)
* moved `data.table`, `grid`, `raster`, and `sp` from Depends to Imports (#207).
* add `archivist`, `ff`, `ffbase` to Imports;  add `lubridate` to Imports (with #151)
* removed `magrittr` from Imports (`%>%` is now exported by `igraph`)
* most external methods/classes imported using `importFrom`; except `methods`, `graphics`, `igraph`, and `utils` (because of methods/classes which are not exported) (#201)
* simulations now use `timeunit` (instead of `timestep`) specified in metadata (#151)
* `defineModule` requires `timeunit` to be a character string instead of numeric (with #151)
* simulation checkpointing fixed (#191, #130)
* `ls` and `ls.str` can now be used with `simList` objects to list objects in the simulation environment
* `objs` returns a list containing all the objects in the simulation environment
* new function `splitRaster` to divide `RasterLayer` into several tiles
* new function `normPath` (extracted from `checkPath`) to more strictly normalize filepaths without checking that they exist.
* new function `classFilter` to filter list of objects by their type
* new function `packages` to get all packages required by a simulation
* new function `rndstr` to generate random strings
* new function `append_attr` to append list items and keep their attributes
* improved `loadPackages`
* improved `.objectNames`
* `defineParameter` now accepts `min`, `max`, and description values (#172; #175)
* `defineModule` better handles `NA` values (#138)
* various `Plot` improvements.
* new plotting functions to produce overview diagrams of simulations (#181; #184):

    - `eventDiagram` shows Gantt chart of event sequences
    - `moduleDiagram` shows igraph network diagram of dependencies
    - `objectDiagram` shows sequence diagram of object dependencies between modules

* improved `simList` show and debug methods (#57; #73)
* improvements to `loadFiles`:

    - use `data.table`
    - allow `package::function` notation in load (#174)
    - bug fixes

* added "metamodules" which specify module groups (#176)
* improved test coverage (see #139)
* all functions imported explicitly (`@importFrom`).
* three entire packages imported: `methods`, `graphics`, `igraph` (because of classes which are not exported from `igraph`) (#201)
* removed package dependencies on raster, sp, data.table, grid. Use importFrom instead (#207).
* improved documentation and vignettes
* added cache mechanism to hash function calls to spades (#206)
* fixed progress bar bug (#147)
* prepend copyright info to `LICENSE` and `README` (with #140)
* various other bug fixes

version 0.6.2
=============
This is a minor release update to satisfy CRAN submission requirements.

* bug fix when saving files

version 0.6.1
=============
* added the Queen as copyright holder (#140)
* explicitly use GPL-3 (previously used GPL) (#140)
* modules now require metadata; module code without it will break!

    - new module dependency checking and metadata structure (#126)
    - explicit declaration of parameters, and object (data) dependencies
    - deprecated use of `reloadModuleLater`

* new module directory structure (#125):

        moduleName/
        |_ moduleName.R    # the actual module code file, incl. module metadata
        |_ moduleName.Rmd  # longform documentation and usage info, etc.
        |_ citation.bib    # properly formatted bibtex citation for the module
        |_ LICENSE         # license file describing the allowed usage etc. of the module
        |_ README          # incl. module metadata in addition to version change info, etc.

* updated package dependencies:
    * added `downloader`, `dplyr`, `fpCompare`, `httr`, `magrittr`, `stringr` to `Imports`
* updated vignettes and documentation improvements
* transferred project to `PredictiveEcology/SpaDES` from `achubaty/SpaDES`.
* overhaul to `Plot` function:

    - no longer needs `*Named` objects.
    - now handles `SpatialPolygons` objects much faster than default `plot`:
        - Uses package `fastshp`.
            - The suggested package `fastshp` can be installed with:

                    install_github("s-u/fastshp")

    - numerous other `Plot` improvements

* deprecated all `*Named` functionality

    - All code will break that uses `*Named` objects

* new function, `clearPlot` can be used to clean all hidden metadata about a plot
* new function, `randomPolygons`, will create random raster maps (not vector maps) of polygons
* numerous bugfixes

version 0.4.0
=============
* second public ("beta") release
* more improvements to `Plot` function, including dramatically faster for files on disk
* added second "Model" that adds and integrates 3 sample modules, Fire, Succession and Aging
* added Plotting vignette
* bugfixes

version 0.3.1
=============
* first public ("beta") release
* reworked plotting and visualization (it's faster, more reliable)
* added additional parameter checks and improved debugging
* new and improved documentation:
    - two new vignettes (introduction to SpaDES and how modules work)
    - new demo highlighting visualization components
    - more complete code documentation
* numerous bug fixes

version 0.2
=============
* renamed package to `SpaDES`

version 0.1
=============
* first working alpha version
* lots of "wishlist" items still to be implemented
