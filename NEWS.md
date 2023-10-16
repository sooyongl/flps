# flps 1.0.0

## New Features
* Added Latent Class Analysis and Latent Profile Analysis.
* Imported example data for binary, polytomous, and continuous items.
* [Note] Stan scripts for multilevel models have been added but are not yet activated.
* [Upcoming] Support for multilevel structures will be introduced in future updates.

## Updates to Functions
* In `runFLPS()`, renamed the argument `group` to `trt`.
* Added a new argument `multilevel` to `runFLPS()`.
* `flps_plot()` can visualize class profiles.
* Class proportions are obtained by `summary()` for LCA or LPA models.

# flps 0.1.0

* flps is developed for Fully-Latent Principal Stratification model.
* Added a `NEWS.md` file to track changes to the package.
