# infernet 0.1.1

## Package

- Updated `DESCRIPTION`
  - Raised the R minimum to 4.1.0, since the examples use the native pipe
  - Pinned the minimum versions of `{manynet}` and `{netrics}`
- Updated CONTRIBUTING to document the architecture and the house conventions
- Added `README.Rmd` and `pkgdown/_pkgdown.yml` for this package
- Improved the loading messages
- Updated the Github Actions workflows
  - Added the PR metadata checks for the version bump and the PR title and body
  - Release notes are now taken from the matching `NEWS.md` section
  - Updated the action versions in `prchecks` and `pushrelease`
- Improved console messaging to use the `snet_*()` wrappers from `{manynet}`
  - Informational output is now silent by default, and follows `snet_verbosity`
  - Errors name what is missing and what is available
  - Added `thisRequires()`, which names the install command for a suggested package

## Tests

- Updated the `tests` documentation to describe `test_configuration()`
- Updated the examples to use the native pipe `|>`


# infernet 0.1.0

## Package

- Initialised package
