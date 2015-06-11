## [Unreleased][unreleased]

### Added

* CLI option `languages` for custom languages.json

## [0.6.7] - 2015-06-02

### Added

* Support for pandoc greater than version 1.14

## [0.6.6] - 2014-09-24

### Changed

* Dependencies to be up to date

## [0.6.5] - 2013-05-14

### Fixed

* Links to section IDs in parallel layout
* Exclusion of shebangs within files

## [0.6.4] - 2013-05-11

### Fixed

* Data directory searching with the cabal package

## [0.6.3] - 2013-05-09

### Fixed

* Title headers when left empty

## [0.6.2] - 2013-05-07

### Changed

* Use of `Map` to `HashMap`
* Store language definitions in a separate JSON file

## [0.6.1] - 2013-05-06

### Fixed

* Multiple CSS inclusions in template files

### Removed

* Explicit normalize.css file in linear template

## [0.6.0] - 2013-05-05

### Added

* cmdags for command line argument parsing
* Help flag in CLI
* Version flag in CLI
* Error message when no files are put into the CLI
* More than one template -- linear and parallel

### Changed

* Headers, both h1 and h2, are on their own separate line
* Recursive directory to include subdirectory as well

## [0.5.0] - 2013-04-30

### Added

* Support for Literate Haskell

### Changed

* Code highlight from pygments to highlighting-kate

## [0.4.0] - 2013-04-26

### Added

* Support for pandoc greater than version 1.10
* Support for unicode characters

### Changed

* Functions using `ByteString` to `Text`

## [0.3.0] - 2012-04-03

### Fixed

* Function name typo `ensurePar` to `ensurePair`

### Changed

* Functions using `String` to `ByteString`

## [0.2.0] - 2011-10-26

### Added

* Language support for CoffeeScript
* Language support for JavaScript
* Language support for Python
* Language support for Ruby
* LICENSE file under the MIT license
* Recursive directory search

### Fixed

* Non-exhausive pattern compiler warning
* A case where some codes lines would be marked as comments

## 0.1.0 - 2011-09-25

### Added

* Translation of functions from Docco to Haskell function
* Implementation of function from Markdown to HTML
* Haskell comment documentation generation
* Highlighting code blocks with pygments
* Parser to separate out prose from code

[unreleased]: https://github.com/sourrust/hyakko/compare/v0.6.7...HEAD
[0.6.7]: https://github.com/sourrust/hyakko/compare/v0.6.6...v0.6.7
[0.6.6]: https://github.com/sourrust/hyakko/compare/v0.6.5...v0.6.6
[0.6.5]: https://github.com/sourrust/hyakko/compare/v0.6.4...v0.6.5
[0.6.4]: https://github.com/sourrust/hyakko/compare/v0.6.3...v0.6.4
[0.6.3]: https://github.com/sourrust/hyakko/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/sourrust/hyakko/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/sourrust/hyakko/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/sourrust/hyakko/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/sourrust/hyakko/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/sourrust/hyakko/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/sourrust/hyakko/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/sourrust/hyakko/compare/1362ad0...v0.2.0
