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

[0.5.0]: https://github.com/sourrust/hyakko/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/sourrust/hyakko/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/sourrust/hyakko/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/sourrust/hyakko/compare/1362ad0...v0.2.0
