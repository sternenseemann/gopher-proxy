# Revision history for gopher-proxy

## 0.1.1.3 -- 2021-10-25

* Support (and require) `attoparsec >= 0.14`
* Fix parsing of the `--default-mime-type` flag: Previoulsy the
  `Read` instance was used and thus required the user to surround
  the mime type with (extra) quotes.

## 0.1.1.1 -- 2017-01-06

* Add missing documentation for `--title`

## 0.1.1.0 -- 2017-01-06

* Add two options: `--title` and `--server-name`
* Fix the README on hackage
* Elaborate the package description

## 0.1.0.2  -- 2017-01-05

* Fixed a build issue

## 0.1.0.1  -- 2017-01-04

* Updated package metadata

## 0.1.0.0  -- 2017-01-04

* First version. Released on an unsuspecting world.
