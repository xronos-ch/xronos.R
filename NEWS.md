# xronos (development version)

# xronos 0.1.1

* Updated documentation to clarify that <https://xronos.ch> now uses ISO two-character codes for countries.
* chron_data() now uses [countrycode](https://vincentarelbundock.github.io/countrycode/) to attempt to interpret `country` arguments that are not country codes.
* Fixed a bug where nested responses from the XRONOS API (e.g. 'typochronological units') could not be collated into a table #5
* Allowed for specifying an alternative API URL endpoint in xronos_request() (primarily for development & testing purposes)

# xronos 0.1.0

Initial release, supporting basic interaction with the XRONOS API v1 <https://xronos.ch/api/v1/data>.
