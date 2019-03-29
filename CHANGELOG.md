# Changelog

## v0.3.0

### Added

- Support for TLS.
- Backends are optional and determined by the keys in the `backend` section of
  the configuration file.
- Archive backend that writes log items to a file specified in the configuration.

### Changed

- The key `jira` is no longer top-level in the configuration file. Moved under
  `backend`.
