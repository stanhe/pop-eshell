# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog, adapted for the current history of this repository.

## [1.0.0] - 2018-11-18

Initial public release of `pop-eshell-mode`.

### Added

- Bottom popup `eshell` workflow similar to terminal drawers in modern IDEs.
- Fullscreen terminal workflow with fast switching back to the previous buffer.
- Project root detection through configurable parent markers.
- Multi-window support.
- Prefix-argument support for opening the fullscreen terminal in the current file directory.

### Changed

- Refined internal methods and documentation during the first release cycle.

### Fixed

- Fullscreen terminal switching behavior in multi-window setups.
- Input initialization behavior when creating `eshell` buffers.
