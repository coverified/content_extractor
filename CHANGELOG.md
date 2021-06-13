# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
-   Build config first from environment variables and if that doesn't work from CLI#
-   Query GraphQL-API for all those urls, that haven't been crawled yet or a predefined time period ago
-   Determine, if a matching entry is missing or an existing needs an update
-   Create or update an entry for the url
-   Build a docker container and copy page profile configurations to consider right into the container
