# Content Extractor

A content extraction microservice, that acquires urls to visit from a GraphQL API, visits it via a browser and extracts
information according to page profiles denoting css elements to take information from.

## Used Frameworks / Libraries
_(not comprehensive, but the most important ones)_

-   [Caliban Client](https://ghostdogpr.github.io/caliban/) to talk to GraphQL endpoint
-   [Scala-Scraper](https://github.com/ruippeixotog/scala-scraper) to extract information from Websites
-   [Sentry](https://sentry.io/welcome/) (error reporting)

## Things to consider
-   When querying urls to visit, we only want to visit those, that have not been visited, yet, or a specified time ago
	-   As of current implementation, it is not possible to query with filters on null / empty fields directly [Caliban#778](https://github.com/ghostdogpr/caliban/issues/778)
	-   Therefore, we expect the 'lastCrawl' information to be EPOCH, if not visited, yet
	-   Urls are not queried in total, but in chunks
	-   Between two successive evaluations, there is a pre-defined delay
-   When an url is re-visited after some time we
	-   update the entry information without further checking, if actually something has changed
	-   reset the flag, if that entry has been tagged to false and
	-   remove All tags

## Environment variables to be set
-   `EXTRACTOR_API_URL` - The url, where GraphQL queries can be posted to
-   `AUTH_SECRET` - Secret token to authenticate against GraphQL-API
-   `EXTRACTOR_PAGE_PROFILE_PATH` - Path, where to find the page profiles
-   `RE_ANALYSIS_INTERVAL` - Amount of hours, after which sites may be re-analysed (default value: 48 hrs)
-   `SENTRY_DSN` - Data Source Name to use for Sentry integration (empty String disables it, e.g., for local execution)
-   `EXTRACTOR_CHUNK_SIZE` - Amount of urls, that should get queried at once (default value: 1,000)
-   `EXTRACTOR_REPEAT_DELAY` - Delay between processing two chunks (default value: 900s)

## Dockerfile build arguments
-   `PROJECT_NAME` - Name of the project (to assemble name of compiled jar file)
-   `PROJECT_VERSION` - Version of the project (to assemble name of compiled jar file)
-   `MAIN_CLASS` - Main class to run the process
-   `PAGE_PROFILE_SOURCE_DIRECTORY` - Source directory on building host system, where the page profile configurations to consider are located (currently `input/production/pageProfiles`)
-   `PAGE_PROFILE_PATH` - Target path within image, where the page profiles might get copied to
