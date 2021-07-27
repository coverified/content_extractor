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

## Configuration
The following configuration parameters are available.
Some are optional and default to the given value.
Most of them can either be set by CLI argument or Environment variable.
However, either all are given as CLI argument or Environment variable.

| Description | Java CLI Argument | Environment variable | Default value |
| ----------- | ----------------- | -------------------- | ------------- |
| Url of GraphQL API | `apiUrl` | `API_URL` | --- |
| Auth secret for GraphQL API | `authSecret` | `AUTH_SECRET` | --- |
| Path to directory, where page profiles are stored | `pageProfileDirectoryPath` | `PAGE_PROFILE_DIRECTORY_PATH` | --- |
| Minimum waiting time, until a url is re-visited and re-analyzed. Given in hours. | `reAnalysisInterval` | `RE_ANALYSIS_INTERVAL` | 48 hrs |
| Amount of parallel url workers | `workerPoolSize` | `WORKER_POOL_SIZE` | 100
| Delay when rescheduling a given url, if a host's rate limit is violated. Given in seconds. | `repeatDelay` | `REPEAT_DELAY` | 1s |
| User agent information to be sent when visiting website | `userAgent` | `USER_AGENT` | "CoVerifiedBot-Extractor" |
| Time out when browsing a web site. Given in seconds. | `browseTimeout` | `BROWSE_TIMEOUT` | 60s |
| Date time pattern, in which date time information shall be transmitted to GraphQL API. | `targetDateTimePattern` | `TARGET_DATE_TIME_PATTERN` | "yyyy-MM-dd'T'HH:mm:ssXXX" |
| Time zone, in which date time information shall be transmitted to GraphQL API. | `targetTimeZone` | `TARGET_TIME_ZONE` | "UTC" |

Per host, a rate limit built by `workerPoolSize` / `repeatDelay` is respected.

Additionally, the environment variable `SENTRY_DSN`, giving the Data Source Name to use for Sentry integration, has to be set.
An empty String disables it, e.g., for local execution.

## Dockerfile build arguments
-   `PROJECT_NAME` - Name of the project (to assemble name of compiled jar file)
-   `PROJECT_VERSION` - Version of the project (to assemble name of compiled jar file)
-   `MAIN_CLASS` - Main class to run the process
-   `PAGE_PROFILE_SOURCE_DIRECTORY` - Source directory on building host system, where the page profile configurations to consider are located (currently `input/production/pageProfiles`)
-   `PAGE_PROFILE_PATH` - Target path within image, where the page profiles might get copied to
