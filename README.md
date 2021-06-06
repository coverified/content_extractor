# Content Extractor

A content extraction microservice, that acquires urls to visit from a GraphQL API, visits it via a browser and extracts
information according to page profiles denoting css elements to take information from.

## Used Frameworks / Libraries
_(not comprehensive, but the most important ones)_

-   [Caliban Client](https://ghostdogpr.github.io/caliban/) to talk to GraphQL endpoint
-   [Scala-Scraper](https://github.com/ruippeixotog/scala-scraper) to extract information from Websites

## Things to consider
-   When querying urls to visit, we only want to visit those, that have not been visited, yet, or a specified time ago
	-   As of current implementation, it is not possible to query with filters on null / empty fields directly [Caliban#778](https://github.com/ghostdogpr/caliban/issues/778)
	-   Therefore, we expect the 'lastCrawl' information to be EPOCH, if not visited, yet
-   When an url is re-visited after some time we
	-   update the entry information without further checking, if actually something has changed
	-   reset the flag, if that entry has been tagged to false and
	-   remove All tags
