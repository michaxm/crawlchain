Crawlchain
==========

Library for simulating user crawl paths (trees) with selectors. Takes a starting point and a chain of actions to go from there and returns crawling results from there.

Implemented Features
--------------------

Initially planned functionality:
- Provide an entry point (URL) for crawling. (DONE)
- Define (link) filters for limiting where to go from there. (DONE)
- Define a sorting order for prioritizing some links over others. (REJECTED, but client blacklisting possible)
- Take resulting links as input for following craws steps. (DONE)
- Support forms, doing post requests with provided default parameters. (DONE)
- Supplying additional parameters to forms (or cookies) (DONE - for forms including headers, but not cookies which is not planned for now)

Finalizing:
- Branching really working - lazy deep search for first successful hit (DONE)
- Downloading of binary files. (DONE - but very primitive, in memory instead of streaming, external download favored)

For productive integration:
- Sample-Template for ixquick search (DONE, yahoo due to missing https support of used libs)
- Adding a web search template (used yahoo because it supports non-https, see below) (DONE)
- Restructured to be able to direct to that search after the initial action (FallbackDirective) (DONE)
- Got scared to break stuff and of checking that manually, added nice testing support for complete crawlchains. (DONE)
- Integration of a web search fallback into a working (test) example (DONE)

Roadmap (next steps)
--------------------
- Lazyness of crawling broke somehow(?). Fix, add tests. (FIXME)
- Testing of generic examples - as of now testing is only done in programs using ist (postponed)

Future plans (maybe)
--------------------
- Support for https (switch from http to http-conduit or maybe wreq)
- Implement a DSL for supplying crawling templates as text file - more experience with new templates needed first.

Scope (never)
-------------
- Task and failure management / validating end crawl result: not in scope, responsibility of caller
- Blacklisting for end results can be done by caller, not implemented in the library
