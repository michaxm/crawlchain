Crawlchain
==========

Library for simulating user crawl paths (trees) with selectors. Takes a starting point and a chain of actions to go from there and returns crawling results from there.

Features
--------

Initially planned functionality:

- Provide an entry point (URL) for crawling. (DONE)
- Define (link) filters for limiting where to go from there. (DONE)
- Define a sorting order for prioritizing some links over others. (postponed)
- Take resulting links as input for following craws steps. (DONE)
- Support forms, doing post requests with provided default parameters. (DONE)
- Supplying additional parameters to forms (or cookies) should be easy,
  but is not in scope at the moment. (DONE - somewhat)

Finalizing:
- Testing of generic examples - as of now testing is only done in programs using ist (postponed)
- Branching really working - lazy deep search for first successful hit (DONE)
- Downloading of binary files. (DONE - but very primitive, in memory instead of streaming, external download favored)

For productive integration:
- Detecting broken links for download and skipping these branches/leaves (SKIPPED: responsibility of caller, not yet implemented there)
- Sample-Template for ixquick search (DONE, yahoo due to missing https support of used libs)

Supporting more real world problems:
- Adding a web search template (used yahoo because it supports non-https, see below) (DONE)
- Restructured to be able to direct to that search after the initial action (FallbackDirective) (DONE)
- Got scared to break stuff and of checking that manually, added nice testing support for complete crawlchains.(DONE)
- Integration of a web search fallback into a working (test) example (DONE)
- Add a "check exists" directive to filter unusable end results (TODO)
- Add a blacklist for end results to allow clients to choose other than undesirable known results and continue crawling (TODO)

Future (maybe)
--------------
- Restructure as a seperate library (DONE)
- Support for https (switch from http to http-conduit or maybe wreq)

Scope
-----
- Task and failure management: not in scope done by caller
