### v0.3.0-1

 * I removed most of the functionality from the package. I think only `stale_package_check()` and `get_age()` have gotten any real downstream usage aside from my own; the few functions I kept are those that I actually continue to use myself with any regularity, or which I think it's worth having written down to save re-inventing the wheel for functions only needed rarely.

   I tried looking around GitHub for existing users, but if this affects you, please don't hesitate to file a FR and I can restore anything deleted too hastily.

 * `stale_package_check()` is also deprecated. Prefer using `lintr::unused_import_linter()` instead. {lintr} will have better maintenance, reliability, and correctness.

 * `get_age()` got some substantial improvements.
   + Gives the right age in some cases, e.g. 7 1/366 years for someone born Dec. 22, 2024 as of Dec. 23, 2031 (#23). Accuracy is now confirmed for a full grid of >2 million possible birthday, age combinations.
   + Implementation is improved for about 2x speed-up. This was in service of making the implementation friendlier for static translation to other execution engines (in particular {arrow}, #18). Thanks @TPDeramus for the request and @jonkeane for consulting on acero particulars.
   + Input not required to be `Date` as long as `as.Date()` succeeds, for convenience in quick examples like `get_age('2003-02-04', '2008-12-30')`.
   + Supports recycling of one length-1 input and handles missing values in either argument.

### v0.2.2

 * Fix a minor issue where `getNamespaceExports()` returns objects in unspecified order (#13)

### v0.2.0

 * Overhaul of `stale_package_check` to use the abstract syntax tree (AST) instead of regular expressions to detect unused packages. This approach should be much more comprehensive & reliable for 99% of use cases. Please file an issue if you'd like support for any of the remaining cases.
 
 * Removed some functions that were convenient only to a younger/more inexperienced self:
   + `D` - `as.Date` wrapper, masks `stats::D`, and is not the most readable
   + `table2` - `table` wrapper, mainly I was using it for what `sort(table())` can accomplish
   + `%+%` - string concatentation. just get used to `paste`/`paste0` folks.
   + `rel_coord` - for naming coordinates relatively (e.g. 20% of the way on the x-axis). was a bit simplistic & not too hard to just do manually (I rarely use it). Better would be something like `ggrepel` which is what this was really trying to do.
 
 * Structured the repo more like a "modern" R package -- added `tests` (via `testthat`), CI (via `GitHub Actions`), and this `NEWS.md` file!

### [No `NEWS` was tracked prior to 0.2.0... sorry!]
