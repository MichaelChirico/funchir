### v0.2.0

 * Overhaul of `stale_package_check` to use the abstract syntax tree (AST) instead of regular expressions to detect unused packages. This approach should be much more comprehensive & reliable for 99% of use cases. Please file an issue if you'd like support for any of the remaining cases.
 
 * Removed some functions that were convenient only to a younger/more inexperienced self:
   + `D` - `as.Date` wrapper, masks `stats::D`, and is not the most readable
   + `table2` - `table` wrapper, mainly I was using it for what `sort(table())` can accomplish
   + `%+%` - string concatentation. just get used to `paste`/`paste0` folks.
   + `rel_coord` - for naming coordinates relatively (e.g. 20% of the way on the x-axis). was a bit simplistic & not too hard to just do manually (I rarely use it). Better would be something like `ggrepel` which is what this was really trying to do.
 
 * Structured the repo more like a "modern" R package -- added `tests` (via `testthat`), CI (via `GitHub Actions`), and this `NEWS.md` file!

### [No `NEWS` was tracked prior to 0.2.0... sorry!]
