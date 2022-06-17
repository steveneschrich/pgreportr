# ChangeLog
For now, the changelog is in this file to make it easier to combine with the TODO list. These should be pushed out to github at some point.

20220222: First version of publications code was finished. Most of the code mirrors the correspond grant code with a pubs instead of grants in the name. There are a few differences between a grant and a pub; notably investigators vs. authors. Most of the fields are different, although the flags and annotations are relatively consistent. 

The import commands have been significantly rewritten to support grants and publications, both from the redcap server directly using the REDCapR library. The entry points now require uri/token vs. filenames. `import_grants.R` contains the grant-specific import code, `import_publications.R` has the publication code and `import.R` has general-purpose code for importing. (TODO: import.R should probably be import_generic.R or the like). 

The import code now utilizes a new programming convention that I am using. Internal functions (not exported and not generally useful to library users) are prefixed with a `.`. That is, `.is_grant_joint()` is an internal function not visible and generally not appropriate for outside use. This is because `is_grant_joint` has other meanings in the library as a derived flag, whereas this function actually derives the flag. (TODO: a better internal calling convention; perhaps if `import_` is the prefix for files, `.import_` can be the prefix for internal functions. In this case, `.import_is_grant_joint()` could work.)

The filter functions were also significantly rewritten (internally) to be more streamlined. I tried to implement a generic filter (e.g., `filter_between()`) that takes a variable to filter on. Then, I created light-weight `filter_grants_between()` and `filter_pubs_between()`. The rule of thumb for this should be that you call the most specific function you can (externally) since there may be some additional filtering that occurs at grant or publication level. This does happen for grants submitted, for instance. (TODO: Ensure all filter functions have a generic and specifics for each type. NB: this does suggest types would be useful here, eg filter_between() could be defined for grants and pubs as objects, then dispatching could determine which to call.)

The styling functions were also rewritten.
Styling
# ToDo Items

1.  Clean up of code. Generally, it seems to work well (for functional programming) to stick with verbs. As the library has matured, I've migrated more towards a couple of key verbs: is, format, filter, count. Count should probably be summarize, and perhaps there are other verb mappings that make more sense. I should refactor to create standard verb_target_condition type structure.

2.  In further functional style, I should also try and keep functions limited to single things. However, given the complexity of these queries, having more complex functions that simply call simpler functions may be the best approach.

3.  Documentation. Good libraries have extensive documentation. I need to ensure there is plenty of information for others to follow.

4.  Test cases. Unfortunately, to date most of the work has been done without real testing. This is ok for development mode, but I need to ensure that normal functions work as expected, as well as that they handle edge cases appropriately.

5. Data dictionary approach. There needs to be really good documentation about what all the variables in the table actually mean. Not just the question asked in the survey (if there is one) but also any further details on derivation, etc. This is not obvious, since I'm not sure tibbles provide this mechanism.

6. The NIH Reporter has an API: https://api.reporter.nih.gov/ that could be incorporated into the library (or as a separate library).

7. Incorporation of `Date Not Funded` field. When summarizing grant activity,
we typically count the number of grants submitted during a time range. Similarly,
the number of grants funded are the number whose grant start date is within the
time range. However, currently the final disposition of a grant is either
Funded (with a start date) or Not Funded (with no associated date). Since a
`Date Not Funded` is captured in the manual annotated, the proposal is to
create a new derived field `Date Not Funded` that is 6 months from the 
`Submission Date`. This inferred date (based on the `Grant Status` field) can
then be used for reporting of grants that are not funded within a given
time range.
