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
