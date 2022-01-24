# ToDo Items

1.  Currently the library maintains this weird state having both a grants table and an investigator table. It would make much more sense to embed the investigator table within the grants table so that we can use dplyr on the grants object and just pass one object to various functions. The tidyr nest() and unnest() functions allow this type of activity pretty easily (when paired with group_by). I should port the code to this format, which involves primarily data loading and data formatting.

2.  Clean up of code. Generally, it seems to work well (for functional programming) to stick with verbs. As the library has matured, I've migrated more towards a couple of key verbs: is, format, filter, count. Count should probably be summarize, and perhaps there are other verb mappings that make more sense. I should refactor to create standard verb_target_condition type structure.

3.  In further functional style, I should also try and keep functions limited to single things. However, given the complexity of these queries, having more complex functions that simply call simpler functions may be the best approach.

4.  Documentation. Good libraries have extensive documentation. I need to ensure there is plenty of information for others to follow.

5.  Test cases. Unfortunately, to date most of the work has been done without real testing. This is ok for development mode, but I need to ensure that normal functions work as expected, as well as that they handle edge cases appropriately.

6. Data dictionary approach. There needs to be really good documentation about what all the variables in the table actually mean. Not just the question asked in the survey (if there is one) but also any further details on derivation, etc. This is not obvious, since I'm not sure tibbles provide this mechanism.

7. The NIH Reporter has an API: https://api.reporter.nih.gov/ that could be incorporated into the library (or as a separate library).
