# pgreportr
An R library for NIH Program Grant Reporting

Program grants from the NIH typically involve a reporting component, to show
the return on investment associated with the grant. This can involve number of other grants
submitted, papers published and other outcomes. This library is one component of a strategy
called PROGRAM, to support this reporting. REDCap data entry is paired with an R library for
reporting purposes. This library provides standard reports using officedown, generating both
Word and Excel output. Many helper functions exist to create flags and filter on these flags
in order to simplify ad-hoc reporting.

**Note**: This library is currently under construction and based on an older library called
`u54reportr` which focused exclusively on NCI U54 PACHE reporting. Therefore, general-purpose
functionality may not yet be implemented.

