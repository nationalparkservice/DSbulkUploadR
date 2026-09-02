 <!-- badges: start -->
  [![R-CMD-check](https://github.com/DOI-NPS/DSbulkUploadR/actions/workflows/R-CMD-check.yaml/badge.svghttps://github.com/DOI-NPS/DSbulkUploadR/actions/workflows/R-CMD-check.yaml/badge.svghttps://github.com/DOI-NPS/DSbulkUploadR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DOI-NPS/DSbulkUploadR/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# DSbulkUploadR
The DSbulkUploadR (DataStore Bulk Uploader) is designed to help efficiently transfer data to DataStore, create the appropriate references, and automate populating them to the greatest extent possible.

# Consult with the IMD CSO Data Science Team before using!
Because DSbulkUploadR automates DataStore reference creation and file uploads, there is a very real possibility that using it could have unintended consequences. For instance, you could inadvertently create hundreds or thousands of draft references that need to be cleaned up. You really don't want to make mistakes using this package. Its designed to be easy to use - perhaps too easy! 

Please consult with the NPS Inventory and Monitoring Central Support Office Data Science Team (currently [Judd Patterson](mailto:judd_patterson@nps.gov), [Rob Baker](mailto:robert_baker@nps.gov), and [Wendy Thorsdatter](mailto:Wendy_Thorsdatter@nps.gov) before using this tool. We promise we can help make your life easier! (And you just might make our lives easier too).

# Detailed Instructions
A set of step-by-step detailed instructions on how to use the bulk uploader, what file types and reference types are supported, etc. is available via the [articles](https://DOI-nps.github.io/DSbulkUploadR/articles/index.html) included on the package website.

# Currently supported DataStore reference types
* Audio Recording
* Generic Document
* Web Site
* Generic Dataset
* Field Notes
* Script
* Project

("Field Notes" is not a valid reference type on DataStore. Information supplied to DSbulkUploadR under "FieldNotes" will be treated as a GenericDocument with the exception that the keyword "FieldNotes" will be added to the GenericDocument created on DataStore to aid in subsequent triage)
