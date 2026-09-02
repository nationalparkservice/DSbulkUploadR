# Helper functions ----

#' Checks the reference types in a file are valid Data Store reference types
#'
#' Reads in a .xlsx file for data validation. Checks the values supplied in the column reference_type using the DataStore API to determine whether they are valid reference types, as stored in the DataStore backend. Note that the Reference Type listed on the DataStore web page (e.g. Audio Recording) is NOT the correct, valid reference type. In this case the valid reference type would be AudioRecording. If any reference type is invalid, the function throws an error and prints a list of the invalid reference types. If all reference types are valid, the function passes.
#'
#' @param filename String. Input file to check. Defaults to "DSbulkdUploadR_input.xlsx"
#' @param path String. Path to the file. Defaults to the current working directory.
#' @param sheet_name String. Name of the excel sheet to check.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_ref_type(sheet_name = "GenericDataset")}
check_ref_type <- function(path = getwd(),
                           filename = "DSbulkUploadR_input.xlsx",
                           sheet_name) {

  #upload_data <- read.delim(file=paste0(path, "/", filename))
  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  refs <- upload_data$reference_type

  url <- paste0(.ds_secure_api(), "FixedList/ReferenceTypes")
  #API call to look for an existing reference:
  req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(req)$status_code

  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("DataStore connection failed.")
  }

  refs_json <- httr::content(req, "text")
  refs_rjson <- jsonlite::fromJSON(refs_json)

  refs <- unique(refs)
  #treat "FieldNotes" as "GenericDocuments". FieldNotes is a DSbulkUploadR
  #only field and does not exist in DataStore. They will be uploaded as
  #GenericDocument  with a keyword "Field notes" added for later triage.
  refs <- stringr::str_replace(refs, "FieldNotes", "GenericDocument")

  bad_refs <- refs[!(refs %in% refs_rjson$key)]

  if (length(bad_refs > 0)) {
    msg <- paste0("Please use valid reference types. The following ",
                  "reference types are invalid: {bad_refs}.")
    cli::cli_abort(c("x" = msg))
  }
  cli::cli_inform(c("v" = "All reference types are valid."))
  return(invisible(NULL))
}

#' Check that Reference Types are Supported
#'
#' Runs a check against a hard coded list of currently supported DataStore reference types. If an unsupported reference type is supplied, the function will fail with an error and the error message will return the specific reference type(s) that are unsupported. If the reference types are all supported, the function will pass.  See the \href{https://nationalparkservice.github.io/DSbulkUploadR/articles/02_Generate-the-Input-File.html#create-an-input-template}{package documentation} for a list of currently supported references and information on using them.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_ref_type_supported(sheet_name = "GenericDocument")}
check_ref_type_supported <- function(path = getwd(),
                                     filename = "DSbulkUploadR_input.xlsx",
                                     sheet_name) {

  #upload_data <- read.delim(file=paste0(path, "/", filename))
  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  refs <- upload_data$reference_type

  # hardcoded list of supported refs. Will need manual updates
  supported_refs <- c("AudioRecording",
                      "GenericDocument",
                      #FieldNotes is not a real reference type on DataStore
                      #It is only here for user convenience at at request of
                      #"managers". It will be treated like GenericDocument"
                      #but will have "Field notes" added as a keyword
                      #for later triage. Good luck, later triaging people.
                      "FieldNotes",
                      "WebSite",
                      "GenericDataset",
                      "Project",
                      "Script")

  bad_refs <- refs[(!refs %in% supported_refs)]
  bad_refs <- unique(bad_refs)

  if (length(bad_refs > 0)) {
    msg <- paste0("The following reference types are unsupported: ",
                  "{bad_refs}. Please supply only currently supported ",
                  "reference types.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All reference types supplied are supported.")
    cli::cli_inform(c("v" = msg))
  }
}


#' Check that all reference types are identical
#'
#' Runs a check against all values supplied in the reference_type column of the input file. If all values in this column are identical, the test passes. If any are not identical, the test fails.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{check_refs_identical(sheet_name = "Project")}
check_refs_identical <- function (path = getwd(),
                                  filename = "DSbulkUploadR_input.xlsx",
                                  sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  if (length(seq_along(unique(upload_data$reference_type))) != 1) {
    msg <- paste0("For each upload all reference types must be the same. ",
                  "Please check that you have only one reference type in ",
                  "your input file.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("Only one reference type is listed for this upload.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks that each file_path in a file contains files
#'
#' Reads in a .xlsx file for data validation. For each item in the column file_path, the function checks whether the path given contains file. If the path given does not contain files (or is not a valid path), the function will throw an error and list the bad paths. If all paths contain valid files, the function passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_files_exist("test_file.xlsx")
#' }
check_files_exist <- function(path = getwd(),
                              filename = "DSbulkUploadR_input.xlsx",
                              sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  #Projects don't have files; skip/pass this test.
  #not sure if this is how all_of works but if so, slick.
  if(all(upload_data$reference_type == "Project")) {
    msg <- paste0("All references of are type \"Project\" which does not ",
                  "require files to upload.")
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  bad_files <- NULL

  for (i in 1:nrow(upload_data)) {
    path_to_file <- upload_data$file_path[i]
    if(is.na(path_to_file)) {
      msg <- paste0("All references must have files to upload. ",
                    "The following references have a bad file path ",
                    "or no files associated with them:\n {bad_files}")
      bad_files <- append(bad_files, upload_data$title[i])
    }
    if (!is.na(path_to_file)) {
      if (!file.exists(path_to_file)) {
        bad_files <- append(bad_files, upload_data$title[i])
      }
    }
  }
  if (!is.null(bad_files)) {
    msg <- paste0("All references must have files to upload. ",
                  "The following references have a bad file path ",
                  "or no files associated with them:\n {bad_files}")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All file paths contain file for upload.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks whether the total number of file to upload exceeds a threshold
#'
#' Reads in a .xlsx file for data validation. Checks the number of files located in each directory supplied via the column file_path. If the total number of files exceeds the file_number_error value, the function produces an error. If the total number of files exceeds 50% of the file_number_error value the function produces a warning. Otherwise the function passes.
#'
#' @inheritParams check_ref_type
#' @param file_number_error Integer. Maximum allowable number of files. Defaults to 500.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_file_number("test_file.xlsx")
#' }
check_file_number <- function(filename = "DSbulkUploadR_input.xlsx",
                              path = getwd(),
                              sheet_name,
                              file_number_error = 500) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  if(all(upload_data$reference_type == "Project")) {
    msg <- paste0("All references of are type \"Project\" which does not ",
                  "require a file number test.")
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  file_num <- 0
  for (i in 1:nrow(upload_data)) {
    files_per_ref <- length(list.files(upload_data$file_path[i]))
    file_num <- (file_num + files_per_ref)
  }
  if (file_num > file_number_error) {
    msg <- paste0("You are attempting to upload {file_num} files, ",
                  "which exceeds the maximum allowable number, ",
                  "({file_number_error} files). Please either adjust ",
                  "the maximum number of files or attempt to upload ",
                  "fewer files.")
    cli::cli_abort(c("x" = msg))
  } else if (file_num > (file_number_error * 0.5)) {
    msg <- paste0("You are attempting to upload {file_num} files, ",
                  "which has triggered a warning. Please make sure you ",
                  "want to upload this many files.")
    cli::cli_warn(c("!" = msg))
  } else {
    msg <- paste0("The files to upload does not exceed the maximum number.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks whether the total file size to upload exceeds some threshold
#'
#'  Reads in a .xlsx file for data validation. Checks the file size of files located in each directory supplied via the column file_path. If the total file size exceeds the file_size_error value, the function produces an error. If the total number of files exceeds 50% of the file_number_error value the function produces a warning. Otherwise the function passes.
#'
#' @inheritParams check_ref_type
#' @param file_size_error Integer. The maximum allowable total file size in GB. Defaults to 100 GB.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_file_size("test_file.xlsx")
#' }
check_file_size <- function(filename = "DSbulkUploadR_input.xlsx",
                            path = getwd(),
                            sheet_name,
                            file_size_error = 100) {
  #convert gb to bytes:
  error_bytes <- file_size_error * 1073741824
  warn_bytes <- error_bytes * 0.5
  #get input data to validate:
  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  if(all(upload_data$reference_type == "Project")) {
    msg <- paste0("All references of are type \"Project\" which does not ",
                  "require a file size test.")
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  file_size <- 0
  for (i in 1:nrow(upload_data)) {
    file_size <- file_size +
      sum(file.info(list.files(upload_data$file_path[i],
                               full.names = TRUE))$size)
  }
  file_gb <- file_size/1073741824
  if (file_size > error_bytes) {
    msg <- paste0("You are attempting to upload {file_gb} GB of data, ",
                  "which exceeds the maximum allowable number, ",
                  "({file_size_error} GB) per bulk upload. Please either ",
                  "adjust the maximum data upload amount, or attempt to ",
                  "upload less data.")
    cli::cli_abort(c("x" = msg))
  } else if (file_size > warn_bytes) {
    msg <- paste0("You are attempting to upload {file_gb} GB of data, ",
                  "which has triggered a warning. Please make sure you ",
                  "want to upload this much data.")
    cli::cli_inform(c("!" = msg))
  } else
    msg <- paste0("The cumulative file size to upload does ",
                  "not exeed the maximum allowable.")
  cli::cli_inform(c("v" = msg))
  return(invisible(NULL))
}

#' Checks a column within a supplied file to ascertain whether 508 status has been supplied.
#'
#' Reads in a .xlsx file for data validation. Checks the column files_508_compliant to make sure that all values are either 'yes' or 'no', which indicates the 508 compliant status of any associated files to be uploaded to DataStore. If any values are missing or are not 'yes' or 'no' (where case is ignored), the function throws an error. If all values have either a 'yes' or 'no' value associated with them, the function passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_508_format("test_file.xlsx")}
check_508_format <- function(path = getwd(),
                             filename = "DSbulkUploadR_input.xlsx",
                             sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  if(all(upload_data$reference_type == "Project")) {
    msg <- paste0("All references of are type \"Project\" which does not ",
                  "require a 508 compliance test.")
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  is_508 <- tolower(upload_data$files_508_compliant) != "yes" &
    tolower(upload_data$files_508_compliant) != "no"
  if (sum(is_508) > 0) {
    msg <- paste0('All files must have a 508 compiance status of "yes" ',
                  'or "no". Please provide valid 508 compliance for ',
                  'each reference.')
    cli::cli_abort(c("x" = msg))
  } else {
    cli::cli_inform(
      c("v" = "All files have a valid 508 compliance designation"))
  }
  return(invisible(NULL))
}

#' Checks for duplicated reference titles
#'
#' Reads in a .xlsx file for data validation. Checks the column title to make sure all values are unique. If any values are duplicated, the function throws an error and returns a list of the duplicated values. If all the values are unique, the function passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_unique_title("test_file.xlsx")}
check_unique_title <- function(path = getwd(),
                              filename = "DSbulkUploadR_input.xlsx",
                              sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  duplicates <- duplicated(upload_data$title)
  if (any(duplicates)) {
    msg <- paste0("All reference titles must be unique. The following titles ",
                  "are duplicated: {upload_data$title[duplicates]}.")
    cli::cli_abort(c("x" = msg))
  } else {
    cli::cli_inform(c("v" = "All reference titles are unique"))
  }
  return(invisible(NULL))
}

#' Checks the begin_content_date column of an input file for ISO 8601 formatting
#'
#' Reads in a .xlsx file for data validation. Throws an error if any dates in the content_begin_date column are not in ISO 8601 (yyyy-mm-dd). Passes if all dates in the content_begin_date column are in ISO 8601 format.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_start_date <- ("test_file.xlsx")}
check_start_date <- function(path = getwd(),
                            filename = "DSbulkUploadR_input.xlsx",
                            sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  start_dates <- suppressWarnings(upload_data$content_begin_date)
  if (is.null(start_dates)) {
    msg <- paste0('All references of are type "', sheet_name, '" which does ',
                  'not require a content start date.')
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }
  #valid dates are "TRUE"
  valid_dates <- !is.na(suppressWarnings(lubridate::ymd(start_dates)))
  if (sum(valid_dates) < nrow(upload_data)) {
    msg <- paste0("Some content begin dates are not in ISO 8601 format ",
                  "(yyyy-mm-dd). Please supply all dates in ISO 8601 format.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content begin dates are supplied ",
                  "in the correct format.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks the end_content_date column of an input file for ISO 8601 formatting
#'
#' Reads in a .xlsx file for data validation. Throws an error if any dates in the content_end_date column are not in ISO 8601 (yyyy-mm-dd). Passes if all dates in the content_begin_date column are in ISO 8601 format.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_end_date("test_file.xlsx")
#' }
check_end_date <- function(path = getwd(),
                          filename = "DSbulkUploadR_input.xlsx",
                          sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  end_dates <- upload_data$content_end_date

  # when there are no dates at all, this is OK
  if (is.null(end_dates)) {
    msg <- paste0('All references of are type "', sheet_name, '" which does ',
                  'not require a content end date.')
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }
  # when there are some but not all end dates:
  if (sum(is.na(end_dates)) > 0) {
    ref_type <- upload_data$reference_type[1]
    if (ref_type == "Project") {
      msg <- paste0('Not all references have content end dates.')
      cli::cli_warn(c("!" = msg))
    } else {
      msg <- paste0("Some content end dates are missing. Please supply ",
                    "content end dates for all references in ISO-8601 ",
                    "format.")
      cli::cli_abort(c("x" = msg))
    }
  }
  #remove NA values
  end_dates <- end_dates[!is.na(end_dates)]
  #check remaining values for valid formatting:
  valid_dates <- !is.na(suppressWarnings(lubridate::ymd(end_dates)))
  if (sum(valid_dates) < length(seq_along(end_dates))) {
    msg <- paste0("Some content end dates are not in ISO 8601 format ",
                  "(yyyy-mm-dd). Please supply all dates in ISO 8601 format.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content end dates are supplied ",
                  "in the correct format.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks that content_end_dates occur after content_begin_dates in a file.
#'
#' Reads in a .xlsx file for data validation. Throws an error if any dates in the content_end_date column occur before the date in the same row for the content_begin_date column. Passes if all end dates occur on or after the being date.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_end_after_start("test_file.xlsx")
#' }
check_end_after_start <- function(path = getwd(),
                                  filename = "DSbulkUploadR_input.xlsx",
                                  sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  end_dates <- suppressWarnings(lubridate::ymd(upload_data$content_end_date))
  start_dates <- suppressWarnings(lubridate::ymd(upload_data$content_begin_date))

  if (length(end_dates) == 0 | length(start_dates) == 0) {
    msg1 <- paste0('All references of are type "', sheet_name, '" which does ',
                  'not require a content start date or a content end date.')
    cli::cli_inform(c("v" = msg1))
    return(invisible(NULL))
  }

  time_dif <- lubridate::interval(start_dates, end_dates)
  time_dif <- time_dif[!is.na(time_dif)]
  if (any(time_dif < 0)) {
    msg <- paste0("Some content end dates predate content start dates. ",
                  "Please make sure end dates are after start dates.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All end dates occur after start dates.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks that dates in a file occurred in the past.
#'
#' Reads in a .xlsx file for data validation. Throws an error if dates in either the content_begin_date or content_end_date occur after the current system date. If any dates occur in the future the function results in an error. The test passes if all dates occur in the past (or on the present day).
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_dates_past("test_file.xlsx")
#' }
check_dates_past <- function(path = getwd(),
                            filename = "DSbulkUploadR_input.xlsx",
                            sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)


  start_dates <- suppressWarnings(lubridate::ymd(upload_data$content_begin_date))
  if (length(start_dates) == 0) {
    msg1 <- paste0('All references of are type "', sheet_name, '" which does ',
                   'not require a content start date.')
    cli::cli_inform(c("v" = msg1))
    return(invisible(NULL))
  }

  today <- Sys.Date()
  check_start <- lubridate::interval(start_dates, today)

  # Projects don't have end dates:
  if(all(upload_data$reference_type == "Project")) {
    if(any(check_start < 0)) {
      msg <- paste0("Some of your content start or dates occur in the ",
                    "future. Please make sure all dates occur in the past.")
      cli::cli_abort(c("x" = msg))
    } else {
      msg <- paste0("All content start dates occur in the past.")
      cli::cli_inform(c("v" = msg))
    }
    return(invisible(NULL))
  }

  end_dates <- lubridate::ymd(upload_data$content_end_date)
  check_end <- lubridate::interval(end_dates, today)

  if (any(check_start < 0) | any(check_end < 0)) {
    msg <- paste0("Some of your content start or end dates occur in the ",
                  "future. Please make sure all dates occur in the past.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content start and end dates occur in the past.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Verifies author emails supplied in file.
#'
#' Reads in a .xlsx file for data validation. Checks that all author emails supplied in the column author_email_list are valid NPS emails. Each instance of author emails contain a single email or a comma separated list of emails (e.g. just joe.smith@nps.gov or joe.smith@nps.gov, jane.doe@partner.nps.gov). If any email is not a valid NPS email, the function will throw an error and list the invalid emails (check for typos!). If all emails supplied are valid NPS emails, the function will pass. This function uses an API rather than Active Directory to access lists of valid NPS emails. If the reference type is a Project, this function will test the leads_email_list column instead of the author_email_list column.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_author_email("test_file.xlsx")}
check_author_email <- function(path = getwd(),
                              filename = "DSbulkUploadR_input.xlsx",
                              sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  #skip test for WebSite reference type which does not have authors
  if (any(upload_data$reference_type == "WebSite")) {
    msg <- "Refernce type WebSite does not require authors"
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  # Projects use "Leads" instead of authors:
  usr_email_list <- NULL
  if (any(upload_data$reference_type == "Project")) {
    usr_email_list <- upload_data$leads_email_list
  } else {
    usr_email_list <- upload_data$author_email_list
  }

  usr_email <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_email <-
      append(usr_email,
             unlist(stringr::str_split(usr_email_list[i], ",")))
  }
  usr_email <- stringr::str_trim(usr_email)
  usr_email <- unique(usr_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  bdy <- usr_email
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    cli::cli_abort(c("x" = "ERROR: Active Directory connection failed."))
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  if (all(rjson$found)) {
    cli::cli_inform(c("v" = "All author emails are valid."))
  } else {
    bad_usr_emails <- dplyr::filter(rjson, found==FALSE)$searchTerm
    msg <- paste0("Please supply valid emails. The following emails ",
                  "are invalid: {bad_usr_emails}.")
    cli::cli_abort(c("x" = msg))
  }
  return(invisible(NULL))
}

#checks to make sure all users have orcids and that orcids are correctly formatted. Does not check whether orcids are valid/associated with the correct username.
#' Checks to make sure all author email accounts have ORCiDs
#'
#' Reads in a .xlsx file for data validation. Checks all the emails listed in the column author_email_list and verifies whether these emails have ORCiDs associated with the via an NPS API. For the Project reference type, the function checks the leads_email_list instead of author_email_list. If any user account associated with an email does not have an ORCiD associated with it (including all emails that are not valid NPS emails), the function throws an error and lists the emails that do not have ORCiDs associated with them. If all emails are tied to valid NPS accounts with ORCiDs associated with them, the function passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_authors_orcid("test_file.xlsx")}
check_authors_orcid <- function(path = getwd(),
                               filename = "DSbulkUploadR_input.xlsx",
                               sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  #skip test for WebSite reference type which does not have authors
  if (any(upload_data$reference_type == "WebSite")) {
    msg <- "Refernce type WebSite does not require authors"
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  # Projects use "Leads" instead of authors:
  usr_email_list <- NULL
  if (any(upload_data$reference_type == "Project")) {
    usr_email_list <- upload_data$leads_email_list
  } else {
    usr_email_list <- upload_data$author_email_list
  }

  usr_email <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_email <-
      append(usr_email,
             unlist(stringr::str_split(usr_email_list[i], ",")))
  }
  usr_email <- stringr::str_trim(usr_email)
  usr_email <- unique(usr_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  bdy <- usr_email
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed.")
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  # test whether orcids exist in active directory:
  if(sum(is.na(rjson$extensionAttribute2)) == 0) {
    cli::cli_inform(c("v" = "All users have ORCiDs."))
  } else {
    no_orcid <- dplyr::filter(rjson, is.na(rjson$extensionAttribute2))$searchTerm
    msg <- paste0("All authors must have ORCiDs. Please have the ",
                  "following authors add ORCiDs to Active Directory ",
                  "prior to proceeding with reference creation: {no_orcid}.")
    cli::cli_abort(c("x" = msg))
  }
  return(invisible(NULL))
}

#' Checks the formatting of ORCiDs associated with NPS emails supplied in a file
#'
#' Reads in a .xlsx file for data validation. Uses an NPS API to check that all the emails listed in the column author_email_list have properly formatted ORCiDs (xxxx-xxxx-xxx-xxxx) associated with them. For the Project reference type, the function will check the column leads_email_list rather than author_email_list. Lack of an ORCiD, including because the supplied email address is not a valid NPS email, is considered improperly formatted for the purposes of this test. emails supplied do not have properly formatted ORCiDs associated with them the function throws an error and lists the emails that do not have properly formatted ORCiDs associated with them. If all emails are tied to valid NPS accounts with properly formatted ORCiDs associated with them, the function passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_orcid_format(path = getwd(),
#'                    filename = "DSbulkUploadR_input.xlsx",
#'                    sheet_name = "AudioRecording")}
check_orcid_format <- function(path = getwd(),
                              filename = "DSbulkUploadR_input.xlsx",
                              sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  #skip test for WebSite reference type which does not have authors
  if (any(upload_data$reference_type == "WebSite")) {
    msg <- "Refernce type WebSite does not require authors"
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  # Projects use "Leads" instead of authors:
  usr_email_list <- NULL
  if (any(upload_data$reference_type == "Project")) {
    usr_email_list <- upload_data$leads_email_list
  } else {
    usr_email_list <- upload_data$author_email_list
  }

  usr_email <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_email <-
      append(usr_email,
             unlist(stringr::str_split(usr_email_list[i], ",")))
  }
  usr_email <- stringr::str_trim(usr_email)
  usr_email <- unique(usr_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")

  bdy <- usr_email
  if (length(seq_along(bdy)) < 2) {
    bdy <- list(bdy)
  }

  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed.")
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  # test whether orcids are valid:
  rjson$orcid_format <- grepl('[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[A-Za-z0-9]{1}', rjson$extensionAttribute2)
  if (sum(rjson$orcid_format) == nrow(rjson)) {
    cli::cli_inform(c("v" = "All ORCiDs have a valid format"))
  } else {
    rjson2 <- rjson %>% dplyr::filter(rjson$orcid_format == FALSE)
    msg <- paste0("All authors must have ORCiDs with valid formats. ",
                  "The following authors have ORCiDs with invalid formats ",
                  "(which may include lack of any ORCiD) listed in Active ",
                  "Dirctory: {rjson2$searchTerm}.")
    cli::cli_abort(c("x" = msg))
  }
  return(invisible(NULL))
}


#' Checks editor emails for valid NPS emails
#'
#' Reads in the specified sheet from an .xlsx file and checks that all email addresses in the editor_email_list column are valid using the AD API.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_editor_email()}
check_editor_email <- function(path = getwd(),
                               filename = "DSbulkUploadR_input.xlsx",
                               sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  editor_email <- NULL
  for (i in 1:nrow(upload_data)) {
    editor_email <-
      append(editor_email,
             unlist(stringr::str_split(upload_data$editor_email_list[i], ",")))
  }
  editor_email <- stringr::str_trim(editor_email)
  editor_email <- unique(editor_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  bdy <- editor_email
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    cli::cli_abort(c("x" = "ERROR: Active Directory connection failed."))
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  if (all(rjson$found)) {
    cli::cli_inform(c("v" = "All editor emails are valid."))
  } else {
    bad_editor_emails <- dplyr::filter(rjson, found==FALSE)$searchTerm
    msg <- paste0("Please supply valid emails. The following emails ",
                  "are invalid: {bad_editor_emails}.")
    cli::cli_abort(c("x" = msg))
  }
  return(invisible(NULL))
}

#' Checks an file for valid license types
#'
#' Reads in a .xlsx file for data validation. Checks all values supplied in the license_code column for valid DataStore license codes. Valid codes are as follows: 1) "Creative Commons Zero v1.0 Universal (CC0)". This is the preferred license for all public content. 2) "Creative Commons Attribution 4.0 International", 3) "Creative commons Attribution Non Commercial 4.0 International" (This license may be useful when working with partners external to NPS), 4) "Public Domain", 5) "Unlicensed (not for public dissemination)" (should only be used for restricted content that contains confidential unclassified information - CUI). If all license codes supplied (as integers ranging from 1-5) are valid, the function passes. If any license code is missing or invalid, the function throws an error.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_license_type("test_file.xlsx")}
check_license_type <- function(path = getwd(),
                               filename = "DSbulkUploadR_input.xlsx",
                               sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  if (any(is.na(upload_data$license_code))) {
    msg <- paste0("You must supply a license type for all references.")
    cli::cli_abort(c("x" = msg))
  }
  good_values <- c(1,2,3,4,5)
  valid_license <- sum(!(upload_data$license_code %in% good_values))
  if (valid_license > 0) {
    msg <- paste0("You have supplied an invalid license code. ",
                  "Please supply a valid license code.")
    cli::cli_abort(c("x" = msg))
  } else {
    cli::cli_inform(c("v" = "All license codes are valid."))
  }
  return(invisible(NULL))
}

#' Check a file for valid CUI labels
#'
#' Checks the supplied CUI labels against a hard-coded set of accepted CUI markings
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_CUI_label_vald(sheet_name = "AudioRecording")}
check_CUI_label_valid <- function(path =getwd(),
                                 filename = "DSbulkUploadR_input.xlsx",
                                 sheet_name) {
  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  if (any(is.na(upload_data$CUI_label))) {
    msg <- paste0("You must supply a CUI label for all references.")
    cli::cli_abort(c("x" = msg))
  }
  good_values <- c("PUBLIC",
                   "National Park System Resources",
                   "Archeological Resources",
                   "Historical Properties",
                   "Cave Resources",
                   "Paleontological Resources",
                   "Copyrighted",
                   "Information Systems Vulnerability Information",
                   "Personnel Records",
                   "General Privacy",
                   "Non-NPS Proprietary Data",
                   "Tribal Data Sovereignty",
                   "Critical Infrastructure: General Critical Infrastructure Information",
                   "Critical Infrastructure: Emergency Management",
                   "Financial: Budget",
                   "Law Enforcement Communication",
                   "Controlled Substance",
                   "Criminal History Record Information",
                   "General Law Enforcement",
                   "Investigation",
                   "Terrorist Screening",
                   "Whistleblower Identity",
                   "Legal: Administrative Proceedings",
                   "Legal: Victim",
                   "Privacy: Health Information",
                   "General Procurement and Acquisition",
                   "Procurement and Acquisition: Source Selection",
                   "General Proprietary Business Information",
                   "Legal: Legal Privilege",
                   "Wells")
  valid_CUI_label <- sum(!(upload_data$CUI_label %in% good_values))

  if (valid_CUI_label > 0) {
    msg <- paste0("You have supplied an invalid CUI marking. ",
                  "Please supply a valid license code. ",
                  "See {.url https://nationalparkservice.github.io/DSbulkUploadR/articles/03_Populating-the-Input-File.html#cui-label} ",
                  "for a list of valid CUI labels.")
    cli::cli_abort(c("x" = msg))
  } else {
    cli::cli_inform(c("v" = "All CUI labels are valid."))
  }
  return(invisible(NULL))
}

#' Checks that license codes are matched with appropriate CUI labels
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_CUI_license_match(sheet_name = "AudioRecording")}
check_CUI_license_match <- function(path =getwd(),
                                    filename = "DSbulkUploadR_input.xlsx",
                                    sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  for (i in 1:nrow(upload_data)) {
    license_type <- upload_data$license_code[i]
    CUI_label <- upload_data$CUI_label[i]

    if (license_type == 5 && CUI_label == "PUBLIC") {
      msg <- paste0("You have selected a restricted license for public data.",
                    "Please make sure all public data have non-restricted ",
                    "licenses (e.g. license code 1-4)")
      cli::cli_abort(c("x" = msg))
    }
    if (license_type < 5 && CUI_label != "PUBLIC") {
      msg <- paste0("You have selected a public license for restricted data.",
                    "Pleae make sure all restricted data have restricted ",
                    "licenses (e.g. license code 5).")
      cli::cli_abort(c("x" = msg))
    }
  }
  msg <- paste0("All license codes are compatible with data restrictions ",
                "(if applicable).")
  cli::cli_inform(c("v" = msg))
  return(invisible(NULL))
}



#' Checks for valid producing unit codes in a file
#'
#' Reads in a .xlsx file for data validation. Uses the NPS Unit Service API to check that the producing_units column contains valid NPS unit codes. If invalid producing units are encountered, the function throws an error and prints a list of the invalid units to the console. If all units are valid, the test passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_units("test_file.xlsx")
#' }
check_prod_units <- function(path = getwd(),
                            filename = "DSbulkUploadR_input.xlsx",
                            sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)
  #get list of all units:
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    # access all park codes from NPS xml file
    curl::curl_download("https://irmaservices.nps.gov/Unit/v2/api/", f)
  }
  result <- XML::xmlParse(file = f)
  dat <- XML::xmlToDataFrame(result)

  #check for bad producing units:
  producing_units <- NULL
  for (i in 1:nrow(upload_data)) {
    producing_units <-
      append(producing_units,
             unlist(stringr::str_split(upload_data$producing_units[i], ", ")))
  }
  producing_units <- unique(producing_units)
  bad_prod_units <- producing_units[!(producing_units %in% dat$UnitCode)]

  if (length(bad_prod_units > 0)) {
    msg <- paste0("The following producing units are invalid. ",
                  "Please provide valid producing units: {bad_prod_units}.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All producing units are valid.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}

#' Checks for valid content unit codes in a file
#'
#' Reads in a .xlsx file for data validation. Uses the NPS Unit Service API to check that the content_units column contains valid NPS unit codes. If invalid content units are encountered, the function throws an error and prints a list of the invalid units to the console. If all units are valid, the test passes.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_content_units(sheet_name = "GenericDocument")}
check_content_units <- function(path = getwd(),
                               filename = "DSbulkUploadR_input.xlsx",
                               sheet_name) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  #get list of all units:
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    # access all park codes from NPS xml file
    curl::curl_download("https://irmaservices.nps.gov/Unit/v2/api/", f)
  }
  result <- XML::xmlParse(file = f, options =  XML::NOWARNING)
  dat <- XML::xmlToDataFrame(result)

  #check for bad conten units:
  content_units <- NULL
  for (i in 1:nrow(upload_data)) {
    content_units <-
      append(content_units,
             unlist(stringr::str_split(upload_data$content_units[i], ", ")))
  }
  content_units <- unique(content_units)
  bad_content_units <- content_units[!(content_units %in% dat$UnitCode)]

  if (length(bad_content_units > 0)) {
    msg <- paste0("Please provide valid content units. The following ",
                  "content units are invalid: {bad_content_units}.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content units are valid.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}


#' Checks that Project IDs are numeric
#'
#' The function checks that all project IDs are numeric (or NA). If projects are all numeric (or NA), the test passes. Otherwise it fails with a warning. Test is passes unconditionally if reference type is a Project.
#'
#' @inheritParams check_ref_type
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_projects_numeric(sheet_name = "Script")
#' }
check_projects_numeric <- function(path = getwd(),
                           filename = "DSbulkUploadR_input.xlsx",
                           sheet_name) {

  if (sheet_name == "Project") {
    msg <- "Project IDs not required for Project reference types"
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  projects <- NULL
  for (i in 1:nrow(upload_data)) {
    projects <-
      append(projects,
             unlist(stringr::str_split(upload_data$project_id[i], ", ")))
  }
  projects[projects == "NA"] <- NA

  tryCatch(
    {
    projects <- as.numeric(projects)
    },
    error = function(e) {
      msg <- "The project_id must be a numeric value or NA"
      cli::cli_abort(c("x" = msg))
    },
    warning = function(w) {
      msg <- "The project_id must be a numeric value or NA"
      cli::cli_abort(c("x" = msg))
    })
  msg <- paste0("All project IDs are numeric or NA.")
  cli::cli_inform(c("v" = msg))
  return(invisible(NULL))
}

#' Checks for valid projects
#'
#' The function Initiates an API call to datastore to make sure that a project is valid. Potential reasons for check to fail with an error include: The reference ID number does not go to a valid DataStore reference, the valid reference is not a Project type reference, the user not being on the VPN/on an NPS network, the project may not be public, the project may not be active, or the user may not have permissions to access or edit the project.This test passes unconditionally if the reference type is a Project.
#'
#' @inheritParams check_ref_type
#' @param dev Logical. Whether or not the API calls should be made to the development (TRUE) or production (FALSE) server. Defaults to TRUE.
#'
#' @returns NULL (invisibly)
#' @export
#' @examples
#' \dontrun{
#' check_projects_valid(sheet_name = "Script")
#' }
check_projects_valid <- function(path = getwd(),
                                 filename = "DSbulkUploadR_input.xlsx",
                                 sheet_name,
                                 dev = TRUE) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  if (sheet_name == "Project") {
    msg <- "Project IDs not required for Project reference types"
    cli::cli_inform(c("v" = msg))
    return(invisible(NULL))
  }

  projects <- NULL
  for (i in 1:nrow(upload_data)) {
    # handle lists of projects:
    projects <-
      append(projects,
             unlist(stringr::str_split(upload_data$project_id[i], ", ")))
  }
  #convert character NAs to <NA> NAs
  projects[projects == "NA"] <- NA

  bad_project_ids <- NULL
  for (i in 1:length(projects)) {
    if (!is.na(projects[[i]])) {
      if (dev == TRUE) {
        get_url <- paste0(.ds_dev_api(),
                          "Profile?q=",
                          projects[[i]])
        } else {
          get_url <- paste0(.ds_secure_api(),
                            "Profile?q=",
                            projects[[i]])
        }

      req <- httr::GET(get_url,
                       httr::authenticate(":", "", "ntlm"),
                       httr::add_headers('Content-Type'='application/json'))

      status_code <- httr::stop_for_status(req)$status_code
      if(!status_code == 200){
        cli::cli_abort(c("x" = "ERROR: DataStore connection failed."))
        return(invisible(NULL))
      }
      json <- httr::content(req, "text")
      rjson <- jsonlite::fromJSON(json)

      # if the reference doesn't exist or isn't a Project:
      if (length(seq_along(rjson)) == 0) {
        bad_project_ids <- append(bad_project_ids, projects[[i]])
      } else if (rjson$referenceType != "Project") {
        bad_project_ids <- append(bad_project_ids, projects[[i]])
      }
    }
  }
  if (!is.null(bad_project_ids)) {
    msg <- paste0("The following project IDs are invalid, inactive, not ",
                  "projects, or you lack the appropriate permissions to ",
                  "access them: {.var {bad_project_ids}}.")
    cli::cli_abort(c("x" = msg))
    return("test complete")
  } else {
    msg <- paste0("All project ids are valid and activated.")
    cli::cli_inform(c("v" = msg))
  }
  return(invisible(NULL))
}


#' Checks whether a upn supplied is valid.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Reads in a .xlsx file for data validation. Checks all user names supplied in the column author_upn against Active Directory using a powershell script to verify that the accounts are valid NPS accounts. The function throws an error and lists any invalid accounts if they are encountered. Additionally, if all accounts are valid the function throws and error if any accounts do not have ORCiDs associated with them (and lists those accounts). If all accounts are valid and have ORCiDs, the function passes.
#'
#' @param filename String. File to check.
#' @param path String. Path to file. Defaults to the current working directory.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' check_users_AD("test_file.xlsx")}
check_users_AD <- function(filename, path = getwd()){
  lifecycle::deprecate_warn("0.0.1",
                            "check_users_AD()",
                            "check_author_email()")

  upload_data <- read.delim(file=paste0(path, "/", filename))
  #check for bad usernames:
  usr_name <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_name <-
      append(usr_name,
             unlist(stringr::str_split(upload_data$author_upn_list[i], ", ")))
  }
  usr_name <- unique(usr_name)

  bad_usr <- NULL
  invalid_orcid <- NULL
  for (i in 1:length(usr_name)) {
    powershell_command <- paste0('([adsisearcher]\\"(samaccountname=',
                                 usr_name[i],
                                 ')\\").FindAll().Properties')

    AD_output <- system2("powershell",
                         args = c("-Command",
                                  powershell_command),
                         stdout = TRUE)

    if (length(AD_output) == 0 ) {
      bad_usr <- append(bad_usr, usr_name)
    }
    #check_orcid - doesn't check formatting just presence/absence and nchar.
    orcid <- AD_output[
      which(AD_output %>% stringr::str_detect("extensionattribute2\\b"))]
    #get orcid from output
    orcid <- stringr::str_extract(orcid, "\\{.*?\\}")
    # remove curly braces:
    orcid <- stringr::str_remove_all(orcid, "[{}]")
    if (length(orcid == 0)) {
      invalid_orcid <- append(invalid_orcid, usr_name)
    }
    if (!grep('[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[A-Za-z0-9]{1}', orcid)) {
      invalid_orcid <- append(invalid_orcid, usr_name)
    }
  }
  if (!is.null(bad_usr)) {
    msg <- paste0("The following usernames are invalid. Please provide ",
                  "valid usernames: {bad_usr}.")
    cli::cli_abort(c("x" = msg))
  }
  if (!is.null(invalid_orcid)) {
    msg <- paste0("The following usernames do not have ORCIDs (or have ",
                  "invalid ORCIDs) associated with them. Please have the ",
                  "users add valid ORCIDs (https://orcid.org) to their ",
                  "Active Directory account at https://myaccount.nps.gov/: ",
                  "{invalid_orcid}.")
    cli::cli_abort(c("x" = msg))
  }
  msg <- "All users have NPS accounts and valid ORCiDs associated with them."
  cli::cli_inform(c("v" = msg))
  return(invisible(NULL))
}
