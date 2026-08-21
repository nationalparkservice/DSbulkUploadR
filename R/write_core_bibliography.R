#' Write the core bibliography information to DataStore reference landing page
#'
#' This function takes in a file with the required pre-validated input data (see `run_input_validation`) and uses it to populate the core bibliography tab for DataStore references. It currently supports AudioRecording and GenericDocument reference types.
#'
#' @param reference_id String (or integer). Seven-digit reference ID for the DataStore reference to which the bibliographic information should be added.
#' @param filename String. The name of the input.xlsx file containing data to be added to the reference page. Defaults to "DSbulkUploadR_input.xlsx".
#' @param sheet_name String. The name of the sheet within the xlsx to get data from.
#' @param row_num Integer. The row in the file of the xlsx sheet to be used
#' @param path String. Path to xlsx file in `filename`. Defaults to `getwd()`.
#' @param dev Logical. Whether data should be written to the development server or not. Defaults to TRUE.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' write_core_bibliography(reference_id = 1234567,
#'                         filename = "DSbulkUploadR_input.xlsx",
#'                         row_num = 1,
#'                         sheet_name = "AudioRecording",
#'                         path = getwd(),
#'                         dev = TRUE)}
write_core_bibliography <- function(reference_id,
                                    path = getwd(),
                                    filename = "DSbulkUploadR_input.xlsx",
                                    sheet_name,
                                    row_num,
                                    dev = TRUE) {

  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet_name)

  # populate draft reference bibliography ----
  # Scripts don't have start dates.
  if (upload_data$reference_type[row_num] != "Script") {
    begin_date <- upload_data$content_begin_date[row_num]
  }

  # Projects and Scripts don't get end dates for this uploader tool.
  if (upload_data$reference_type[row_num] != "Project" &&
      upload_data$reference_type[row_num] != "Script") {
    end_date <- upload_data$content_end_date[row_num]
  }

  # create author list ====
  # Websites don't have authors
  if (upload_data$reference_type[row_num] != "WebSite") {
    # for Projects, "authors" are equivalent to "leads":
    usr_email_list <- NULL
    if (upload_data$reference_type[row_num] == "Project") {
      usr_email_list <- upload_data$leads_email_list[row_num]
    } else {
      usr_email_list <- upload_data$author_email_list[row_num]
    }

    usr_email <- unlist(stringr::str_split(
      usr_email_list,
      ", "))
    usr_email <- stringr::str_trim(usr_email)
    usr_email <- unique(usr_email)

    req_ad_ver_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
    bdy <- usr_email
    req_ad_ver <- httr::POST(req_ad_ver_url,
                      httr::add_headers('Content-Type' = 'application/json'),
                      body = jsonlite::toJSON(bdy))
    status_code <- httr::stop_for_status(req_ad_ver)$status_code
    if (!status_code == 200) {
      stop("ERROR: DataStore connection failed.")
    }
    json <- httr::content(req_ad_ver, "text")
    rjson <- jsonlite::fromJSON(json)

    contacts <- NULL
    for (j in 1:length(usr_email)) {
      author <- list(title = "",
                     primaryName = rjson$sn[j],
                     firstName = rjson$givenName[j],
                     middleName = "",
                     suffix = "",
                     affiliation = "",
                     isCorporate = FALSE,
                     ORCID = rjson$extensionAttribute2[j])
      contacts <- append(contacts, list(author))
    }
  }

  #get system date
  today <- Sys.Date()

  # generate json body for rest api call ====
  #AudioRecordings lack publisher element:
  if (upload_data$reference_type[row_num] == "AudioRecording") {
    bib_body <- list(title = upload_data$title[row_num],
                   issuedDate = list(year = lubridate::year(today),
                                     month = lubridate::month(today),
                                     day = lubridate::day(today),
                                     precision = ""),
                   contentBeginDate = list(year = lubridate::year(begin_date),
                                           month = lubridate::month(begin_date),
                                           day = lubridate::day(begin_date),
                                           precision = ""),
                   contentEndDate = list(year = lubridate::year(end_date),
                                         month = lubridate::month(end_date),
                                         day = lubridate::day(end_date),
                                         precision = ""),
                   location = "",
                   miscellaneousCode = "",
                   volume = "",
                   issue = "",
                   pageRange = "",
                   edition = "",
                   dateRange = "",
                   meetingPlace = "",
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = upload_data$purpose[row_num],
                   tableOfContents = "",
                   #publisher = "Fort Collins, CO",
                   size1 = upload_data$length_of_recording[row_num],
                   contacts1 = contacts
                   #metadataStandardID = ""
                   #licenseTypeID = upload_data$license[row_num]
                   )
  #treat "FieldNotes" as "GenericDocument" because "FieldNotes is not a real
  #reference but the decision was made to treat it like a GenericDocument
  #and add in the keyword "FieldNotes" for later triage.
  #good luck, later triage team!
  } else if (upload_data$reference_type[row_num] == "GenericDocument" |
             upload_data$reference_type[row_num] == "FieldNotes") {
    bib_body <- list(title = upload_data$title[row_num],
                   issuedDate = list(year = lubridate::year(today),
                                     month = lubridate::month(today),
                                     day = lubridate::day(today),
                                     precision = ""),
                   contentBeginDate = list(year = lubridate::year(begin_date),
                                           month = lubridate::month(begin_date),
                                           day = lubridate::day(begin_date),
                                           precision = ""),
                   contentEndDate = list(year = lubridate::year(end_date),
                                         month = lubridate::month(end_date),
                                         day = lubridate::day(end_date),
                                         precision = ""),
                   location = "",
                   miscellaneousCode = "",
                   volume = "",
                   issue = "",
                   pageRange = "",
                   edition = "",
                   dateRange = "",
                   meetingPlace = "",
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = upload_data$purpose[row_num],
                   tableOfContents = "",
                   publisher = "National Park Service",
                   publisher = "Fort Collins, CO",
                   contacts1 = contacts
                   #metadataStandardID = "",
                   #licenseTypeID = upload_data$license[row_num]
                   )
  } else if (upload_data$reference_type[row_num] == "WebSite") {
    bib_body <- list(title = upload_data$title[row_num],
                     issuedDate = list(year = lubridate::year(today),
                                       month = lubridate::month(today),
                                       day = lubridate::day(today),
                                       precision = ""),
                     contentBeginDate = list(year = lubridate::year(begin_date),
                                             month = lubridate::month(begin_date),
                                             day = lubridate::day(begin_date),
                                             precision = ""),
                     contentEndDate = list(year = lubridate::year(end_date),
                                           month = lubridate::month(end_date),
                                           day = lubridate::day(end_date),
                                           precision = ""),
                     location = "",
                     miscellaneousCode = "",
                     volume = "",
                     issue = "",
                     pageRange = "",
                     edition = "",
                     dateRange = "",
                     meetingPlace = "",
                     abstract = upload_data$description[row_num],
                     notes = upload_data$notes[row_num],
                     purpose = upload_data$purpose[row_num],
                     tableOfContents = ""
                     #publisher = "Fort Collins, CO",
                     #size1 = upload_data$length_of_recording[row_num],
                     #contacts1 = contacts
                     #metadataStandardID = ""
                     #licenseTypeID = upload_data$license[row_num]
      )
  } else if (upload_data$reference_type[row_num] == "GenericDataset") {
    bib_body <- list(title = upload_data$title[row_num],
                   issuedDate = list(year = lubridate::year(today),
                                     month = lubridate::month(today),
                                     day = lubridate::day(today),
                                     precision = ""),
                   contentBeginDate = list(year = lubridate::year(begin_date),
                                           month = lubridate::month(begin_date),
                                           day = lubridate::day(begin_date),
                                           precision = ""),
                   contentEndDate = list(year = lubridate::year(end_date),
                                         month = lubridate::month(end_date),
                                         day = lubridate::day(end_date),
                                         precision = ""),
                   location = "Fort Collins, CO",
                   miscellaneousCode = "",
                   #volume = "",
                   #issue = "",
                   #pageRange = "",
                   edition = "",
                   #dateRange = "",
                   #meetingPlace = "",
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = upload_data$purpose[row_num],
                   #tableOfContents = "",
                   publisher = "National Park Service",
                   contacts1 = contacts,
                   contacts2 = "",
                   contacts3 = "",
                   #metadataStandardID = "",
                   licenseTypeID = upload_data$license_code[row_num]
    )
  } else if (upload_data$reference_type[row_num] == "Project") {
    # end dates are optional
    end_date <- upload_data$content_end_date[row_num]

    if (!is.na(end_date)) {
      proj_end_date <- list(year = lubridate::year(end_date),
                       month = lubridate::month(end_date),
                       day = lubridate::day(end_date),
                       precision = "")
    } else {
      proj_end_date <- list(year = "null",
                            month = "null",
                            day = "null",
                            precision = "")
    }

    bib_body <- list(title = upload_data$title[row_num],
                   issuedDate = list(year = lubridate::year(today),
                                     month = lubridate::month(today),
                                     day = lubridate::day(today),
                                     precision = ""),
                   contentBeginDate = list(year = lubridate::year(begin_date),
                                           month = lubridate::month(begin_date),
                                           day = lubridate::day(begin_date),
                                           precision = ""),
                   contentEndDate = proj_end_date,
                   location = "Fort Collins, CO",
                   miscellaneousCode = "",
                   #volume = "",
                   #issue = "",
                   #pageRange = "",
                   #edition = "",
                   #dateRange = "",
                   #meetingPlace = "",
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = "",
                   #tableOfContents = "",
                   publisher = "National Park Service",
                   contacts1 = contacts,
                   contacts2 = "",
                   contacts3 = "",
                   #metadataStandardID = "",
                   licenseTypeID = upload_data$license_code[row_num]
    )
  } else if (upload_data$reference_type[row_num] == "Script") {
    bib_body <- list(title = upload_data$title[row_num],
                   issuedDate = list(year = lubridate::year(today),
                                     month = lubridate::month(today),
                                     day = lubridate::day(today),
                                     precision = ""),

                   #location = "Fort Collins, CO",
                   miscellaneousCode = upload_data$scripting_language[row_num],
                   #volume = "",
                   #issue = "",
                   #pageRange = "",
                   edition = upload_data$version[row_num],
                   #dateRange = "",
                   #meetingPlace = "",
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = "",
                   #tableOfContents = "",
                   publisher = "National Park Service",
                   contacts1 = contacts,
                   contacts2 = "",
                   contacts3 = "",
                   #metadataStandardID = "",
                   licenseTypeID = upload_data$license_code[row_num]
    )
  }

  #bib_body <- rjson::toJSON(bib_body)
  #for testing purposes and to look at the json sent:
  #jsonlite::prettify(bib_body)

  # make request to populate reference ====
  if (dev == TRUE) {
    bib_api_url <- paste0(.ds_dev_api(),
                      "Reference/",  reference_id, "/Bibliography")
  } else {
    bib_api_url <- paste0(.ds_secure_api(),
                      "Reference/",  reference_id , "/Bibliography")
  }

  bib_put_req <- httr::PUT(
    url = bib_api_url,
    httr::add_headers('Content-Type' = 'application/json'),
    httr::authenticate(":", "", "ntlm"),
    body = jsonlite::toJSON(bib_body, pretty = TRUE, auto_unbox = TRUE))

  return(invisible(NULL))
}
