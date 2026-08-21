#' Launch bulk reference creation and file upload tool
#'
#' The function will first run `run_input_validation` on the supplied .txt of information and enforce data validation. All errors must be resolved before the function will proceed.
#'
#' The function will inform the user of the total number of references to be created, the number of files to be uploaded, and the total volume of data to be uploaded (in GB) and ask the user if they are sure they want to proceed.
#'
#' The function then creates a draft reference on DataStore for each line in the input .txt and uses the information provided in the .txt to populate the Reference. Finally, all files in the given path for a reference in the .txt will be uploaded to the appropriate reference. All generated references will have the by-or-for-NPS flag set to TRUE. Currently the bulk reference generation tool only supports public references.
#'
#' The original dataframe generated from the .txt is returned to the user with a single column added: the DataStore reference ID for each newly created reference.
#'
#' @param path String. Path to the file.
#' @param filename String. The name of the file with information on what will be uploaded. Defaults to "DSbulkUploadR_input.xlsx". Must be an xlsx.
#' @param sheet String. Name of the sheet within the .xlsx to read data from.
#' @param max_file_upload Integer. The maximum allowable number of files to upload. Defaults to 500.
#' @param max_data_upload Integer. The maximum allowable amount of data to upload (in GB). Defaults to 100.
#' @param data_upload Logical. Defaults to TRUE. To create a bunch of draft reference but not upload any files to them, set the parameter `data_upload` to `FALSE`.
#' @param dev Logical. Whether the reference creation/file uploads will occur on the development server (TRUE) or the production server (FALSE). Defaults to TRUE.
#'
#' @returns Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' generate_references(sheet = "AudioRecording")}
generate_references <- function(path = getwd(),
                                      filename = "DSbulkUploadR_input.xlsx",
                                      sheet,
                                      max_file_upload = 500,
                                      max_data_upload = 10,
                                      data_upload = TRUE,
                                      dev = TRUE) {

  #Projects cannot have data uploaded directly to them:
  if (sheet == "Project") {
    data_upload <- FALSE
  }

  #check upload file validity:
  validation <- run_input_validation(filename = filename,
                                     path = path,
                                     sheet = sheet,
                                     max_file_upload = max_file_upload,
                                     max_data_upload = max_data_upload,
                                     dev = dev)

  #force user to fix all errors:
  if (validation[1] > 0) {
    msg <- paste0("Please ensure you have supplied valid upload ",
                  "information and have addressed all the errors ",
                  "identified before proceeding with the bulk upload.")
    cli::cli_abort(msg)
    return()
  } else if (validation[2] > 0) {
    msg <- cat("The data validation process has identified",
                  "warnings. Are you sure you want to proceed without",
                  "addressing these warnings?","\n",
                  "1: Yes", "\n","2: No","\n")
    cli::cli_inform(c("!" = msg))
    var1 <- readline(prompt= " ")
    if (var1 != 1) {
      cat("Exiting the function.")
      return(invisible(NULL))
    }
  }

  #get info about bulk upload creation:
  upload_data <- readxl::read_excel(path = paste0(path,
                                                  "/",
                                                  filename),
                                    sheet = sheet)
  #number of refs to create:
  ref_count <- nrow(upload_data)

  if (data_upload == TRUE) {
    #calculate number of files to upload:
    file_num <- 0
      for (i in 1:nrow(upload_data)) {
        files_per_ref <- length(list.files(upload_data$file_path[i]))
        file_num <- (file_num + files_per_ref)
      }

    #calculate total file size to upload:
    file_size <- 0
    for (i in 1:nrow(upload_data)) {
      file_size <- file_size +
        sum(file.info(list.files(upload_data$file_path[i],
                                 full.names = TRUE))$size)
    }
    file_gb <- file_size/1073741824

    #ask to proceed; verify number of refs to create, files to upload, and total upload size:

    msg <- paste0("Would you like to upload all of your files and create ",
                "{ref_count} new references on DataStore? This will ",
                "involve uploading {file_num} files and ",
                "{round(file_gb, 3)} GB of data.")
    cli::cli_inform(msg)
  } else {
    # if data_upload is FALSE (no file uploads)
    msg <- paste0("Would you like to create {ref_count} new references on ",
                  "DataStore? No files will be uploaded to these references.")
    cli::cli_inform(msg)
  }

  var2 <- readline(prompt = "1: Yes\n2: No\n ")
  if (var2 != 1) {
    cli::cli_inform("Exiting the function.")
    return(invisible(NULL))
  }

  upload_data$reference_id <- NULL

  for (i in 1:nrow(upload_data)) {
    # create draft reference ----
    ref_code <- create_draft_reference(draft_title = upload_data$title[i],
                                       ref_type = upload_data$reference_type[i],
                                       dev = dev)
    cli::cli_inform("Creating draft reference {ref_code}.")
    cli::cli_inform("Populating draft reference {ref_code}.")
    cli::cli_inform("Writing Core Bibliography for {ref_code}.")
    write_core_bibliography(reference_id = ref_code,
                            filename = filename,
                            sheet_name = sheet,
                            row_num = i,
                            path = path,
                            dev = dev)
    #set by-for-nps to TRUE
    cli::cli_inform("Setting \"by or for NPS\" flag for {ref_code}.")
    NPSdatastore::set_by_for_nps(reference_id = ref_code,
                                 by_for_nps = TRUE,
                                 dev = dev,
                                 interactive = FALSE)

    # upload files to reference ----
    # don't upload if data_upload == FALSE
    if (data_upload == TRUE) {

      #translate 508compliance:
      compliant <- NULL
      if (upload_data$files_508_compliant[i] == "yes") {
        compliant <- TRUE
      } else {
        compliant <- FALSE
      }

      file_list <- list.files(path = upload_data$file_path[i],
                              full.names = TRUE)

      for (j in 1:length(file_list)) {
        msg <- paste0("Uploading file {j} of {length(file_list)} to ",
                      "reference {ref_code}.")
        cli::cli_inform(msg)
        suppressWarnings(upload_files(
          filename = list.files(upload_data$file_path[i])[j],
          path = upload_data$file_path[i],
          reference_id = ref_code,
          is_508 = compliant,
          chunk_size_mb = 1,
          retry = 1,
          dev = dev))
      }
    }

    # add keywords ----
    keywords_to_add <- unlist(stringr::str_split(upload_data$keywords[i],
                                         ", "))
    keywords_to_add <- stringr::str_trim(keywords_to_add)

    #cli::cli_inform("Adding keywords to reference {ref_code}.")
    replace_keywords(reference_id = ref_code,
                 keywords = keywords_to_add,
                 dev = dev)

    #Items under FieldNotes in the input.xlsx are treated as GenericDocuments
    #as "field notes" is not a real DataStore reference type.
    #per management decision the keyword "FieldNotes" added to the
    #GenericDocument so that it can be triaged later
    #good luck, future triage team!
    if (upload_data$reference_type[i] == "FieldNotes") {
      NPSdatastore::add_keywords(reference_id = ref_code,
                                 keywords = "FieldNotes",
                                 dev = dev,
                                 interactive = FALSE)
    }

    # add content unit links ----
    cli::cli_inform("Adding Content Unit Links to {ref_code}.")
    links_to_add <- unlist(stringr::str_split(upload_data$content_units[i],
                                              ", "))
    links_to_add <- stringr::str_trim(links_to_add)

    set_content_units(reference_id = ref_code,
                      content_units = links_to_add,
                      dev = dev)

    # add producing units; takes a single reference id and one or more unit codes
    cli::cli_inform("Adding Producing Units to {ref_code}.")
    prod_units <- upload_data$producing_units[i]
    NPSdatastore::add_producing_units(reference_id = ref_code,
                                      nps_units = prod_units,
                                      dev = dev,
                                      interactive = FALSE)


    # add license information ----
    # set license type: wasn't working in set bibliography.. check to see if
    # that part of the API endpoint now works
    # Last check:
    cli::cli_inform("Setting license for {ref_code}.")
    NPSdatastore::set_license(reference_id = ref_code,
                              license_type_id = upload_data$license_code[i],
                              dev = dev,
                              interactive = FALSE)

    # add reference to project(s) (but NOT if the ref IS a project) ----
    if (upload_data$reference_type[i] != "Project") {
      if(upload_data$project_id[i] == "NA") {
        project_data$project_id[i] <- NA
      }
      if (!is.na(upload_data$project_id[i])) {
        cli::cli_inform("Adding reference {ref_code} to project {upload_data$project[i].")
        projects_to_add <- unlist(stringr::str_split(upload_data$project_id[i],
                                               ", "))
        projects_to_add <- stringr::str_trim(projects_to_add)
        for (j in 1: projects_to_add) {
          if (!is.na(projects_to_add[j])) {
            add_ref_to_projects(reference_id = ref_code,
                                project_id = projects_to_add[j],
                                dev = dev)
          }
        }
      }
    }

    # add editors to project (person uploading is also added as an editor) ----
    cli::cli_inform("Adding editors to {ref_code}.")
    editors_to_add <- unlist(stringr::str_split(upload_data$editor_email_list[i],
                                                 ", "))
    editors_to_add <- stringr::str_trim(editors_to_add)

    add_editors(reference_id = ref_code,
               editor_list = editors_to_add,
               dev = dev)

    #add reference id column to dataframe to make it easier to find them all
    suppressWarnings(upload_data$reference_id[i] <- ref_code)

  }
  return(upload_data)
}
