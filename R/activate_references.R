#' Activate one or more references
#'
#' activate_references takes a list of one or more references, checks their lifecycle status and if they are in Draft form, attempts to activate the reference. If upon attempting activation an error occurs, the user will be notified and the function will move on to the next reference in the list. The function invisibly returns a dataframe where each reference has a row with reference id, initial activation status, current activation status, and an indicator of whether an error was encountered. It is a very wise idea to capture the function output into a variable so that any references that were not activated can be inspected and a re-activation can be attempted. If you forget to do this, it's OK to just re-run the function. It does no harm to attempt to re-activate already active references.
#'
#' Typical errors include: authentication errors (not on VPN), permissions errors (not a reference owner), or business rules violations (the reference lacks one or more fields required for activation).
#'
#' @param reference_id String (or integer). One or more 7-digit DataStore reference IDs to be activated
#' @param dev Logical. Defaults to TRUE. Should the API call be to the development server (TRUE) or the production server (FALSE)
#'
#' @returns dataframe (invisibly)
#' @export
#' @examples
#'  \dontrun{
#'  activated_refs <- activate_references(c(1234567, 7654321, 1111111),
#'                                        dev = TRUE)}
activate_references <- function(reference_id,
                                dev = TRUE) {
  df <- NULL
  for (i in seq_along(reference_id)) {
    problem <- NA
    pre_lifecycle <- NPSdatastore::get_lifecycle_info(reference_id[i],
                                                      dev = dev)
    pre_lifecycle <- pre_lifecycle$lifecycle

    if (pre_lifecycle == "Draft") {
      tryCatch({
        NPSdatastore::set_lifecycle_active(reference_id[i],
                                       dev = dev,
                                       interactive = FALSE)
        problem <<- "no"
      }, error = function(e) {
        msg <- paste0("Reference ", reference_id[i], " could not be activated.",
                      " Make sure you are connected to the VPN and that all ",
                      "the required fields for activating the reference are ",
                      "complete.")
        cli::cli_warn(c("!" = msg))
        problem <<- "yes"
      })
    } else {
      problem <- "no lifecycle change made"
    }
    post_lifecycle <- NPSdatastore::get_lifecycle_info(reference_id[i],
                                                       dev = dev)
    post_lifecycle <- post_lifecycle$lifecycle

    df <- rbind(df, c(reference_id[i], pre_lifecycle, post_lifecycle, problem))
  }
  colnames(df) <- c("reference_id", "start_lifecycle",
                    "current_lifecycle", "errors")
  #return: list of active and non-active references
  return(invisible(df))
}
