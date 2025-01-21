verbatim_logical = function(x) {
  result <- ifelse(x, "true", "false")
  class(result) <- "verbatim"
  return(result)
}

ymd_date = function(x){
  return(format.Date(x, "%Y-%m-%d"))
}

verbatim_character = function(x){
  if (length(x) > 1) {
    return(paste0('[', paste("\"", x, "\"", sep = '', collapse = ', '), ']'))
  } else {
    return(x)
  }
}

yml_handlers <- list(logical=verbatim_logical, POSIXct = ymd_date)

change_yaml_matter <- function(input_file, ..., output_file) {
  input_lines <- readLines(input_file)
  delimiters <- grep("^---\\s*$", input_lines)
  if (!length(delimiters)) {
    stop("unable to find yaml delimiters")
  } else if (length(delimiters) == 1L) {
    if (delimiters[1] == 1L) {
      stop("cannot find second delimiter, first is on line 1")
    } else {
      # found just one set, assume it is *closing* the yaml matter;
      # fake a preceding line of delimiter
      delimiters <- c(0L, delimiters[1])
    }
  }
  delimiters <- delimiters[1:2]
  yaml_list <- yaml::yaml.load(
    input_lines[ (delimiters[1]+1):(delimiters[2]-1) ])

  dots <- list(...)

  for (element_name in names(dots)){
    if(element_name %in% names(yaml_list)) {
      yaml_list[element_name] <- dots[element_name]
    } else {
      yaml_list <- c(yaml_list,dots[element_name])
    }
  }


  output_lines <- c(
    if (delimiters[1] > 0) input_lines[1:(delimiters[1])],
    strsplit(yaml::as.yaml(yaml_list, handlers = yml_handlers), "\n")[[1]],
    input_lines[ -(1:(delimiters[2]-1)) ]
  )

  if (missing(output_file)) {
    return(output_lines)
  } else {
    writeLines(output_lines, con = output_file)
    cli::cli_alert_info("Metadata of {output_file} updated.")
    return(invisible(output_lines))
  }
}

