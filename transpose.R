library(dplyr)

# convert the df of files to a list representation that is easy to render in shiny

filesToList <- function (...){
  # accepts a list of lists, with each of the structure { name: String, data: DataFrame }
  files <- list(...)
  files <- purrr::map(files, function(file){
    mutate(file$data, Category=file$name)
  })
  # str(files[1])
  master.df <- do.call(rbind, files)
  patients.list <- split(master.df, master.df$Patient.Id)
  purrr::imap(patients.list, function(patient.df, pid){
    # separate charts
    chartGroups <- purrr::map(split(patient.df, patient.df$Category), function (chart.group){
      purrr::map(seq_len(nrow(chart.group)),
        function(row){as.list(unlist(chart.group[row,]))}
      )
    })
    list(id=pid, chartGroups=chartGroups)

  })

}

exports$filesToList <- filesToList