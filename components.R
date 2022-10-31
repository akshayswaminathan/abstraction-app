exports$settingsButton <- a(class="w-full my-2 bg-grey-100 flex flex-row gap-3 px-4 py-3 rounded font-medium text-sidebar items-center",
                            href=route_link("/"),
                            rheroicons::rheroicon(name = "cog", type = "solid", class="w-6 h-6"),
                            span(class="w-full", "Configure")
)

# exports$exportButton <- function(download.url){
#   a(class="w-full my-2 bg-grey-100 flex flex-row gap-3 px-4 py-3 rounded font-medium text-sidebar items-center",
#                             href=download.url,
#                       rheroicons::rheroicon(name = "folder_download", type = "solid", class="w-6 h-6"),
#                       span(class="w-full", "Export"))
# }

exports$exportButton <- function(route){
  uiOutput("generatedData")
}

exports$monogram <- monogram <- function(id){
  div(class="h-8 w-8 shrink-0 flex flex-col primary-gradient rounded-full my-auto",
      span(class="block mx-auto my-auto text-monogram text-center font-semibold text-white",id))
}

exports$patientListItem <- patientListItem <- function(patient, selected = FALSE){
  a(class=paste("block p-[10px] rounded flex flex-row gap-[10px]", ifelse(selected, "bg-grey-200", "hover:bg-grey-200/50")), 
    href=route_link(paste0('patient?patient_id=',patient$id)),
    monogram(patient$id),
    div(class="flex flex-col gap-1 w-full",
        span(class="font-medium text-monogram",paste("Patient", patient$id)),
        span(class="block text-muted break-words line-clamp-1 font-light text-mini leading-tight", paste(length(unlist(patient$chartGroups)), "charts"))
    ),
    rheroicon("chevron_right", "outline", class = 'w-6 h-6 my-auto shrink-0')
  )
}

exports$patientsList <- patientsList <- function(patients, selectedId = NULL){
  do.call(div, c(
    class="rounded bg-grey-100 flex flex-col gap-[5px] p-[5px] overflow-y-auto h-full",
    unname(purrr::imap(patients, function(p, pIndex){
      patientListItem(p, selected= selectedId == pIndex)
    }))
  ))
}

# really should be named chartGroupTab
chartTab <- function(chart, idx, selected, patient){
  a(class="pl-4 pr-3",
    # use condition to display this perhaps..?
    href=route_link(do.call(paste0, c(list('patient?patient_id=',patient$id), if (is.null(idx)) {list()} else list('&chart_group=', idx) ))),
    style=paste("display: flex;
       flex-direction: row;
       flex-shrink: 0;
       flex-grow: 1;
       width: 100%;",
                ifelse(selected, "background-color: transparent","background-color: white")),
    span(chart, class="font-medium text-bodyTitle",
         style=" flex-grow: 1;
            margin: auto;
            text-align: center;
            flex-shrink: 0;
            writing-mode: sideways-lr;
            text-orientation: mixed;
            width: min-content;
            display: block"))
}

chartTabs <- function(charts, selected.tab=NULL, patient){
  do.call(div, c(
    class="flex flex-col divide-y-2 divide-grey-200 h-full",
    style="display: flex; flex-direction: column; min-width: 60px;",
    list(chartTab("All", NULL, is.null(selected.tab), patient)), # i think this works??
    unname(purrr::imap(charts, function(chart, pos){
      selected <- {
        if (is.null(selected.tab)) {
          FALSE
        }
        else {
          pos==selected.tab
        }
      }
      chartTab(pos, pos, selected, patient)
    }))
  ))
}


exports$dropdownButton <- dropdownButton <- function(name, options, inputName, ...){
  div(class="text-right ml-auto",
      div(class="relative inline-block text-left",
          
          
          tags$label(class="inline-flex cursor-pointer rounded h-full w-full justify-center text-black rounded-md bg-grey-200/50 px-4 py-2  font-medium hover:bg-opacity-30 focus:outline-none focus-visible:ring-2 focus-visible:ring-white focus-visible:ring-opacity-75",
                     `for`="dropdown-trigger",
                     name,
                     rheroicons::rheroicon("chevron_down"),
          ),
          tags$input(class="hidden", type="checkbox", id="dropdown-trigger"),
          
          do.call(div, c(list(class=" dropdown-content scale-0 absolute z-30 right-0 mt-2 w-56 origin-top-right rounded bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none transform opacity-100  p-1"),
                         purrr::map(options,
                                    function(option){
                                      tags$button( class="cursor-pointer text-gray-900 group flex w-full items-center hover:bg-grey-200 rounded px-2 py-2 text-sidebar", option, onclick=paste0("Shiny.setInputValue('", inputName,"', this.innerText);", ...))
                                    }
                         )
          )
          )
      )
  )
}

exports$switchInput <- switchInput <- function(...){
  tags$label(class="switch",
             tags$input(type="checkbox", ...),
             span(class="slider round")
  )
  
  # tags$form(class="flex flex-col pr-8 gap-2",
  #           # span(class="font-bold text-sidebarHeader mb-2", "Variable"),
  #           # tags$input(
  #             # onblur="Shiny.setInputValue('variableName', this.value);", 
  #             # class="rounded bg-grey-200 px-3 py-2 focus:outline-none ring-0 border-0 outline-none", 
  #             # type="text", 
  #             # name="variableName", 
  #             # placeholder="Variable Name", 
  #             # value=input$variableName,
  #             # tags$fieldset(class="flex flex-col border border-solid border-grey-200 p-3", 
  #                           # name="variableType", 
  #                           # onchange=" Shiny.setInputValue('variableType', event.target.value);",
  #                           # tags$legend(class="px-1", "Variable Type"),
  #                           div(class="flex flex-row gap-2", 
  #                               tags$input(class="my-auto", type="radio", ..., 
  #                                          tags$label("Yes", `for`="yes", class="grow my-auto"))),
  #                           div(class="flex flex-row gap-2", 
  #                               tags$input(class="my-auto", type="radio", ..., 
  #                                          tags$label("No", `for`="no", class="grow my-auto")))
  #                           
  #             # )
  #           # )
  #           )
  
  # tags$label(tags$input(id = "boolean", ...),
  #            span(class="slider round")
  # )
  
}

exports$numberInput <- numberInput <- function(...){
  tags$input(type="number", ..., class="bg-grey-200 rounded px-2 py-1 w-20 text-center outline-none ring-0 focus:outline-none border-0")
  
}

exports$txtInput <- txtInput <- function(...){
  tags$input(class="rounded px-2 py-1 bg-grey-200 w-24", ...)
}

searchSpace <- function(patient, chart.group){
  div(class="flex flex-col grow w-full lg:px-32 md:px-16 px-8 pt-8",
      div(class="rounded bg-grey-200  text-muted font-light flex flex-row",
          div(class="py-2 px-3 gap-2 flex flex-row grow",
              rheroicon("search", class = "w-5 h-5 my-auto"),
              tags$input(class="w-full bg-transparent outline-none caret-primary font-medium text-black placeholder:text-muted placeholder:font-normal",
                         placeholder="Search charts...",
                         id="patient-search-input",
                         oninput="Shiny.setInputValue('patientSearchString', this.value);"
              ),
              tags$button(
                `data-tooltip`="Regular Expression",
                class="cursor-pointer hover:text-white tooltip",
                onclick="window.useRegex = !window.useRegex; Shiny.setInputValue('useRegex', window.useRegex); this.classList.toggle('text-primary', window.useRegex); this.classList.toggle('hover:text-white', !window.useRegex)",
                rheroicon("flag", "solid")
              ),
          ),
          dropdownButton("Presets", list("apple", "banana", "cantaloupe"), 'patientSearchString', "document.getElementById('patient-search-input').value = this.innerText;"),
          tags$script("Shiny.setInputValue('patientSearchString', '');"),
          
      ),
      uiOutput('searchSpaceRender', class="h-full no-scrollbar overflow-y-auto relative")
  )
}

exports$searchSpace <- function(patient, chart.group.results, all=FALSE){
  chartListItem <-  function(chart){
    a(class="flex flex-row gap-4 rounded hover:bg-grey-200/50 cursor-pointer p-2",
      href=route_link(paste0("patient?patient_id=", patient$id, "&chart_group=", chart$Category, "&chart_id=", chart$Chart.ID)),
      div(class="w-20 h-24 shadow bg-white rounded shrink-0 my-auto"),
      div(class="flex flex-col",
          span(class="font-medium text-sidebarHeader", chart$Title), span(class="line-clamp-3 text-muted font-light", chart$Text))
    )
  }
  # IN THE EVENT OF [ALL] TAB
  if (all){
    do.call(div,
            c(class="flex flex-col gap-2 mt-5 h-full pb-5",
              unname(purrr::imap(chart.group.results, function(result.group, group.name){
                do.call(div,
                        c(class="flex flex-col gap-2 mt-3", # ml-4 for indentation?
                          list(
                            span(class=" text-muted text-sidebarHeader uppercase", group.name ), hr(class="h-1 border-muted ")
                          ),
                          purrr::map(result.group, chartListItem)
                        )
                )
              })
              )
            )
    )
  }
  # NORMAL CHART TAB
  else {
    do.call(div,
            c(class="flex flex-col gap-2 mt-5 h-full pb-5",
              purrr::map(chart.group.results, chartListItem)
            )
    )
  }
}


recordDataSidebar <- function(placeholder = FALSE){
  if (placeholder){
    div(class = "flex flex-col shrink-0 py-4 px-6 w-[300px]",
        div(class="rounded p-4 text-muted text-center bg-grey-100", "Please select a chart from the list.")
    )
  }
  else {
    div(class = "flex flex-col shrink-0 py-4 px-6 w-[300px]",
        h3(class = "text-sidebarHeader font-semibold", "Record Data"),
        uiOutput("recordDataSidebar"),
        h3(class="font-semibold text-sidebarHeader mt-5 mb-3", "Flag Chart"),
        uiOutput("flagChartSidebar")
    )
  }
  
}

patientSearchView <- function(patient, selChart.group){
  if (is.null(selChart.group)) {
    # instead of this placeholder, just display ALL of the charts.
    #div(class="flex text-sidebarHeader font-bold w-full h-full", span(class="mx-auto my-auto","Please select a chart group"))
    div(class = "flex flex-row w-full divide-x-2 divide-grey-200",
        searchSpace(patient, unlist(patient$chartGroups, recursive = F, use.names = F)),
        
    )
  }
  else {
    div(class = "flex flex-row w-full divide-x-2 divide-grey-200",
        searchSpace(patient, patient$chartGroups[[selChart.group]]),
        
    )
  }
}

chartSearchView <- function(chart.group, 
                            chart.id, 
                            highlight_string,
                            patient.id = NULL,
                            chart.group.name = NULL,
                            all_patients = NULL){
  #' @param chart.group a list of all the charts in that group
  
  chart <- Filter(function(x){x$Chart.ID == chart.id}, chart.group)[[1]]
  chart_ids <- map_chr(chart.group, ~.x$Chart.ID)
  patient_ids <- map_chr(all_patients, ~.x$id)
  
  # if the chart ID is the last chart for the patient, go to the next patient
  next_chart_id <- chart_ids[pmin(which(chart_ids == chart.id) + 1, length(chart_ids))]
  if (next_chart_id == chart.id) {
    next_patient_id <- patient_ids[pmin(which(patient_ids == patient.id) + 1, length(patient_ids))]
    next_chart_group <- all_patients[[next_patient_id]]$chartGroups[[chart.group.name]]
    next_chart_id <- map_chr(next_chart_group, ~.x$Chart.ID)[1]
  } else {
    next_patient_id <- patient.id
  }
  
  # if the chart ID is the first chart for the patient, go to the previous patient
  previous_chart_id <- chart_ids[pmax(which(chart_ids == chart.id) - 1, 1)]
  if (previous_chart_id == chart.id) {
    previous_patient_id <- patient_ids[pmax(which(patient_ids == patient.id) - 1, 1)]
    previous_chart_group <- all_patients[[previous_patient_id]]$chartGroups[[chart.group.name]]
    previous_chart_id <- map_chr(previous_chart_group, ~.x$Chart.ID)[1]
  } else {
    previous_patient_id <- patient.id
  }
  
  div(class="px-36 flex flex-col py-8 gap-2 grow h-full",
      # div(class="rounded bg-grey-200 py-2 px-3 gap-2 text-muted font-light flex flex-row mb-4",
      #         rheroicon("search", class = "w-5 h-5 my-auto"),
      #         tags$input(class="w-full bg-transparent outline-none caret-primary font-medium text-black placeholder:text-muted placeholder:font-normal", placeholder="Search chart...",
      #                    oninput="window.markInstance && window.markInstance.unmark()[window.useRegex? 'markRegExp':'mark'](window.useRegex? new RegExp(this.value, 'i') : this.value); Shiny.setInputValue('chartSearchString', this.value)"),
      #          tags$button(
      #            `data-tooltip`="Regular Expression",
      #            class="cursor-pointer hover:text-white tooltip",
      #            onclick="window.useRegex = !window.useRegex; this.classList.toggle('text-primary', window.useRegex); this.classList.toggle('hover:text-white', !window.useRegex)",
      #            rheroicon("flag", "solid")
      #          )
      #     ),
      div(class="rounded bg-grey-200 py-2 px-3 gap-2 text-muted font-light flex flex-row mb-4",
          rheroicon("search", class = "w-5 h-5 my-auto"),
          tags$input(class="w-full bg-transparent outline-none caret-primary font-medium text-black placeholder:text-muted placeholder:font-normal",
                     #placeholder="Search chart...",
                     value= highlight_string,
                     onclick="window.markInstance && window.markInstance.unmark()[window.useRegex? 'markRegExp':'mark'](window.useRegex? new RegExp(this.value, 'i') : this.value)"),
          tags$button(
            `data-tooltip`="Regular Expression",
            class="cursor-pointer hover:text-white tooltip",
            onclick="window.useRegex = !window.useRegex; this.classList.toggle('text-primary', window.useRegex); this.classList.toggle('hover:text-white', !window.useRegex)",
            rheroicon("flag", "solid")
          )
      ),
      div(id="mark-target",class="rounded px-6 py-4 bg-white grow overflow-y-auto", chart$Text),
      tags$script("window.markInstance = new Mark('#mark-target');"),
      div(class="flex flex-row",
          
          # div(class="flex",
          #     div(class="rounded-full p-2 bg-primary my-auto mx-auto text-white",
          #         rheroicon("chevron_left", "solid", class="h-8 w-8 stroke-2")
          #     )
          # ),
          
          a(class="flex",
            href = route_link(paste0("patient?patient_id=", previous_patient_id, "&chart_group=", chart.group.name, "&chart_id=", previous_chart_id)),
            div(class="rounded-full p-2 bg-primary my-auto mx-auto text-white",
                rheroicon("chevron_left", "solid", class="h-8 w-8 stroke-2")
            )
          ),
          
          div(class="grow flex flex-col pt-4 pb-2 gap-3 text-center",
              span(class="font-medium", paste0(chart$Title)),
              span(class="text-muted","5 matches")
              # uiOutput('chartMatches')
          ),
          # div(class="flex",
          #     div(class="rounded-full p-2 bg-primary my-auto mx-auto text-white",
          #         rheroicon("chevron_right", "solid", class="h-8 w-8 stroke-2")
          #     )
          # ),
          
          a(class="flex",
            href = route_link(paste0("patient?patient_id=", next_patient_id, "&chart_group=", chart.group.name, "&chart_id=", next_chart_id)),
            div(class="rounded-full p-2 bg-primary my-auto mx-auto text-white",
                rheroicon("chevron_right", "solid", class="h-8 w-8 stroke-2")
            )
          ),
          
          # a(class=paste("block p-[10px] rounded flex flex-row gap-[10px]", ifelse(selected, "bg-grey-200", "hover:bg-grey-200/50")), 
          #   href=route_link(paste0('patient?patient_id=',patient$id)),
          #   monogram(patient$id),
          #   div(class="flex flex-col gap-1 w-full",
          #       span(class="font-medium text-monogram",paste("Patient", patient$id)),
          #       span(class="block text-muted break-words line-clamp-1 font-light text-mini leading-tight", paste(length(unlist(patient$chartGroups)), "charts"))
          #   ),
          #   rheroicon("chevron_right", "outline", class = 'w-6 h-6 my-auto shrink-0')
          # )
          
          
      ),
      div(class="flex flex-row justify-center gap-6",
          div(class="h-16 w-12 rounded bg-white shadow hover:shadow-lg hover:scale-125 duration-300 cursor-pointer transition-all"),
          div(class="h-16 w-12 scale-110 rounded bg-white shadow-md hover:shadow-lg hover:scale-125 duration-300 cursor-pointer transition-all"),
          div(class="h-16 w-12 rounded bg-white shadow hover:shadow-lg  hover:scale-125 duration-300 cursor-pointer transition-all")
      )
      
  )
}

exports$patientView <- function(patient, selChart.group, selChart, highlight_string,
                                all_patients){
  div(class="flex flex-col h-full divide-y-2 divide-grey-200",
      div(class="flex flex-row font-semibold py-3 px-6 bg-white text-bodyTitle", paste("Patient", patient$id)),
      div(class="flex flex-row divide-x-2 divide-grey-200 h-[calc(100%_-_53px)]",
          chartTabs(patient$chartGroups, selChart.group, patient),
          {
            if (is.null(selChart)){
              patientSearchView(patient, selChart.group)
            }
            else {
              chartSearchView(chart.group = patient$chartGroups[[selChart.group]], 
                              chart.id = selChart, 
                              highlight_string = highlight_string,
                              patient.id = patient$id,
                              chart.group.name = selChart.group,
                              all_patients = all_patients)
            }
          },
          recordDataSidebar(is.null(selChart))
          
      )
  )
  
  
}
