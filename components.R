exports$settingsButton <- div(class="w-full bg-grey-100 flex flex-row gap-3 px-4 py-3 rounded font-medium text-sidebar items-center",
                      rheroicons::rheroicon(name = "cog", type = "solid", class="w-6 h-6"),
                      span(class="w-full", "Settings")
)

exports$monogram <- monogram <- function(id){
  div(class="h-8 w-8 shrink-0 flex flex-col primary-gradient rounded-full",
      span(class="block mx-auto my-auto text-monogram text-center font-semibold text-white",id))
}

exports$patientListItem <- patientListItem <- function(patient, selected = FALSE){
  a(class=paste("block p-[10px] rounded flex flex-row gap-[10px]", ifelse(selected, "bg-grey-200", "hover:bg-grey-200/50")), href=route_link(paste0('patient?patient_id=',patient$id, '&page=1')),
    monogram(patient$id),
    div(class="flex flex-col gap-[5px] w-full",
        span(class="font-medium text-monogram",paste("Patient", patient$id)),
        span(class="block text-muted break-words line-clamp-1 font-light text-mini leading-tight", paste0(patient$charts, collapse="•"))
    ),
    rheroicon("chevron_right", "outline", class = 'w-6 h-6 my-auto shrink-0')
  )
}

exports$patientsList <- patientsList <- function(patients, selectedId = NULL){
  do.call(div, c(
    class="rounded bg-grey-100 flex flex-col gap-[5px] p-[5px]",
    purrr::map(patients, function(p){
      patientListItem(p, selected= selectedId == p$id)
    })
  ))
}

chartTab <- function(chart, idx, selected, patient){
  a(class="pl-4 pr-3",
    href=route_link(paste0('patient?patient_id=',patient$id, '&page=', idx)),
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

exports$chartTabs <- chartTabs <- function(charts, selected=1, patient){
  do.call(div, c(
    class="flex flex-col divide-y-2 divide-grey-200  h-full",
    style="display: flex; flex-direction: column; min-width: 60px;",
    purrr::imap(charts, function(chart, pos){
      chartTab(chart, pos, pos==selected, patient)
    })
  ))
}

exports$patientView <- function(patient, selChart){
  div(class="flex flex-col h-full divide-y-2 divide-grey-200",
    div(class="flex flex-row font-semibold py-3 px-6 bg-white text-bodyTitle", paste("Patient", patient$id)),
    div(class="flex flex-row divide-x-2 divide-grey-200 h-full relative",
      chartTabs(patient$charts, selChart, patient),
      div(class="flex flex-row w-full divide-x-2 divide-grey-200",
          div(class="flex flex-col grow w-full px-40 pt-8",
              div(class="rounded bg-grey-200 py-2 px-3 gap-2 text-muted font-light flex flex-row",
                  rheroicon("search", class = "w-5 h-5 my-auto"),
                  tags$input(class="w-full bg-transparent outline-none caret-primary font-medium text-black placeholder:text-muted placeholder:font-normal", placeholder="Search charts..."))
          ),
          div(class="flex flex-col py-4 px-6 w-[350px]",
              h3(class="text-sidebarHeader font-semibold","Record Data")
              )
          )
  )
  )


}
