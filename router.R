

router <- make_router(
  #route('/', div(class="flex text-center h-full", span(class="block mx-auto my-auto font-bold text-bodyTitle","index"))),
  route('/', uiOutput("settingsBody")),
  route('patient', uiOutput("body"))
)

#patientRouter <- make_router(
#  route('patient', uiOutput("patientSearchBody")),
#  route('patient/chart', uiOutput("patientChartSearchBody"))
#)

exports$router <- router
#exports$patientRouter <- patientRouter