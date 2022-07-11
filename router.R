

router <- make_router(
  route('/', span("index")),
  route('test', span("test")),
  route('patient', uiOutput("body"))
)

exports$router <- router