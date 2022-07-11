components <- require.r('./components.R')
router <- require.r('./router.R')$router


ui <- div(class = "bg-grey-100 divide-x-2 divide-grey-200 flex flex-row h-screen w-screen",
          shiny.tailwind::use_tailwind(
            # Custom classes
            css = c("custom.css"),
            # configuration, see here: https://tailwindcss.com/docs/configuration
            # This one adds extra colors
            tailwindConfig = "tailwind.config.js"
          ),
          div(class="w-[300px] bg-white flex flex-col px-7 pt-12 gap-8",
              h1(class="sidebar-title", "UDP App"),
                  components$settingsButton,
                  h2(class="sidebar-subtitle", "Patients"),
                uiOutput('patientList')

          ),
          div(class="w-full bg-grey-100 ",
                router$ui
              )

)


exports$ui = ui