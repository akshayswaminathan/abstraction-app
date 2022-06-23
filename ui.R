ui <- div(class = "bg-gray-200 flex flex-col h-screen w-screen px-10 pt-8",
          shiny.tailwind::use_tailwind(),
          h1(class = "text-2xl font-bold mb-5", "Test App"),
          tags$input(type="file", oninput="this.files[0].text().then(v=>{Shiny.setInputValue(\"fileContent\", v)})", id="fileInput", accept="text/csv", class="block mb-2 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 cursor-pointer dark:text-gray-400 focus:outline-none dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400", id="file_input"),
          tags$input(class="rounded-md bg-white outline-none block px-2 py-1", oninput="Shiny.setInputValue(\"searchString\", this.value)"),
          div(class="mt-2 flex flex-col"),
          tableOutput("selected_var")
)


exports$ui <- ui