linkModUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      
      shinyjs::useShinyjs(),
      uiOutput(ns("linkList"))
      
    ))
}

linkModServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      linkCSV <- read_csv("https://raw.githubusercontent.com/bcgov/LFS_app/main/app/bcstats_links.csv")
      links <- linkCSV$url
      names(links) <- linkCSV$name
      
      output$linkList <- renderUI({
        
        div(selectInput("link-list",
                    label = "",
                    selected = "nothing",
                    choices = c("More BCStats Statistics..." = "nothing", links)),
            tags$script(type='text/javascript', 
                    "{
                       var urlMenu = document.getElementById('link-list');
                       urlMenu.onchange = function() {
                         var userOption = this.options[this.selectedIndex];
                         if(userOption.value != 'nothing') {
                         window.open(userOption.value, 'bcstats-shinyapps', '');
                         }
                       }}"))
        
        
        
      })
      
    }
  )
} 
# 
# ui <- fluidPage(modUI('testModule'))
# 
# server <- function(input, output) { mod("testModule") }
# 
# shinyApp(ui = ui, server = server)