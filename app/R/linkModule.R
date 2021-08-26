linkModUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      
      shinyjs::useShinyjs(),
      selectInput("link-list", 
                  label = "",
                  selected = "nothing",
                  choices = c("More BCStats Statistics..." = "nothing",
                              "Economic Recovery Indicators" = "https://bcstats.shinyapps.io/Economic-Indicators",
                              "Household Projections" = "https://bcstats.shinyapps.io/hsdProjApp",
                              "Population Estimates" = "https://bcstats.shinyapps.io/popApp",
                              "Population Projections" = "https://bcstats.shinyapps.io/popProjApp",
                              "Country Trade Profiles" = "https://bcstats.shinyapps.io/CountryTradeApp")),
      tags$script(type='text/javascript', 
                  "{
                       var urlMenu = document.getElementById('link-list');
                       urlMenu.onchange = function() {
                         var userOption = this.options[this.selectedIndex];
                         if(userOption.value != 'nothing') {
                         window.open(userOption.value, 'bcstats-shinyapps', '');
                         }
                       }}")
    ))
}

linkModServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
    }
  )
} 
# 
# ui <- fluidPage(modUI('testModule'))
# 
# server <- function(input, output) { mod("testModule") }
# 
# shinyApp(ui = ui, server = server)