
library(shiny)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    shiny::includeCSS('www/style.css'),
    theme = shinythemes::shinytheme('cosmo'),
    titlePanel("D&D swarm simulator"),
    fluidRow(
        column(4,
               actionButton('addSwarm',label = 'Add swarm'),
               actionButton('removeSwarm', label = 'Remove swarm')),
        column(8,
               br(),
               div(consoleUI('console'),
                   class="affix",
                   style="width:50%"
                   ))
    )
 
))
