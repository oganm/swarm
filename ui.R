#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    shiny::includeCSS('www/style.css'),
    theme = shinythemes::shinytheme('cosmo'),
    # titlePanel("D&D swarm simulator"),
    fluidRow(
        column(4,
               actionButton('addSwarm',label = 'Add swarm'),
               actionButton('removeSwarm', label = 'Remove swarm'),
               actionButton('consoleTest',label = 'test')),
        column(8,
               div(consoleUI('console'),
                   class="affix",
                   style="width:50%"
                   ))
    )
 
))
