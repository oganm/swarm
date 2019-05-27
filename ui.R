
library(shiny)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    shiny::includeCSS('www/style.css'),
    theme = shinythemes::shinytheme('sandstone'),
    titlePanel("D&D swarm simulator"),
    shinyjs::hidden(div(id = 'main_content',
        fluidRow(
            column(4,
                   actionButton('addSwarm',label = 'Add swarm'),
                   actionButton('removeSwarm', label = 'Remove swarm')),
            column(8,
                   br(),
                   div(consoleUI('console'),
                       class="affix"
                   ))),
        actionButton('meh','Donate',
                     icon = icon('gift'),
                     onclick =
                         "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=NBC57LQVGMAJG', '_blank')",
                     style = 'float:right;padding:6px 10px;font-size:80%'),
        bsTooltip('meh',title = readLines('http://oganm.com/donation.txt'),placement = 'top')
        ))

 
))
