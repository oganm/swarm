library(shiny)
library(monsteR) # github.com/oganm/monseR
library(diceSyntax) # github.com/oganm/diceSyntax
library(magrittr)
library(shinycssloaders)
library(shinyjs)
library(purrr)
library(glue)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)

source('modules/consoleModule.R')
source('modules/swarmModule.R')

swarmLimit = 1000