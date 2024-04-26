# Library in packages used in this application
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(shinymanager)
library(htmltools)
library(sf)
library(lubridate)
library(telegram.bot)
library(shinyWidgets)
library(future)
library(highcharter)

# Turn off scientific notation
options(scipen = 999)
options(spinner.type = 4)


db_config <- config::get()$db
conn <- DBI:: dbConnect(RSQLite::SQLite(), dbname = db_config$dbname)

