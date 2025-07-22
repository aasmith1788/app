# =====================================================================
# app.R – top-level file that runs the Stuff+ Shiny app
# =====================================================================

library(shiny)

# 1) Load your module file
source("C:/Users/aasmi/p3_summer_2025/Pitch Modeling Organaized/StuffApp/Modules/MLBstuff.R")

# 2) Define UI
ui <- fluidPage(
  stuffPlusUI("sp")  # "sp" = module ID
)

# 3) Define server
server <- function(input, output, session) {
  stuffPlusServer("sp")
}

# 4) Launch app
shinyApp(ui, server)
