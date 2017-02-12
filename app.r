# source server
source("app/server.r")

# source user interface
source("app/ui.r")

# Run app
shinyApp(ui = ui, server = server)
