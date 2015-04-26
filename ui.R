library(shiny)

shinyUI(fluidPage(
        titlePanel("Agile - Lead Time Histogram"),        
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Work item"),
                        
                        checkboxInput("story", 
                                      "Show story", value=TRUE),
                        
                        checkboxInput("defect", 
                                      "Show defect", value=TRUE),
                        
                        br(),
                        
                        helpText("Fill"),
                        
                        checkboxInput("fill", "Fill by work item type", 
                                      value=FALSE)
                ),
                
                mainPanel(
                        plotOutput("leadTimeHist", height="350px"),
                        
                        br(),
                        
                        tableOutput("leadTimeTable")
                )
        )
))
