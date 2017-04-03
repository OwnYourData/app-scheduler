# layout for section "Status"
# last update: 2016-10-10

appStatus <- function(){
        fluidRow(
                column(12, 
                       tabsetPanel(
                               type='tabs',
                               tabPanel('periodische Aufgaben', br(),
                                        DT::dataTableOutput('scheduler_tasks'),
                                        actionButton('delTask', 'Entfernen', 
                                                     icon('trash'))),
                               tabPanel('Verlauf', br(),
                                        DT::dataTableOutput('scheduler_logs'))
                       )
                )
        )
}

# constants for configurable Tabs
# defaultStatTabsName <- c('Plot')
# 
# defaultStatTabsUI <- c(
#         "
#         tabPanel('Plot',
#                  plotOutput(outputId = ns('bank2Plot'), height = '300px')
#         )
#         "
# )
# 
# defaultStatTabsLogic <- c(
#         "
#         output$bank2Plot <- renderPlot({
#                 data <- currData()
#                 plot(x=data$date, y=data$value, type='l', 
#                         xlab='Datum', ylab='Euro')
#         
#         })
#         "
# )
