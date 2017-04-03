# application specific logic
# last update: 2016-10-07

source('srvEmail.R', local=TRUE)

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

observe({
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                schedulerStatusKey)
                retVal <- readItems(app, url)
                schedulerActiveStatus <- TRUE
                if((nrow(retVal) > 1) | (nrow(retVal) == 0)){
                        deleteRepo(app, url)
                        item <- list(active         = schedulerActiveStatus,
                                     '_oydRepoName' = 'Scheduler Status')
                        writeItem(app, url, item)
                }
                if(nrow(retVal) == 1){
                        schedulerActiveStatus <- retVal$active
                }
                if(schedulerActiveStatus){
                        updateCheckboxInput(session, 'scheduler_active',
                                            value=TRUE,
                                            label=paste(appTitle, '(aktiv)'))
                } else {
                        updateCheckboxInput(session, 'scheduler_active',
                                            value=FALSE,
                                            label=paste(appTitle, '(inaktiv)'))
                }
        }
})

observeEvent(input$scheduler_active, {
        app <- currApp()
        if(length(app) > 0){
                schedulerActiveStatus <- TRUE
                if(input$scheduler_active){
                        updateCheckboxInput(session, 'scheduler_active',
                                            label=paste(appTitle, '(aktiv)'))
                        schedulerActiveStatus <- TRUE
                } else {
                        updateCheckboxInput(session, 'scheduler_active',
                                            label=paste(appTitle, '(inaktiv)'))
                        schedulerActiveStatus <- FALSE
                }
                url <- itemsUrl(app[['url']],
                                schedulerStatusKey)
                retVal <- readItems(app, url)
                if(nrow(retVal) > 1){
                        deleteRepo(app, url)
                        retVal <- data.frame()
                }
                item <- list(active         = schedulerActiveStatus,
                             '_oydRepoName' = 'Scheduler Status')
                if(nrow(retVal) == 1){
                        updateItem(app, url, item, retVal$id)
                }
                if(nrow(retVal) == 0){
                        writeItem(app, url, item)
                }
        }
})

renderSchedulerTasks <- function(data){
        if(nrow(data) > 0) {
                data <- data[, c('app', 'task', 'time')]
                colnames(data) <- c('App', 'Typ', 'Pattern')
                DT::datatable(data,
                              selection = 'single',
                              options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')))
        } else {
                data.frame()
        }
}

renderSchedulerApps <- function(data){
        if(nrow(data) > 0){
                data <- data[, c('app'), drop=FALSE]
                unique(data$app)
        } else {
                data.frame()
        }
}

output$scheduler_tasks <- DT::renderDataTable({
        data <- repoData('eu.ownyourdata.scheduler')
        renderSchedulerTasks(data)
})

output$scheduler_logs <- DT::renderDataTable({
        data <- repoData('eu.ownyourdata.scheduler.log')
        if(nrow(data) > 0) {
                data$zeit <- as.POSIXct(data$timestamp, origin="1970-01-01")
                data <- data[, c('zeit', 'text')]
                colnames(data) <- c('Zeitpunkt', 'Meldung')
                DT::datatable(data,
                              selection = 'none',
                              options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')))
        } else {
                data.frame()
        }
})

output$schedulerApps <- renderTable({
        data <- currData()
        renderSchedulerApps(data)
}, colnames=FALSE
)

observeEvent(input$delTask, {
        taskId <- input$scheduler_tasks_rows_selected
        if(is.integer(taskId)){
                app <- currApp()
                url <- itemsUrl(app[['url']], schedulerKey)
                schedulerItems <- readItems(app, url)
                itemID <- schedulerItems[taskId,'id']
                deleteItem(app, url, itemID)
                output$scheduler_tasks <- DT::renderDataTable({
                        renderSchedulerTasks(
                                schedulerItems[schedulerItems$id != itemID, ])
                })
                output$schedulerApps <- renderTable({
                        renderSchedulerApps(
                                schedulerItems[schedulerItems$id != itemID, ])
                })
                
        }
})