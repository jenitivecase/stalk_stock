
library(shiny)
library(RSQLite)
library(DTedit)
library(lubridate)

#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

conn <- dbConnect(RSQLite::SQLite(), glue::glue(here::here(), "/stalk_stock/", "stalks.sqlite"))

if(!'stalks' %in% dbListTables(conn)) {
    stalks <- readRDS(glue::glue(here::here(), "/stalk_stock/", "data_through_20200515.rds")) %>%
        arrange(Date, AMPM)
    stalks$id <- 1:nrow(stalks)
    stalks <- stalks %>%
        select(id, everything()) %>%
        mutate(Date = as.character(Date)) %>%
        arrange(Date, AMPM)
    dbWriteTable(conn, "stalks", stalks, overwrite = TRUE)
}

getstalks <- function() {
    res <- dbSendQuery(conn, "SELECT * FROM stalks")
    stalks <- dbFetch(res)
    dbClearResult(res)
    stalks <- stalks %>%
        mutate(AMPM = factor(AMPM, levels = c("AM", "PM"))) %>%
        mutate(Date = as_date(Date, format = "%Y-%m-%d")) %>%
        mutate(Day = lubridate::wday(Date, label = TRUE, abbr = TRUE), 
               Week = lubridate::year(Date) + lubridate::isoweek(Date) - 2037) %>%
        arrange(desc(Date), desc(AMPM)) %>%
        unique()
    return(stalks)
}

##### Callback functions.
stalks.insert.callback <- function(data, row) {

    
    query <- paste0("INSERT INTO stalks (id, Week, Day, Date, AMPM, Brianna, Jack, Jen, Kelly) VALUES (",
                    "", max(getstalks()$id) + 1, ", ",
                    "", lubridate::year(as_date(data[row,]$Date, format = "%Y-%m-%d")) + 
                        lubridate::isoweek(as_date(data[row,]$Date, format = "%Y-%m-%d")) - 2037 , ", ", #Week
                    
                    "'", as.character(lubridate::wday(as_date(data[row,]$Date, format = "%Y-%m-%d"),
                                                      label = TRUE, abbr = TRUE)), "', ", #Day
                    "'", as.character(as_date(data[row,]$Date, format = "%Y-%m-%d")), "', ", #Date
                    "'", as.character(data[row,]$AMPM), "', ", #AMPM
                    "'", data[row,]$Brianna, "', ", #Brianna
                    "'", data[row,]$Jack, "', ", #Jack
                    "'", data[row,]$Jen, "', ", #Jen
                    "'", data[row,]$Kelly, "' ", #Kelly
                    ")")
    print(query) # For debugging
    dbSendStatement(conn, query)
     
    return(getstalks())
}

stalks.update.callback <- function(data, olddata, row) {
    query <- paste0("UPDATE stalks SET ",
                    
                    "Week = '", lubridate::year(as_date(data[row,]$Date, format = "%Y-%m-%d")) + 
                        lubridate::isoweek(as_date(data[row,]$Date, format = "%Y-%m-%d")) - 2037 , "', ",
                    "Day = '", as.character(lubridate::wday(as_date(data[row,]$Date, format = "%Y-%m-%d"),
                                                            label = TRUE, abbr = TRUE)), "', ",
                    "Date = '", as.character(as_date(data[row,]$Date, format = "%Y-%m-%d")), "', ",
                    "AMPM = '", as.character(data[row,]$AMPM), "', ",
                    "Brianna = '", data[row,]$Brianna, "', ",
                    "Jack = '", data[row,]$Jack, "', ",
                    "Jen = '", data[row,]$Jen, "', ",
                    "Kelly = '", data[row,]$Kelly, "' ",
                    "WHERE id = ", data[row,]$id)
    print(query) # For debugging
    dbSendStatement(conn, query)
    return(getstalks())
}

stalks.delete.callback <- function(data, row) {
    query <- paste0('DELETE FROM stalks WHERE id = ', data[row,]$id)
    dbSendStatement(conn, query)
     
    return(getstalks())
}

##### Create the Shiny server
server <- function(input, output) {
    
    stalks <- getstalks()
    
    day_label_set <- c('Mon_AM', 'Mon_PM', 'Tue_AM', 'Tue_PM', 'Wed_AM', 'Wed_PM',
                       'Thu_AM', 'Thu_PM', 'Fri_AM', 'Fri_PM', 'Sat_AM', 'Sat_PM')
        
    long_stalks <- reactive({
        # req(input$stalk_update)

        getstalks() %>%
            mutate(time = case_when(AMPM == "AM" ~ "09:00",
                                    AMPM == "PM"~ "15:00",
                                    TRUE ~ as.character(NA))) %>%
            mutate(datetime = as.POSIXct(paste0(Date, " ", time))) %>%
            select(-time) %>%
            mutate(Week = paste0("Week ", Week)) %>%
            gather(key = "person", value = "sell_price", Brianna, Jack, Jen, Kelly) %>%
            group_by(person, Week) %>%
            arrange(Week, person, Date, AMPM) %>%
            # mutate(obs_index = paste0("obs_", sprintf("%02d", row_number()))) %>%
            mutate(obs_index = paste0(Day, "_", AMPM)) %>%
            mutate(obs_index = factor(obs_index,
                                      levels = day_label_set,
                                      labels = day_label_set))
    })
    
    output$stalk_plot <- renderPlot({
        ggplot(long_stalks()) +
            facet_grid(Week ~ ., switch = "y") + 
            geom_point(aes(x = factor(obs_index, labels = gsub("_", "\n", day_label_set)), 
                           y = sell_price, group = person, color = person), size = 1) +
            geom_line(aes(x = factor(obs_index, labels = gsub("_", "\n", day_label_set)), 
                          y = sell_price, group = person, color = person), size = 1) +
            labs(title = "Stalk Market Patterns", 
                 x = "Time",
                 y = "Sell Price",
                 color = "Person")
    })
    
   
    
    ##### Create the DTedit object
    DTedit::dtedit(input, output,
                   name = 'stalk_update',
                   thedata = stalks,
                   edit.cols = c('Date', 'AMPM', 'Brianna', 'Jack', 'Jen', 'Kelly'),
                   edit.label.cols = c('Date', 'AMPM', 'Brianna', 'Jack', 'Jen', 'Kelly'),
                   input.types = c(Date='dateInput'),
                   view.cols = c('Date', 'AMPM', 'Brianna', 'Jack', 'Jen', 'Kelly'),
                   callback.update = stalks.update.callback,
                   callback.insert = stalks.insert.callback,
                   callback.delete = stalks.delete.callback)
    
    # output$stalk_table <- renderTable(stalks)
}

##### Create the shiny UI
ui <- fluidPage(
    
    fluidRow(
        column(width = 6,
            h3('Stalk Market Time'),
            uiOutput('stalk_update')
            # tableOutput('stalk_table')
        ),
        column(width = 6,
           plotOutput('stalk_plot', height = '600px')
        )
    )
    
)

##### Start the shiny app
shinyApp(ui = ui, server = server)
