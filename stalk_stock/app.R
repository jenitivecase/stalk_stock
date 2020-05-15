
library(shiny)
library(DTedit)
library(lubridate)

#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")


##### Create the Shiny server
server <- function(input, output) {
    
    stalks <- readRDS("data_through_20200515.rds") %>%
        arrange(desc(Date), desc(`AM/PM`))
    
    
    day_label_set <- stalks %>%
            filter(Week == 1) %>%
            rename("AMPM" = `AM/PM`) %>%
            mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
            gather(key = "person", value = "sell_price", Brianna, Jack, Jen, Kelly) %>%
            group_by(person, Week) %>%
            arrange(Week, person, Date, AMPM) %>%
            mutate(obs_index = paste0(Day, "_", AMPM)) %>%
            pull(obs_index) %>%
            unique()
        
    
    long_stalks <- reactive({
        
        stalks %>%
            rename("AMPM" = `AM/PM`) %>%
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
    
    ##### Callback functions.
    my.insert.callback <- function(data, row) {
        stalks <- rbind(data, stalks) %>%
            mutate(`AM/PM` = factor(`AM/PM`, levels = c("AM", "PM"))) %>%
            mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
            mutate(Day = lubridate::wday(Date, label = TRUE, abbr = TRUE), 
                   Week = lubridate::year(Date) + lubridate::isoweek(Date) - 2037) %>%
            arrange(desc(Date), desc(`AM/PM`)) %>%
            unique()
        
        return(stalks)
    }
    
    my.update.callback <- function(data, olddata, row) {
        stalks[row,] <- data[1,]
        
        stalks <- stalks %>%
            mutate(`AM/PM` = factor(`AM/PM`, levels = c("AM", "PM"))) %>%
            mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
            mutate(Day = lubridate::wday(Date, label = TRUE, abbr = TRUE), 
                   Week = lubridate::year(Date) + lubridate::isoweek(Date) - 2037) %>%
            arrange(desc(Date), desc(`AM/PM`)) %>%
            unique()
        
        return(stalks)
    }
    
    my.delete.callback <- function(data, row) {
        stalks <- stalks[-row,] %>%
            unique()
        return(stalks)
    }
    
    ##### Create the DTedit object
    DTedit::dtedit(input, output,
                   name = 'stalk_update',
                   thedata = stalks,
                   edit.cols = c('Date', 'AM/PM', 'Brianna', 'Jack', 'Jen', 'Kelly'),
                   edit.label.cols = c('Date', 'AM/PM', 'Brianna', 'Jack', 'Jen', 'Kelly'),
                   #input.types = c(notes='textAreaInput'),
                   view.cols = c('Date', 'AM/PM', 'Brianna', 'Jack', 'Jen', 'Kelly'),
                   callback.update = my.update.callback,
                   callback.insert = my.insert.callback,
                   callback.delete = my.delete.callback)
}

##### Create the shiny UI
ui <- fluidPage(
    
    fluidRow(
        column(width = 6,
            h3('Stalk Market Time'),
            uiOutput('stalk_update')
        ),
        column(width = 6,
           plotOutput('stalk_plot')
        )
    )
    
)

##### Start the shiny app
shinyApp(ui = ui, server = server)
