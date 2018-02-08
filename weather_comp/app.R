#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(maps)

temp <- read_csv("temps.csv")
sun <- read_csv("sun.csv")
precip <- read_csv("precip.csv")
geo <- read_csv("geo.csv")
global <- map_data("world")

ui <- function(request) {
    fluidPage(
        fluidRow(column(6, selectizeInput("city1", "City 1", choices = unique(temp$location), selected = "Charlotte, NC, United States")),
                 column(6, selectizeInput("city2", "City 2", choices = unique(temp$location), selected = "Yellowknife, Nortwest Territories, Canada"))
                 ),

        fluidRow(column(6, plotOutput("temp1")),
                 column(6, plotOutput("temp2"))
        ),

        fluidRow(column(6, plotOutput("precip1")),
                  column(6, plotOutput("precip2"))
        ),
        fluidRow(column(6, plotOutput("sun1")),
                  column(6, plotOutput("sun2"))
        ),

        fluidRow(column(6, plotOutput("map1")),
                 column(6, plotOutput("map2"))
        )
    )
}

server <- function(input, output, session) {

    geo1Data <- reactive({
        geo %>% filter(location == input$city1)
    })

    geo2Data <- reactive({
        geo %>% filter(location == input$city2)
    })

    temp1Data <- reactive({
        temp %>% filter(location == input$city1) %>% mutate(month = factor(month, levels = month.abb))
    })

    temp2Data <- reactive({
        temp %>% filter(location == input$city2) %>% mutate(month = factor(month, levels = month.abb))
    })

    precip1Data <- reactive({
        precip %>% filter(location == input$city1) %>% mutate(month = factor(month, levels = month.abb))
    })

    precip2Data <- reactive({
        precip %>% filter(location == input$city2) %>% mutate(month = factor(month, levels = month.abb))
    })

    sun1Data <- reactive({
        sun %>% filter(location == input$city1) %>% mutate(month = factor(month, levels = month.abb))
    })

    sun2Data <- reactive({
        sun %>% filter(location == input$city2) %>% mutate(month = factor(month, levels = month.abb))
    })

    output$map1 <- renderPlot({
        ggplot() + geom_polygon(data = global, aes(x=long, y = lat, group = group), fill = "#2C3E50") +
            geom_point(data=geo1Data(), aes(lon, lat), color = "#CD2C24", size=3, shape = 25, fill = "#CD2C24") +
            labs(x="", y="") +
            theme_minimal() +
            theme(axis.text.x=element_blank(), axis.text.y=element_blank())
    })

    output$map2 <- renderPlot({
        ggplot() + geom_polygon(data = global, aes(x=long, y = lat, group = group), fill = "#2C3E50") +
            geom_point(data=geo2Data(), aes(lon, lat), color = "#CD2C24", size=3, shape = 25, fill = "#CD2C24") +
            labs(x="", y="") +
            theme_minimal() +
            theme(axis.text.x=element_blank(), axis.text.y=element_blank())
    })

    output$temp1 <- renderPlot({

        temp_max = max(max(temp1Data()$temp_high_rec), max(temp2Data()$temp_high_rec))
        temp_min = min(min(temp1Data()$temp_low_rec), min(temp2Data()$temp_low_rec))

        ggplot(temp1Data(), aes(x=month)) +
            geom_hline(yintercept = 32, color = "#D9D1C7") +

            geom_point(aes(y=temp_high_rec), size = 9, color = "#CD2C24") +
            geom_line(aes(y=temp_high_rec), group = 1, color = "#CD2C24") +
            geom_text(aes(y=temp_high_rec, label=temp_high_rec), color = "white", size = 3.3) +

            geom_point(aes(y=temp_high_avg), size = 9, color = "#F2594B") +
            geom_line(aes(y=temp_high_avg), group = 1, color = "#F2594B") +
            geom_text(aes(y=temp_high_avg, label=temp_high_avg), color = "white", size = 3.3) +

            geom_point(aes(y=temp_low_avg), size = 9, color = "#3498DB") +
            geom_line(aes(y=temp_low_avg), group = 1, color = "#3498DB") +
            geom_text(aes(y=temp_low_avg, label = temp_low_avg), color = "white", size = 3.3) +

            geom_point(aes(y=temp_low_rec), size = 9, color = "#2C3E50") +
            geom_line(aes(y=temp_low_rec), group = 1, color = "#2C3E50") +
            geom_text(aes(y=temp_low_rec, label = temp_low_rec), color = "white", size = 3.3) +

            scale_y_continuous(limits = c(temp_min, temp_max)) +

            labs(y = "Temperature (F)", x = "Month") +

            theme_minimal()
    })

    output$temp2 <- renderPlot({

        temp_max = max(max(temp1Data()$temp_high_rec), max(temp2Data()$temp_high_rec))
        temp_min = min(min(temp1Data()$temp_low_rec), min(temp2Data()$temp_low_rec))

        ggplot(temp2Data(), aes(x=month)) +
            geom_hline(yintercept = 32, color = "#D9D1C7") +

            geom_point(aes(y=temp_high_rec), size=9, color = "#CD2C24") +
            geom_line(aes(y=temp_high_rec), group =1, color = "#CD2C24") +
            geom_text(aes(y=temp_high_rec, label=temp_high_rec), color = "white", size = 3.3) +

            geom_point(aes(y=temp_high_avg), size=9, color = "#F2594B") +
            geom_line(aes(y=temp_high_avg), group =1, color = "#F2594B") +
            geom_text(aes(y=temp_high_avg, label=temp_high_avg), color = "white", size = 3.3) +

            geom_point(aes(y=temp_low_avg), size=9, color = "#3498DB") +
            geom_line(aes(y=temp_low_avg), group =1, color = "#3498DB") +
            geom_text(aes(y=temp_low_avg, label=temp_low_avg), color = "white", size = 3.3) +

            geom_point(aes(y=temp_low_rec), size=9, color = "#2C3E50") +
            geom_line(aes(y=temp_low_rec), group =1, color = "#2C3E50") +
            geom_text(aes(y=temp_low_rec, label=temp_low_rec), color = "white", size = 3.3) +

            scale_y_continuous(limits = c(temp_min, temp_max)) +

            labs(y = "Temperature (F)", x = "Month") +

            theme_minimal()
    })


    output$precip1 <- renderPlot({

        precip_max = max(max(precip1Data()$data_value),max(precip2Data()$data_value))

        ggplot(precip1Data(), aes(month, data_value, fill=data_key)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = c("#225378","#1695A3","#ACF0F2"), name = "Type") +
            ylim(0, precip_max) +
            labs(y = "Precipitation (Inches)", x = "Month") +
            theme_minimal() +
            theme(legend.position = "bottom")
    })

    output$sun1 <- renderPlot({
        ggplot(sun1Data(), aes(month, data_value, fill=data_key)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("#004358","#FFE11A"), name = "Time") +
            labs(y = "Average Sunshine (Hours)", x = "Month") +
            theme_minimal() +
            theme(legend.position = "bottom")
    })

    output$precip2 <- renderPlot({

        precip_max = max(max(precip1Data()$data_value), max(precip2Data()$data_value))

        ggplot(precip2Data(), aes(month, data_value, fill=data_key)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = c("#225378","#1695A3","#ACF0F2"), name = "Type") +
            ylim(0, precip_max) +
            labs(y = "Precipitation (Inches)", x = "Month") +
            theme_minimal() +
            theme(legend.position = "bottom")
    })

    output$sun2 <- renderPlot({
        ggplot(sun2Data(), aes(month, data_value, fill=data_key)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("#004358","#FFE11A"), name = "Time") +
            labs(y = "Average Sunshine (Hours)", x = "Month") +
            theme_minimal() +
            theme(legend.position = "bottom")
    })
}
# Run the application
shinyApp(ui = ui, server = server)

