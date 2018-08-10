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
library(viridis)
library(dichromat)
library(scales)
library(ggTimeSeries)
library(plotly)
library(shinycssloaders)
options(spinner.color="#657b83")

temp <- read_csv("temps.csv")
sun <- read_csv("sun.csv")
precip <- read_csv("precip.csv")
geo <- read_csv("geo.csv")
comfort <- read_csv("comfort.csv")
cluster <- read_csv("cluster.csv")
global <- map_data("world")

ui <- function(request) {
    fluidPage(theme = "style.css",

        titlePanel(title = "", windowTitle = "Weather Compare"),

        fluidRow(column(6,
                        align = "center",
                        selectizeInput("city1", "Select City",
                                       choices = unique(temp$location),
                                       selected = "Charlotte, North Carolina, United States")),
                 column(6,
                        align = "center",
                        selectizeInput("city2", "Select Comparison City",
                                       choices = unique(temp$location),
                                       selected = "Yellowknife, Northwest Territories, Canada"))
        ),


        # Main panel for displaying outputs ----
        mainPanel(width = 12,

          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Temp",
                               fluidRow(column(6, style = 'padding:0px',
                                               plotOutput("temp1") %>% withSpinner()),
                                        column(6, style = 'padding:0px',
                                               plotOutput("temp2") %>% withSpinner()))
                              ),
                      tabPanel("Precip.",
                               fluidRow(column(6, plotOutput("precip1")%>% withSpinner()),
                                        column(6, plotOutput("precip2")%>% withSpinner()))
                               ),
                      tabPanel("Sun",
                                fluidRow(column(6, plotOutput("sun1")%>% withSpinner()),
                                         column(6, plotOutput("sun2")%>% withSpinner()))
                              ),
                      tabPanel("Comfort",
                               fluidRow(column(6, plotOutput("comfort1")%>% withSpinner()),
                                        column(6, plotOutput("comfort2")%>% withSpinner()))
                      ),
                      tabPanel("Clusters",
                               fluidRow(column(12, plotlyOutput("clusterChart", width = "80%")%>% withSpinner()))
                      ),
                      tabPanel("Map",  fluidRow(column(width = 8,
                                                       offset = 3,
                                                       plotOutput("map1", width = "600px")%>% withSpinner())
                      )),
                      tabPanel("Records",
                               includeMarkdown("records.md")
                      ),
                      tabPanel("Notes",
                               includeMarkdown("notes.md")
                      )
          )
          # hr(),
          # print("~~~my disclaimer~~~~")
        ))
}

server <- function(input, output, session) {

    #################################################################################

    geo1Data <- reactive({
        geo %>% filter(location %in% c(input$city1, input$city2))
    })

    temp1Data <- reactive({
        temp %>%
        filter(location == input$city1) %>%
        mutate(date = as.Date(paste0("2018-",month,"-01"),"%Y-%b-%d"))
    })

    temp2Data <- reactive({
      temp %>%
        filter(location == input$city2) %>%
        mutate(date = as.Date(paste0("2018-",month,"-01"),"%Y-%b-%d"))
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

    comfort1Data <- reactive({
      comfort %>% filter(location == input$city1)
    })

    comfort2Data <- reactive({
      comfort %>% filter(location == input$city2)
    })

    clusterData <- reactive({
        cluster %>% filter(location %in% c(input$city1, input$city2))
    })

    #################################################################################

    output$map1 <- renderPlot({
        ggplot() +
        geom_polygon(data = global, aes(x=long, y = lat, group = group), fill = "#073642") +
        geom_point(data=geo1Data(),
                   aes(lon, lat),
                   color = "#CD2C24",
                   size = 3,
                   shape = 25,
                   fill = "#CD2C24") +
        geom_label(data=geo1Data(),
                   aes(lon, lat, label = location), nudge_y = 6) +
        labs(x="", y="") +
        ggthemes::theme_solarized() %+replace%
        theme(
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank())
    })

    output$temp1 <- renderPlot({

        temp_max = max(max(temp1Data()$temp_high_rec), max(temp2Data()$temp_high_rec))
        temp_min = min(min(temp1Data()$temp_low_rec), min(temp2Data()$temp_low_rec))

        ggplot(temp1Data(), aes(x = date)) +
          geom_hline(yintercept = 32, color = "#D9D1C7") +

          geom_ribbon(aes(ymax = temp_high_rec, ymin = temp_high_avg), fill = "#871528", alpha = 0.2) +
          geom_ribbon(aes(ymax = temp_high_avg, ymin = temp_low_avg), fill = "#CC211B", alpha = 0.6) +
          geom_ribbon(aes(ymax = temp_low_avg, ymin = temp_low_rec), fill = "#871528", alpha = 0.2) +

          geom_label(aes(date, y = temp_high_rec, label = temp_high_rec, fill = temp_high_rec, color = temp_high_rec), label.size = 0, size = 4.8) +
          geom_label(aes(date, y = temp_high_avg, label = temp_high_avg, fill = temp_high_avg, color = temp_high_avg), label.size = 0, size = 4.8) +
          geom_label(aes(date, y = temp_low_avg, label = temp_low_avg, fill = temp_low_avg, color = temp_low_avg), label.size = 0, size = 4.8) +
          geom_label(aes(date, y = temp_low_rec, label = temp_low_rec, fill = temp_low_rec, color = temp_low_rec), label.size = 0, size = 4.8) +

          scale_fill_gradientn(colors = viridis::viridis_pal(option = "magma")(10), limits=c(temp_min, temp_max), na.value = "#FDE725FF") +
          scale_color_gradientn(colors = viridis::viridis_pal(option = "viridis", direction = -1)(10), limits=c(temp_min, temp_max), na.value = "#FDE725FF") +

          # scale_x_date(labels = scales::date_format("%b"), breaks = scales::date_breaks("months")) +
          scale_x_date(date_labels = "%b", date_breaks = "1 month") +
          scale_y_continuous(limits = c(temp_min, temp_max)) +
          guides(fill = FALSE, color = FALSE) +
          labs(title = temp1Data()$location,
               subtitle = "",
               caption = "") +
          ggthemes::theme_solarized() %+replace%
          theme(
            plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
            panel.border = element_blank(),
            panel.background = element_blank(),
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank()
          )

    })

    output$temp2 <- renderPlot({

        temp_max = max(max(temp1Data()$temp_high_rec), max(temp2Data()$temp_high_rec))
        temp_min = min(min(temp1Data()$temp_low_rec), min(temp2Data()$temp_low_rec))

        ggplot(temp2Data(), aes(x = date)) +
          geom_hline(yintercept = 32, color = "#D9D1C7") +
          geom_ribbon(aes(ymax = temp_high_rec, ymin = temp_high_avg), fill = "#871528", alpha = 0.2) +
          geom_ribbon(aes(ymax = temp_high_avg, ymin = temp_low_avg), fill = "#CC211B", alpha = 0.6) +
          geom_ribbon(aes(ymax = temp_low_avg, ymin = temp_low_rec), fill = "#871528", alpha = 0.2) +

          geom_label(aes(date, y = temp_high_rec, label = temp_high_rec, fill = temp_high_rec, color = temp_high_rec), label.size = 0, size = 4.8) +
          geom_label(aes(date, y = temp_high_avg, label = temp_high_avg, fill = temp_high_avg, color = temp_high_avg), label.size = 0, size = 4.8) +
          geom_label(aes(date, y = temp_low_avg, label = temp_low_avg, fill = temp_low_avg, color = temp_low_avg), label.size = 0, size = 4.8) +
          geom_label(aes(date, y = temp_low_rec, label = temp_low_rec, fill = temp_low_rec, color = temp_low_rec), label.size = 0, size = 4.8) +

          scale_fill_gradientn(colors = viridis::viridis_pal(option = "magma")(10), limits=c(temp_min, temp_max), na.value = "#FDE725FF") +
          scale_color_gradientn(colors = viridis::viridis_pal(option = "viridis", direction = -1)(10), limits=c(temp_min, temp_max), na.value = "#FDE725FF") +

          scale_x_date(date_labels = "%b", date_breaks = "1 month") +
          scale_y_continuous(limits = c(temp_min, temp_max)) +

          guides(fill = FALSE, color = FALSE) +

            labs(title = temp2Data()$location,
                 subtitle = "",
                 caption = "Source: Wikipedia") +

          ggthemes::theme_solarized() %+replace%
          theme(
              plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank()
          )
    })

    output$precip1 <- renderPlot({

        precip_max = max(max(precip1Data()$data_value),max(precip2Data()$data_value))

        ggplot(precip1Data(), aes(month, data_value, fill=data_key)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = c("#268bd2","#ADD5F7"),
                              name = "Type",
                              labels=c("Rain", "Snow")) +
            ylim(0, precip_max) +
            labs(title = temp1Data()$location,
                 subtitle = "",
                 caption = "",
                 y = "Precipitation (Inches)", x = "Month") +

            ggthemes::theme_solarized() %+replace%
            theme(
                plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 14),
              #axis.title.y = element_blank(),
              #axis.ticks.y = element_blank(),
              #axis.text.y = element_blank(),
              axis.line.y = element_blank(),
              legend.text = element_text(size = 14, color = "#073642"),
              legend.position = "bottom"
            )
    })

    output$sun1 <- renderPlot({

      ggplot(sun1Data(), aes(x = date)) +
        geom_area(aes(y = data_value, fill = data_key, alpha = data_key)) +
        # scale_x_date(labels = scales::date_format("%b"), breaks = scales::date_breaks("months")) +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        scale_fill_manual(values = c("#051026", "#93a1a1", "#FCD215"),
                          name = element_blank(),
                          labels=c("Night", "Cloudy", "Sunny")) +
        scale_alpha_manual(values = c(1,0.7,1),guide=F) +
            labs(title = temp1Data()$location,
                 subtitle = "",
                 caption = "") +
        ggthemes::theme_solarized() %+replace%
        theme(
            plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          legend.text = element_text(size = 14, color = "#073642"),
          legend.position = "bottom"
        )

    })

    output$precip2 <- renderPlot({

        precip_max = max(max(precip1Data()$data_value), max(precip2Data()$data_value))
        normalizer = max(precip2Data()$data_value) / max(sun2Data()$data_value)

        ggplot(precip2Data(), aes(month, data_value, fill=data_key)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = c("#268bd2","#ADD5F7"),
                              name = "Type",
                              labels=c("Rain", "Snow")) +
            ylim(0, precip_max) +
            labs(title = temp2Data()$location,
                 subtitle = "",
                 caption = "Source: Wikipedia",
                 y = "Precipitation (Inches)", x = "Month") +
            ggthemes::theme_solarized() %+replace%
            theme(
                plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.line.y = element_blank(),
              legend.text = element_text(size = 14, color = "#073642"),
              legend.position = "bottom"
            )


    })

    output$sun2 <- renderPlot({
      ggplot(sun2Data(), aes(x = date)) +
        geom_area(aes(y = data_value, fill = data_key, alpha=data_key)) +
        # scale_x_date(labels = scales::date_format("%b"), breaks = scales::date_breaks("months")) +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        scale_fill_manual(values = c("#051026", "#93a1a1", "#FCD215"),
                          name = element_blank(),
                          labels=c("Night", "Cloudy", "Sunny")) +
        scale_alpha_manual(values = c(1,0.7,1),guide=F) +
            labs(title = temp2Data()$location,
                 subtitle = "",
                 caption = "Source: Wikipedia") +
        ggthemes::theme_solarized() %+replace%
        theme(
            plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          legend.text = element_text(size = 14, color = "#073642"),
          legend.position = "bottom"
        )
    })

    output$comfort1 <- renderPlot({

      ggTimeSeries::ggplot_calendar_heatmap(comfort1Data(), 'date', 'ci') +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red",
                             limits = c(min(comfort$ci), max(comfort$ci)),
                             na.value = "grey") +
        guides(fill = FALSE, color = FALSE) +
            labs(title = temp1Data()$location,
                 subtitle = "") +
        ggthemes::theme_solarized() %+replace%
        theme(
            plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
          #panel.border = element_rect(color = "#fdf6e3"),
          #panel.background = element_rect(color = "#fdf6e3"),
          #plot.background = element_rect(color = "#fdf6e3"),
          panel.grid = element_line(color = "#fdf6e3"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank()
        )

    })

    output$comfort2 <- renderPlot({

      ggTimeSeries::ggplot_calendar_heatmap(comfort2Data(), 'date', 'ci') +
        scale_fill_gradient2(low = "blue", mid = "green", high = "red",
                             midpoint = 0,
                             na.value = "grey") +
        guides(fill = FALSE, color = FALSE) +
            labs(title = temp2Data()$location,
                 subtitle = "") +
        ggthemes::theme_solarized() %+replace%
        theme(
            plot.title = element_text(size = 18, face="bold", margin = margin(b = 2)),
          panel.border = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank()
        )

    })

    output$clusterChart <- renderPlotly({
        plot_ly(data = cluster,
                x = ~precip,
                y = ~avg_temp,
                color = ~cluster,
                colors = c("#268bd2", "#073642", "#859900", "#d33682", "#cb4b16"),
                text = ~location,
                hoverinfo = 'text',
                type = "scatter",
                mode = "markers") %>%
            layout(yaxis = list(title = "Temp -->",
                                showticklabels = FALSE),
                   xaxis = list(title = "Precipitation -->",
                                showticklabels = FALSE),
                   plot_bgcolor = "#fdf6e3",
                   paper_bgcolor = "#fdf6e3",
                   annotations = list(x = clusterData()$precip,
                                      y = clusterData()$avg_temp,
                                      text = clusterData()$location,
                                      # bordercolor='#c7c7c7',
                                      borderwidth=0,
                                      borderpad=2,
                                      bgcolor="white",
                                      opacity=0.9))
    })

}
# Run the application
shinyApp(ui = ui, server = server)

