library(glue)
library(highcharter)
library(shiny)
library(shinydashboard)
library(tidyverse)

load("data/sp_yearly_1_2.RData")

# FIXME: distinct shoulnd't be needed
dat <- dat %>% 
    dplyr::distinct()

# Read species definition data
sp_data <- readr::read_csv("data/Halias_sp_v1.2.csv") %>% 
    dplyr::arrange(Species_code)

spps <- sp_data$Sci_name

ui <- dashboardPage(
    dashboardHeader(title = "Halias observations"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidPage(
            fluidRow(
                column(6,
                       box(
                           width = 12,
                           column(6,
                                  selectInput("selector", "Select species", 
                                              choices = spps)
                           ),
                           column(6
                                  #checkboxInput("fixy", "Fix y-axis")
                           )
                       ),
                       box(
                           width = 12,
                           highchartOutput("migration", height = "300px")
                           
                       ),
                       box(
                           width = 12,
                           highchartOutput("local", height = "300px")
                       )
                ),
                column(6,
                       box(
                           width = 12,
                           uiOutput("text1")
                       )
                )
            )
        )
    )
)

server <- function(input, output) {
    
    output$text1 <- renderUI ({
        
        current_sp <- sp_data %>% 
            filter(Sci_name == input$selector)
        common_name <- current_sp$ENG_name
        sci_name <- current_sp$Sci_name
        
        withTags(
            div(
                h2(common_name),
                h4(sci_name)
            )
        )
        
        
    })
    
    output$migration <- renderHighchart({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        sp_name <- sp_current$Sci_name
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb) 
        
        # Make a subselectiong of the data containing two different epochs:
        # 1979-1999 and 2009-
        epochs <- obs_current %>% 
            dplyr::select(day, begin, end) %>% 
            tidyr::gather(variable, value, -day)
        #browser()
        hc <- obs_current %>% 
            hchart(type = "spline", 
                   hcaes(x = day, y = muutto),
                   color = c("#e5b13a", "#4bd5ee")) 
        
        return(hc)
    })
    
    output$local <- renderHighchart({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        sp_name <- sp_current$Sci_name
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb) 
        
        # Make a subselectiong of the data containing two different epochs:
        # 1979-1999 and 2009-
        epochs <- obs_current %>% 
            dplyr::select(day, begin, end) %>% 
            tidyr::gather(variable, value, -day)
        #browser()
        hc <- obs_current %>% 
            hchart(type = "spline", 
                   hcaes(x = day, y = paik),
                   color = c("#e5b13a", "#4bd5ee")) 
        
        return(hc)
    })
    
    output$plot1 <- renderPlot({
        
        sp_current <- sp_data %>% 
            dplyr::filter(Sci_name == input$selector)
        
        sp_name <- sp_current$Sci_name
        
        obs_current <- dat %>% 
            dplyr::filter(sp == sp_current$Species_Abb)
        
        # Make a subselectiong of the data containing two different epochs:
        # 1979-1999 and 2009-
        epochs <- obs_current %>% 
            dplyr::select(day, begin, end) %>% 
            tidyr::gather(variable, value, -day)
        
        
        p1 <- ggplot(obs_current, aes(x = day, y = paik)) +
            geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
            ggtitle(paste0(sp_name, ", Paikalliset / Lokal / Locals")) +
            scale_x_date(labels = date_format('%e %b')) + theme_bw()
        
        p2 <- ggplot(obs_current, aes(x = day, y = muutto)) +
            geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
            ggtitle(paste0(sp_name, ", Muuttavat / Flyttande / Migrants")) +
            scale_x_date(labels = date_format('%e %b')) + theme_bw()
        
        p3 <- ggplot(epochs, aes(x = day, y = value, color = variable)) +
            geom_line() + xlab("Day of Year") + ylab("Yks./pvm - Ind./day") +
            ggtitle("Muutos / Förändring / Change") +
            scale_color_manual(values = c("red", "blue"),
                               labels = c("1979-1999", "2009-")) +
            scale_x_date(labels = date_format('%e %b')) +
            theme_bw() + theme(legend.title = element_blank(), legend.position = c(0.9, 0.75))
        
        p4 <- grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)
    })
}

shinyApp(ui, server)
