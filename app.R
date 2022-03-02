library(shiny)
library(shinydashboard)
library(tidyverse)
library(viridis)
library(plotly)
library("ggplot2")
library(rsconnect)
data(diamonds)
head(diamonds)
set.seed(50)
small.diamonds <- diamonds[sample(nrow(diamonds),500),]
head(small.diamonds)


ui <- dashboardPage(
    # format
    skin="blue",
    
    # define the title
    dashboardHeader(
        title="Diamonds"
    ),
    
    # define the sidebar
    dashboardSidebar(
        # set sidebar menu  
        sidebarMenu(
            menuItem("Main", tabName = "Main"),
            menuItem("Price per carat", tabName = "per_carat"),
            menuItem("Diamonds of color E", tabName = "color_E")
        )
    ),
    
    # define the body
    dashboardBody(
        tabItems(
            tabItem( "Main",
                     h1( "Description" ),
                     p("Since the public evaluates diamond price based on color and carat, 
                       this shiny application is used to explore whether diamond prices are 
                       primarily related to these two factors or more closely related to clarity."),
                     p("This shiny application shows the average unit price of diamonds of different clarity and color, 
                       and the price of diamonds with E color changing with clarity and carat."),
                     p("Users can view diamond clarity, price, carat, and color data at each 
                       point on the tables to determine the price movement of the corresponding diamond."),
                     p("The second page is a histogram of the average unit price of diamonds of different colors and different clarity, 
                       and the third page is a detailed tabular data for diamonds with 
                       color E and a trend chart of the price of diamonds of different clarity with the number of carats.")
            ),
            
            tabItem("per_carat",
                    h2("Price per carat of different color and clarity"),
                    p("This histogram shows the average price per carat of a diamond of seven colors at different clarities. 
                      According to the plot, a diamond with clarity of VVS1 and color D has the highest price as compared to other 
                      diamonds, but if the color is J, its price will fall to the lowest. 
                      In general, in addition to diamonds with color E, the price of diamonds of other colors fluctuates greatly depending on the clarity."),
                    box(plotlyOutput("p_carat"), width= 500)
            ),
            
            tabItem("color_E",
                    h2("Price of diamonds of color E"),
                    h3("price by carat"),
                    p("This image shows the price movement of diamonds of color E with carats at different clarity. 
                      As shown in the plot, as the carat increases from 0.5 to 0.1, 
                      the gap among diamonds with clarity VVS2, VS1, and VS2 and other clarity diamonds gradually widens. 
                      Specifically, when the carat rises from 1.0 to 1.5, the price of a diamond with a clarity of 
                      VS1 rises in an approximately linear pattern. When the carat of a diamond with a clarity of VS2 
                      is greater than 1.11, the upward trend in price slows down, and its price drops significantly 
                      between 1.0 and 1.01 in carats. The price of diamonds with SI1 clarity began to recover gradually 
                      after experiencing a decline in carats between 1.50 and 1.51."),
                    box(plotlyOutput("p_E"), width= 500),
                    h3("precise data of diamonds of color E"),
                    box(dataTableOutput("t_E"), width= 500)
            )
        )
    )
    
)

server <- function(input, output) {
    
    
    
    output$p_carat <- renderPlotly({
        
        
        # plot
        
        p_carat <- ggplot(small.diamonds%>% 
                              group_by(color, clarity) %>%
                              summarise(n=mean(price/carat)),
                          aes(x=color,
                              y=n,
                              fill=clarity)) +
            geom_histogram(stat="identity",
                           position = position_dodge2(preserve = "single")) + 
            theme_minimal() +
            theme(axis.title = element_blank()) 
        
        # plotly for final graph
        ggplotly(p_carat)
        
    })
    
    output$p_E <- renderPlotly({
        
        p_E <- ggplot(small.diamonds %>% filter(color=="E"),
                      aes(x=carat,
                          y=price,
                          color=clarity)) +
            geom_line() + 
            scale_color_viridis(discrete = TRUE) +
            theme_minimal() +
            theme(axis.title = element_blank(),
                  legend.position = "none") + 
            scale_x_continuous(limits = c(min(small.diamonds$carat),NA))
        
    })
    
    output$t_E <- renderDataTable({
        
        t_E <- small.diamonds %>% filter(color=="E")
        t_E
        
    })
    
}
shinyApp(ui, server)
