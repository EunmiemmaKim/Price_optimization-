if(!require(tidyverse)) { install.packages("tidyverse"); require(Rsolnp)}
if(!require(shiny)) { install.packages("shiny"); require(Rsolnp)}
if(!require(plotly)) { install.packages("plotly"); require(Rsolnp)}
if(!require(ggplot2)) { install.packages("ggplot2"); require(Rsolnp)}
if(!require(gridExtra)) { install.packages("gridExtra"); require(Rsolnp)}
if(!require(Rsolnp)) { install.packages("Rsolnp"); require(Rsolnp)}
if(!require(gtools)) { install.packages("gtools"); require(Rsolnp)}
if(!require(lubridate)) { install.packages("lubridate"); require(Rsolnp)}
if(!require(shinydashboard)) { install.packages("xts"); require(shinydashboard)}
if(!require(DT)) { install.packages("xts"); require(DT)}
if(!require(ggpubr)) { install.packages("xts"); require(ggpubr)}
if(!require(mgcv)) { install.packages("xts"); require(mgcv)}
if(!require(xts)) { install.packages("xts"); require(xts)}
if(!require(remotes)) { install.packages("remotes"); require(remotes)}
if(!require(dashboardthemes)) {remotes::install_github("nik01010/dashboardthemes"); require(dashboardthemes)}


demo <- read.csv("demographics.csv")
event <- read.csv("event_calendar.csv")
historic <- read.csv("historical_volume.csv")
industry_sales <- read.csv("industry_soda_sales.csv")
industry_volume <- read.csv("industry_volume.csv")
industry <- merge(industry_sales,industry_volume, by = "YearMonth")
industry$marketshare <- industry$Industry_Volume/industry$Soda_Volume
price_sales <- read.csv("price_sales_promotion.csv")

price_sales$promotionpct <- price_sales$Promotions/price_sales$Price
weather <- read.csv("weather.csv")
weather

price_volume <- merge(price_sales,historic, by = c("SKU","YearMonth","Agency"))

price_volume_weather <- merge(price_volume,weather, by=c("YearMonth","Agency"))

price_volume_weather_event <- merge(price_volume_weather,event,by = "YearMonth")

df <- price_volume_weather_event
df$Agency <- as.character(df$Agency)
df$SKU <- as.character(df$SKU)
df$Revenue <- df$Sales * df$Volume

SKU <- df$SKU %>% unique() %>% sort()
agency1 <- df$Agency %>%  unique() %>% sort()
agency1 <- append(agency1,"Total Market")

hist(df$Volume)
hist(df$Sales)

hist(df$Avg_Max_Temp)

df$YearMonth<- as.yearmon(as.character(df$YearMonth), "%Y%m")
str(df)

# UI for pricing opimization

ui <- dashboardPage( 
  
  dashboardHeader(title = "Stallion & Co.’s global beer Price Opimiztion !!",
                  
                  titleWidth = 450),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(shinyDashboardThemes(theme = "grey_dark"),
                
                tags$head(tags$style(HTML('
                              
                               /* logo */
                              
                              .skin-blue .main-header .logo {
                              
                              background-color: #AF1E2D;
                              
                              font-family: Arial;
                              
                              }
                              
                              /* navbar (rest of the header) */
                              
                              .skin-blue .main-header .navbar {
                              
                              background-color: #AF1E2D;
                              
                              }'
                ))),
                
                fluidRow(
                  
                  
                  column(width = 2,
                         
                         box(selectInput("demand", "Choose Demand Function:",
                                         
                                         c("Linear" = "fit_linear",
                                           
                                           "B-Spline (Degree=3)" = "fit_spline",
                                           
                                           "Generalized Additive Models" = "fit_gam")),
                             
                             width=NULL),
                         
                         
                         
                         box(selectInput("sku", "SKU:",
                                         choices =  SKU),
                             width=NULL),
                         
                         box(selectInput("agency", "Agency:",
                                         
                                         choices = agency1, selected = "Total Market"),
                             
                             width=NULL),
                         
                         box(numericInput("temp_num", label="Input temperature (Ceisius)", value = 25),
                             fluidRow(
                               column(width = 1,
                                      actionButton("notemp", "No Effect")),
                               column(width = 1, offset = 3,
                                      actionButton("temp", "Temperature Effect"))), 
                             width=NULL),
                         
                         box(title=strong('Purpose: '), width=NULL,
                             "we are going to measure how demand changes in response to a price change. 
                      This can be helpful for the company to maximize their revenue. 
                      To maximize revenue, we are going to find optimization prices for each SKU level and each agency 
                      with a different temperature using three methods ( Linear regression, Spline regression, 
                      and Generalized Additive Models ) ")
                         
                  ),
                  
                  
                  column(width = 6,
                         
                         box(plotlyOutput("demand_plots"),
                             
                             width = NULL),
                         
                         box(plotOutput("time_plots1",height="500px"),
                             width = NULL)),
                  
                  column(width = 4,
                         
                         box( uiOutput("vbox1",height="400px")
                              ,width=NULL),
                         
                         
                         
                         # box(dataTableOutput("result_table"),
                         #     
                         #     width=NULL),
                         
                         box(verbatimTextOutput('model_print'),
                             
                             width=NULL),
                         
                         box(img(src = "https://www.cga.co.uk/wp-content/uploads/2019/08/iStock-1040303026-1.jpg", height = 220,width=550),width=NULL))
                  
                  
                )
                
  )
  
)



server <- function(input, output,session) {
  
  
  observe({
    
    data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
    agency1 <- data$Agency %>% unique() %>% sort()
    agency1 <- append(agency1,"Total Market")
    
    updateSelectInput(session,
                      "agency", "Agency:",
                      choices = agency1)
  })
  
  
  observeEvent( 
    input$notemp, 
    
    {
      
      output$demand_plots <- renderPlotly({
        
        if (input$demand == "fit_linear") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #linear regression
            l_model <- lm(Volume ~ Sales, data=sku_agency)
            sum_l_model <-summary(l_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1') + geom_smooth(method = "lm")
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.border=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.grid.major=element_blank(),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'),
                             axis.title.x = element_text(color="white", size=14, face="bold"),
                             axis.title.y = element_text(color="white", size=14, face="bold"))+
                       labs(x="Sales", y="Volume"))
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #linear regression
            l_model <- lm(Volume ~ Sales, data=sku_agency)
            sum_l_model <-summary(l_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1') + geom_smooth(method = "lm")
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.border=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.grid.major=element_blank(),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'),
                             axis.title.x = element_text(color="white", size=14, face="bold"),
                             axis.title.y = element_text(color="white", size=14, face="bold"))+
                       labs(x="Sales", y="Volume"))
            
            
          } 
          
        }
        
        
        else if (input$demand == "fit_spline") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3))
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.border=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.grid.major=element_blank(),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'),
                             axis.title.x = element_text(color="white", size=14, face="bold"),
                             axis.title.y = element_text(color="white", size=14, face="bold"))+
                       labs(x="Sales", y="Volume"))
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data  %>%  filter(Agency %in% input$agency)
            
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3))
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.border=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.grid.major=element_blank(),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'),
                             axis.title.x = element_text(color="white", size=14, face="bold"),
                             axis.title.y = element_text(color="white", size=14, face="bold"))+
                       labs(x="Sales", y="Volume"))
            
            
          } 
        }
        
        else if (input$demand == "fit_gam") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data
            
            #GAM 
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_smooth(method = "gam", formula = y ~ s(x,bs="cr"))
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.border=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.grid.major=element_blank(),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'),
                             axis.title.x = element_text(color="white", size=14, face="bold"),
                             axis.title.y = element_text(color="white", size=14, face="bold"))+
                       labs(x="Sales", y="Volume"))
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data  %>%  filter(Agency %in% input$agency)
            
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_smooth(method = "gam", formula = y ~ s(x,bs="cr"))
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.border=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             panel.grid.major=element_blank(),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'),
                             axis.title.x = element_text(color="white", size=14, face="bold"),
                             axis.title.y = element_text(color="white", size=14, face="bold"))+
                       labs(x="Sales", y="Volume"))
            
            
          } 
        }
        
      })
      
      
      output$time_plots1 <- renderPlot({
        
        if (input$agency == "Total Market") {
          
          data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
          sku_agency <- data 
          
          plot1 <- ggplot(data = sku_agency,aes(x = Sales , y =Revenue)) + geom_line(color = 'tomato1') + ggtitle("Revenue by Sales") +
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.border=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.grid.major=element_blank(),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'),
                  axis.title.x = element_text(color="white", size=14, face="bold"),
                  axis.title.y = element_text(color="white", size=14, face="bold"))
          
          
          plot2 <- ggplot(data = sku_agency,aes(x = YearMonth , y =Volume)) + geom_line(color = 'tomato1') + ggtitle("Change of Volume over time")+
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.border=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.grid.major=element_blank(),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'),
                  axis.title.x = element_text(color="white", size=14, face="bold"),
                  axis.title.y = element_text(color="white", size=14, face="bold"))
          
          ggarrange(plot1,plot2,nrow = 2) }
        
        else {
          
          data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
          sku_agency <- data %>%  filter(Agency %in% input$agency)
          
          plot1 <- ggplot(data = sku_agency,aes(x = Sales , y =Revenue)) + geom_line(color = 'tomato1')+ ggtitle("Revenue by Sales") +
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.border=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.grid.major=element_blank(),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'),
                  axis.title.x = element_text(color="white", size=14, face="bold"),
                  axis.title.y = element_text(color="white", size=14, face="bold"))
          
          
          plot2 <- ggplot(data = sku_agency,aes(x = YearMonth , y =Volume)) + geom_line(color = 'tomato1')+ggtitle("Change of Volume over time")+
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.border=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  panel.grid.major=element_blank(),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'),
                  axis.title.x = element_text(color="white", size=14, face="bold"),
                  axis.title.y = element_text(color="white", size=14, face="bold"))
          
          ggarrange(plot1,plot2,nrow = 2)
          
        }
        
      })
      
      
      output$vbox1 = renderUI({
        
        if (input$demand == "fit_linear") 
          
        {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data
            sku_agency$lsales <- sku_agency$Sales %>% log()
            sku_agency$lvolume <- sku_agency$Volume %>% log()
            
            #linear regression
            l_model <- lm(Volume ~ Sales, data=sku_agency)
            sum_l_model <-summary(l_model)
            log_l_model <- lm(lvolume ~ lsales, data=sku_agency)
            log_sum_l_model <-summary(log_l_model)
            
            # Revenue Function
            revenue_lmfun = function(p){
              # Demand for linear model
              D <- predict(l_model,data.frame(Sales = c(p)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            
            lm_opt.result = gosolnp(pars= p,
                                    fun=revenue_lmfun,
                                    LB = c(0),
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_l_model$r.squared,2),
                                 Coeffcient = round(l_model$coefficients[2],2),
                                 Price_Elasticity = round(log_l_model$coefficients[2],2),
                                 Optimal_price = round(lm_opt.result[[1]],2),
                                 Optimal_Revnue = -round(lm_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       valueBox("Price Elasticity",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       
                       valueBox("Optimal Price",
                                value=result[1,4],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,5],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
          }          
          
          
          else  {
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            sku_agency$lsales <- sku_agency$Sales %>% log()
            sku_agency$lvolume <- sku_agency$Volume %>% log()
            
            #linear regression
            l_model <- lm(Volume ~ Sales, data=sku_agency)
            sum_l_model <-summary(l_model)
            log_l_model <- lm(lvolume ~ lsales, data=sku_agency)
            log_sum_l_model <-summary(log_l_model)
            
            # Revenue Function
            revenue_lmfun = function(p){
              # Demand for linear model
              D <- predict(l_model,data.frame(Sales = c(p)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            
            lm_opt.result = gosolnp(pars= p,
                                    fun=revenue_lmfun,
                                    LB = c(0),
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_l_model$r.squared,2),
                                 Coeffcient = round(l_model$coefficients[2],2),
                                 Price_Elasticity = round(log_l_model$coefficients[2],2),
                                 Optimal_price = round(lm_opt.result[[1]],2),
                                 Optimal_Revnue = -round(lm_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       valueBox("Price Elasticity",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       
                       valueBox("Optimal Price",
                                value=result[1,4],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,5],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
          }
        }
        
        
        else if (input$demand == "fit_spline") {
          
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            #Revenue Function
            revenue_spfun = function(p){
              # Demand for linear model
              D <- predict(sp_model,data.frame(Sales = c(p)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            sp_opt.result = gosolnp(pars= p,
                                    fun=revenue_spfun,
                                    LB = lb,
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_sp_model$r.squared,2),
                                 Otimal_Price = round(sp_opt.result[[1]],2),
                                 Optimal_Revenue = -round(sp_opt.result[[3]][1],2))
            
            
            
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            #Revenue Function
            revenue_spfun = function(p){
              # Demand for linear model
              D <- predict(sp_model,data.frame(Sales = c(p)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            sp_opt.result = gosolnp(pars= p,
                                    fun=revenue_spfun,
                                    LB = lb,
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_sp_model$r.squared,2),
                                 Otimal_Price = round(sp_opt.result[[1]],2),
                                 Optimal_Revenue = -round(sp_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
            
          }
          
        } 
        
        
        else if (input$demand == "fit_gam") {
          
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr"), data=sku_agency)
            sum_gam_model <- summary(gam_model)
            
            #Revenue Function
            revenue_gamfun = function(p){
              # Demand for linear model
              D <- predict(gam_model,data.frame(Sales = c(p)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            gam_opt.result = gosolnp(pars= p,
                                     fun=revenue_gamfun,
                                     LB = lb,
                                     UB = ub,
                                     n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_gam_model$r.sq,2),
                                 Otimal_Price = round(gam_opt.result[[1]],2),
                                 Optimal_Revenue = -round(gam_opt.result[[3]][1],2))
            
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr"), data=sku_agency)
            sum_gam_model <- summary(gam_model)
            
            #Revenue Function
            revenue_gamfun = function(p){
              # Demand for linear model
              D <- predict(gam_model,data.frame(Sales = c(p)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            gam_opt.result = gosolnp(pars= p,
                                     fun=revenue_gamfun,
                                     LB = lb,
                                     UB = ub,
                                     n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_gam_model$r.sq,2),
                                 Otimal_Price = round(gam_opt.result[[1]],2),
                                 Optimal_Revenue = -round(gam_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
            
          }
          
        } 
        
      })
      
      
      output$model_print = renderPrint({
        
        if (input$demand == "fit_linear") {
          
          if (input$agency == "Total Market" ) {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #linear regression
            l_model <- lm(Volume ~ Sales, data=sku_agency)
            summary(l_model)
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #linear regression
            l_model <- lm(Volume ~ Sales, data=sku_agency)
            summary(l_model)
          }
          
        }  
        
        else if (input$demand == "fit_spline") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            summary(sp_model)
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3), data=sku_agency)
            summary(sp_model)
            
          }
          
        } 
        
        else if (input$demand == "fit_gam") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            gam_model <- gam(Volume ~ s(Sales,bs="cr"), data=sku_agency)
            summary(gam_model)
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr"), data=sku_agency)
            summary(gam_model)
            
          }
          
        }
      })
    }
  )
  
  observeEvent( 
    input$temp, 
    
    {
      
      
      output$demand_plots <- renderPlotly({
        
        if (input$demand == "fit_linear") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #linear regression
            l_model <- lm(Volume ~ Sales + Avg_Max_Temp, data=sku_agency)
            sum_l_model <-summary(l_model)
            
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1') + geom_point(aes(y=l_model$fitted.values),alpha =0.8,color = 'skyblue')
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             legend.background = element_rect(fill="#2C3E4F"),
                             legend.key = element_rect(fill = "#2C3E4F", color = NA),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'))+
                       labs(x="Sales", y="Volume"))
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data  %>%  filter(Agency %in% input$agency)
            
            #linear regression
            l_model <- lm(Volume ~ Sales + Avg_Max_Temp, data=sku_agency)
            sum_l_model <-summary(l_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1') + geom_point(aes(y=l_model$fitted.values),alpha =0.8,color = 'skyblue')
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             legend.background = element_rect(fill="#2C3E4F"),
                             legend.key = element_rect(fill = "#2C3E4F", color = NA),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'))+
                       labs(x="Sales", y="Volume"))
            
            
          } 
          
        }
        
        
        else if (input$demand == "fit_spline") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3) + Avg_Max_Temp , data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_point(aes(y=sp_model$fitted.values),alpha =0.8,color = 'skyblue')
                     + ggtitle("Demand Function") +
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             legend.background = element_rect(fill="#2C3E4F"),
                             legend.key = element_rect(fill = "#2C3E4F", color = NA),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'))+
                       labs(x="Sales", y="Volume"))
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data  %>%  filter(Agency %in% input$agency)
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3) + Avg_Max_Temp, data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_point(aes(y=sp_model$fitted.values),alpha =0.8,color = 'skyblue')
                     + ggtitle("Demand Function") + 
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             legend.background = element_rect(fill="#2C3E4F"),
                             legend.key = element_rect(fill = "#2C3E4F", color = NA),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'))+
                       labs(x="Sales", y="Volume"))
            
            
          } 
        }
        
        else if (input$demand == "fit_gam") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr") + Avg_Max_Temp, data=sku_agency)
            sum_gam_model <- summary(gam_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_point(aes(y=gam_model$fitted.values),alpha =0.8,color = 'skyblue')
                     + ggtitle("Demand Function") +
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             legend.background = element_rect(fill="#2C3E4F"),
                             legend.key = element_rect(fill = "#2C3E4F", color = NA),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'))+
                       labs(x="Sales", y="Volume"))
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data  %>%  filter(Agency %in% input$agency)
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr") + Avg_Max_Temp, data=sku_agency)
            sum_gam_model <- summary(gam_model)
            
            ggplotly(ggplot(data = sku_agency,aes(x =Sales , y =Volume)) 
                     + geom_point(color = 'tomato1')
                     + geom_point(aes(y=gam_model$fitted.values),alpha =0.8,color = 'skyblue')
                     + ggtitle("Demand Function") +
                       theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                             panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             legend.background = element_rect(fill="#2C3E4F"),
                             legend.key = element_rect(fill = "#2C3E4F", color = NA),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                             axis.text.x = element_text(face="bold",color='white'),
                             axis.text.y = element_text(face="bold",color='white'))+
                       labs(x="Sales", y="Volume"))
            
            
          } 
        }
        
      })
      
      
      output$time_plots1 <- renderPlot({
        
        if (input$agency == "Total Market") {
          
          data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
          sku_agency <- data 
          
          plot1 <- ggplot(data = sku_agency,aes(x = Sales , y =Revenue)) + geom_line(color = 'tomato1') + ggtitle("Revenue by Sales")+
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  legend.background = element_rect(fill="#2C3E4F"),
                  legend.key = element_rect(fill = "#2C3E4F", color = NA),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'))
          
          
          plot2 <- ggplot(data = sku_agency,aes(x = YearMonth , y =Volume)) + geom_line(color = 'tomato1') + ggtitle("Change of Volume over time")+
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  legend.background = element_rect(fill="#2C3E4F"),
                  legend.key = element_rect(fill = "#2C3E4F", color = NA),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'))
          ggarrange(plot1,plot2,nrow = 2) }
        
        else {
          
          data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
          sku_agency <- data %>%  filter(Agency %in% input$agency)
          
          plot1 <- ggplot(data = sku_agency,aes(x = Sales , y =Revenue)) + geom_line(color = 'tomato1')+ggtitle("Revenue by Sales")+
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  legend.background = element_rect(fill="#2C3E4F"),
                  legend.key = element_rect(fill = "#2C3E4F", color = NA),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'))
          
          
          plot2 <- ggplot(data = sku_agency,aes(x = YearMonth , y =Volume)) + geom_line(color = 'tomato1')+ggtitle("Change of Volume over time")+
            theme(plot.title = element_text(size=15, face='bold',hjust = 0.5,color='white'),
                  panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  legend.background = element_rect(fill="#2C3E4F"),
                  legend.key = element_rect(fill = "#2C3E4F", color = NA),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
                  axis.text.x = element_text(face="bold",color='white'),
                  axis.text.y = element_text(face="bold",color='white'))
          
          ggarrange(plot1,plot2,nrow = 2)
          
        }
        
      })
      
      
      output$vbox1 = renderUI({
        
        if (input$demand == "fit_linear") 
          
        {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data
            sku_agency$lsales <- sku_agency$Sales %>% log()
            sku_agency$lvolume <- sku_agency$Volume %>% log()
            
            #linear regression
            l_model <- lm(Volume ~ Sales + Avg_Max_Temp, data=sku_agency)
            sum_l_model <-summary(l_model)
            log_l_model <- lm(lvolume ~ lsales + Avg_Max_Temp, data=sku_agency)
            log_sum_l_model <-summary(log_l_model)
            
            # Revenue Function
            revenue_lmfun = function(p){
              # Demand for linear model
              D <- predict(l_model,data.frame(Sales = c(p),Avg_Max_Temp= c(input$temp_num)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            
            lm_opt.result = gosolnp(pars= p,
                                    fun=revenue_lmfun,
                                    LB = c(0),
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_l_model$r.squared,2),
                                 Coeffcient = round(l_model$coefficients[2],2),
                                 Price_Elasticity = round(log_l_model$coefficients[2],2),
                                 Optimal_price = round(lm_opt.result[[1]],2),
                                 Optimal_Revnue = -round(lm_opt.result[[3]][1],2))
            
            
            fluidRow(
              column(width=12,  
                     box(
                       valueBox("Price Elasticity",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       
                       valueBox("Optimal Price",
                                value=result[1,4],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,5],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
          } 
          
          else  {
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            sku_agency$lsales <- sku_agency$Sales %>% log()
            sku_agency$lvolume <- sku_agency$Volume %>% log()
            
            #linear regression
            l_model <- lm(Volume ~ Sales + Avg_Max_Temp, data=sku_agency)
            sum_l_model <-summary(l_model)
            log_l_model <- lm(lvolume ~ lsales + Avg_Max_Temp, data=sku_agency)
            log_sum_l_model <-summary(log_l_model)
            
            # Revenue Function
            revenue_lmfun = function(p){
              # Demand for linear model
              D <- predict(l_model,data.frame(Sales = c(p),Avg_Max_Temp= c(input$temp_num)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            
            lm_opt.result = gosolnp(pars= p,
                                    fun=revenue_lmfun,
                                    LB = c(0),
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_l_model$r.squared,2),
                                 Coeffcient = round(l_model$coefficients[2],2),
                                 Price_Elasticity = round(log_l_model$coefficients[2],2),
                                 Optimal_price = round(lm_opt.result[[1]],2),
                                 Optimal_Revnue = -round(lm_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       valueBox("Price Elasticity",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       
                       valueBox("Optimal Price",
                                value=result[1,4],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,5],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
          }
        }
        
        
        else if (input$demand == "fit_spline") {
          
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3) + Avg_Max_Temp, data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            #Revenue Function
            revenue_spfun = function(p){
              # Demand for linear model
              D <- predict(sp_model,data.frame(Sales = c(p),Avg_Max_Temp= c(input$temp_num)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            sp_opt.result = gosolnp(pars= p,
                                    fun=revenue_spfun,
                                    LB = lb,
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_sp_model$r.squared,2),
                                 Otimal_Price = round(sp_opt.result[[1]],2),
                                 Optimal_Revenue = -round(sp_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3) + Avg_Max_Temp, data=sku_agency)
            sum_sp_model <- summary(sp_model)
            
            #Revenue Function
            revenue_spfun = function(p){
              # Demand for linear model
              D <- predict(sp_model,data.frame(Sales = c(p),Avg_Max_Temp= c(input$temp_num)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            sp_opt.result = gosolnp(pars= p,
                                    fun=revenue_spfun,
                                    LB = lb,
                                    UB = ub,
                                    n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_sp_model$r.squared,2),
                                 Otimal_Price = round(sp_opt.result[[1]],2),
                                 Optimal_Revenue = -round(sp_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
          }
          
        } 
        
        else if (input$demand == "fit_gam") {
          
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr") + Avg_Max_Temp, data=sku_agency)
            sum_gam_model <- summary(gam_model)
            
            #Revenue Function
            revenue_gamfun = function(p){
              # Demand for linear model
              D <- predict(gam_model,data.frame(Sales = c(p),Avg_Max_Temp= c(input$temp_num)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            gam_opt.result = gosolnp(pars= p,
                                     fun=revenue_gamfun,
                                     LB = lb,
                                     UB = ub,
                                     n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_gam_model$r.sq,2),
                                 Otimal_Price = round(gam_opt.result[[1]],2),
                                 Optimal_Revenue = -round(gam_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr") + Avg_Max_Temp, data=sku_agency)
            sum_gam_model <- summary(gam_model)
            
            #Revenue Function
            revenue_gamfun = function(p){
              # Demand for linear model
              D <- predict(gam_model,data.frame(Sales = c(p),Avg_Max_Temp= c(input$temp_num)))
              # REVENUE
              revenue = D * p
              return(-revenue)
            }
            
            p <- c(median(sku_agency$Sales))
            ub<- c(max(sku_agency$Sales))
            lb<- c(min(sku_agency$Sales))
            
            gam_opt.result = gosolnp(pars= p,
                                     fun=revenue_gamfun,
                                     LB = lb,
                                     UB = ub,
                                     n.restarts = 1, n.sim = 20000)
            
            result <- data.frame(R2 = round(sum_gam_model$r.sq,2),
                                 Otimal_Price = round(gam_opt.result[[1]],2),
                                 Optimal_Revenue = -round(gam_opt.result[[3]][1],2))
            
            fluidRow(
              column(width=12,  
                     box(
                       
                       valueBox("Optimal Price",
                                value=result[1,2],
                                width="200px",color="orange")
                     ),
                     
                     box(
                       valueBox("Maximum Revenue",
                                value=result[1,3],
                                width="200px",color="orange"),
                       
                       valueBox( "R-squared",
                                 value=result[1,1],
                                 width="200px",color="orange"))))
            
          }
          
        }
        
      })
      
      
      output$model_print = renderPrint({
        
        if (input$demand == "fit_linear") {
          
          if (input$agency == "Total Market" ) {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #linear regression
            l_model <- lm(Volume ~ Sales + Avg_Max_Temp, data=sku_agency)
            summary(l_model)
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #linear regression
            l_model <- lm(Volume ~ Sales + Avg_Max_Temp, data=sku_agency)
            summary(l_model)
          }
          
        }  
        
        else if (input$demand == "fit_spline") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3) + Avg_Max_Temp, data=sku_agency)
            summary(sp_model)
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #spline regression
            sp_model <- lm(Volume ~ splines::bs(Sales, 3) + Avg_Max_Temp, data=sku_agency)
            summary(sp_model)
            
          }
          
        } 
        
        else if (input$demand == "fit_gam") {
          
          if (input$agency == "Total Market") {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data 
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr") + Avg_Max_Temp, data=sku_agency)
            summary(gam_model)
            
          }
          
          else {
            
            data <- df  %>% filter(SKU == input$sku) %>% filter(Sales != 0 & Volume !=0)
            sku_agency <- data %>%  filter(Agency %in% input$agency)
            
            #GAM
            gam_model <- gam(Volume ~ s(Sales,bs="cr") + Avg_Max_Temp, data=sku_agency)
            summary(gam_model)
            
          }
          
        } 
        
      })
      
      
    }
    
  )  
  
}


shinyApp(ui, server)

