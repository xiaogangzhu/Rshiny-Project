shinyUI(dashboardPage(
  dashboardHeader(title = "Ecommerce Analysis"),
  dashboardSidebar(
    sidebarUserPanel("Xiaogang Zhu", image = "https://cdn.acowebs.com/wp-content/uploads/2019/02/Impact-of-eCommerce-On-Society.png"),
    sidebarMenu(
      menuItem("Data Summary", tabName = "summary", icon = icon("chart-line")),
      menuItem("Funnel Analysis", tabName = "tunnel", icon = icon("funnel-dollar")),
      menuItem("User Active Pattern", tabName = "active", icon = icon("chart-bar")),
      menuItem("Product Analysis", tabName = "product", icon = icon("box-open")),
      menuItem("Detect Valuavle Users", tabName = "users", icon = icon("users")),
      menuItem("About me", tabName = "aboutme", icon = icon("address-card"))
      
      
    
  )),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "summary",
              tabsetPanel(type="tabs",
                          tabPanel("Summary",
                                   fluidRow(
                                     infoBoxOutput("UV"),
                                     infoBoxOutput("PV"),
                                     infoBoxOutput("PV.UV"),
                                     infoBoxOutput("sales"),
                                     infoBoxOutput("revenue"),
                                     infoBoxOutput("cr"),
                                     infoBoxOutput("rpr"))
                                   
                                   
                                   ),
                          tabPanel("Plot",
                                   fluidRow(
                                     column(width = 12,
                                            checkboxGroupInput("metrics","Select metrics by day",
                                                           choices = choice_var,
                                                           selected = c("UV","PV")))),
                                   fluidRow(box(width = 12,dygraphOutput('dygraph')))
                                   
                                   
                                   )
                
              )),
      
      tabItem(tabName = "tunnel",
              tabsetPanel(type = "tabs",
                    tabPanel("Funnel Analysis by Behaviour",
                      fluidRow( box(title = "View-Cart-Purchase",width = 12,plotlyOutput("tunnel1"))),
                      fluidRow( box(title = "View-Cart-remove_from_cart",width = 12,plotlyOutput("tunnel2")))
                      
                    
                     
                      
                      
                    ),
                    tabPanel("Funnel Analysis by Distinct Users",
                             column(width = 6 ,
                                    selectizeInput("overall","Select summary funnel path",
                                            choice = c("view-cart-purchase","view-cart-remove from cart"))),
                             
                             fluidRow(box(title = "Summary user funnel",width = 12,plotlyOutput("tunnel3"))),
                             column(width = 6 ,
                                    selectizeInput("path","Select specific funnel path",
                                                   choice = c("view-purchase","view-cart-purchase","view-cart-remove from cart"))),
                             fluidRow(box(title = "Specified funnel path",width = 12,plotlyOutput("tunnel4")))      
                                      
                                      )
                
                
                
                
              )),
      
      tabItem(tabName = "active",
            tabsetPanel(type = "tabs",
                        tabPanel("User Behaviour by month",
                                 fluidRow(htmlOutput("active_month"))
                                 
                                 ),
                        tabPanel("User Behaviour by week",
                                 selectizeInput("week", "Select week start by",
                                                choice = seq(as.Date("2019-12-01"), as.Date("2020-2-29"), by = "weeks")),
                                 fluidRow(htmlOutput("active_week"))
                                 
                                 ),
                        tabPanel("User Behaviour by day",
                                 selectizeInput("month", "Select month",
                                                choice = unique(df_tot$month)),
                                 fluidRow(htmlOutput("active_day"))
                                 
                                 
                                 ),
                        tabPanel("User Behaviour by hour",
                                 dateInput(inputId = "date",
                                           label = "Date",
                                           value = "2019-12-01",
                                           min = "2019-12-01",
                                           max = "2020-2-29"),
                                 
                                 fluidRow(htmlOutput("active_hour"))
                                 
                                 
                                 )
                        
                        
                        
                        
                        
                        )),
      
      tabItem(tabName = "product",
              column(width = 6,
                     selectizeInput("type", "Select type",
                             choice = unique(df_tot$event_type))),
              fluidRow(box(width = 12,plotlyOutput("category")),
                       box(width = 12,plotlyOutput('product')))
              
              
              ),
      
      tabItem(tabName = "users",
            "to be replaced with users"),
      tabItem(tabName = "aboutme","I am Xiaogang Zhu")
    )
  )
))