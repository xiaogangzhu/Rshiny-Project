shinyServer(function(input, output){
  df_summary <- reactive({
    df_tot %>% 
      summarise(UV = n_distinct(user_id),
                PV = sum(event_type =='view'),
                sales = sum(event_type== 'purchase'),
                )
  })
  
  
  output$UV <- renderInfoBox({
    UV = df_summary()[1,1]
    infoBox("User View",UV,icon = icon("user"))
  })
  
  output$PV <- renderInfoBox({
    PV = df_summary()[1,2]
    infoBox("Page View",PV,icon = icon("mouse"))
  })
  
  output$PV.UV <- renderInfoBox({
    infoBox("Page View per person",df_summary()[1,2]/df_summary()[1,1],icon = icon("percentage"))
  })
  
  output$sales <- renderInfoBox({
    infoBox("Sales",df_summary()[1,3],icon = icon("shopping-cart"))
  })
  
  output$revenue <- renderInfoBox({
    revenue = df_tot %>% 
      filter(event_type=='purchase') %>% 
      summarise(revenue = sum(price))
    infoBox("Revenue",revenue[1,1],icon = icon("dollar-sign"))
  })
  
  output$cr <- renderInfoBox({
    infoBox("Conversion Rate",paste0(round(df_tot_cr[1,4],4)*100,'%'),icon = icon("hand-o-up") )
  })
  
  output$rpr <- renderInfoBox({
    infoBox("Repurchase Rate",paste0(round(df_tot_cr[1,5],4)*100,'%'),icon = icon("exchange-alt"))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(tidy_xts_date[,input$metrics],main='Different Metrics change by date') %>%
       dyAxis("x", drawGrid = T)%>%
       dyAxis('y',drawGrid = T) %>% 
       #dySeries("UV.PV", label = "Highest Price") %>%
      # dySeries("Low", label = "Lowest Price")%>%
      # dySeries("Adjusted", label = "Adjusted Price")%>%
       dyRangeSelector(height=20) #%>%
       #dyOptions(colors =brewer.pal(3, "Dark2"))
  })
  
  
  df_tunnel <- reactive({
    df_tot %>% 
      group_by(event_type) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count))
  })
  
  output$tunnel1 <- renderPlotly({
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(type = "funnel",
                y = df_tunnel()$event_type[c(1,2,4)],
                x = df_tunnel()$count[c(1,2,4)],
                textposition = "inside",
                textinfo = "value+percent previous+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "lightsalmon", "silver"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
     fig <- fig %>%
      layout(yaxis = list(categoryarray = c("view", "cart","purchase")))
    
    
    
  })
  
  output$tunnel2 <- renderPlotly({
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(type = "funnel",
                y = df_tunnel()$event_type[c(1,2,3)],
                x = df_tunnel()$count[c(1,2,3)],
                textposition = "inside",
                textinfo = "value+percent previous+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "lightsalmon",  "silver"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
    fig <- fig %>%
      layout(yaxis = list(categoryarray = c("view", "cart","remove_from_cart")))
    
    
    
  })
  
  df_tunnel_user = reactive({
    df_tot_user %>% 
      filter(!(cart ==0 & remove_from_cart >0),view>0) %>% 
      summarise(view_user = sum(view>0),
                cart_user = sum(cart>0),
                remove_from_cart_user = sum(remove_from_cart>0),
                purchase_user = sum(purchase>0),
                view_purchase_user = sum(cart ==0 & purchase>0),
                view_cart_purchase_user = sum(cart>0 & purchase >0 ),
                view_cart_remove_from_cart_user = sum(cart>0 & remove_from_cart>0 & purchase==0 )) %>% 
      gather("user_type","count")
      
  })
  
  output$tunnel3 <- renderPlotly({
    if (input$overall == "view-cart-purchase"){
      y = df_tunnel_user()$user_type[c(1,2,4)]
      x = df_tunnel_user()$count[c(1,2,4)]
      v = c("view_user","cart_user","purchase_user")
    }
    if (input$overall == "view-cart-remove from cart"){
      y = df_tunnel_user()$user_type[c(1,2,3)]
      x = df_tunnel_user()$count[c(1,2,3)]
      v = c("view_user","cart_user","remove_from_cart_user")
    }
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(type = "funnel",
                y = y,
                x = x,
                textposition = "inside",
                textinfo = "value+percent previous+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "lightsalmon",  "silver"),
                              line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
     fig <- fig %>%
       layout(yaxis = list(categoryarray = v))
  })
  
  output$tunnel4 <- renderPlotly({
    if (input$path == "view-purchase"){
      y = df_tunnel_user()$user_type[c(1,5)]
      x = df_tunnel_user()$count[c(1,5)]
      v = c("view_user","view_purchase_user")
    }
    if (input$path == "view-cart-purchase"){
      y = df_tunnel_user()$user_type[c(1,2,6)]
      x = df_tunnel_user()$count[c(1,2,6)]
      v = c("view_user","cart_user","view_cart_purchase_user")
    }
    if (input$path == "view-cart-remove from cart"){
      y = df_tunnel_user()$user_type[c(1,2,7)]
      x = df_tunnel_user()$count[c(1,2,7)]
      v = c("view_user","cart_user","view_cart_remove_from_cart_user")
    }
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(type = "funnel",
                y = y,
                x = x,
                textposition = "inside",
                textinfo = "value+percent previous+percent initial",
                opacity = 0.65,
                #marker = list(color = c("deepskyblue", "lightsalmon",  "silver"),
                              #line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
    fig <- fig %>%
      layout(yaxis = list(categoryarray = v))
  })
  
  output$active_month <- renderGvis({
   df_tot %>% 
      group_by(month,event_type) %>% 
      summarise(count = n()) %>% 
      spread(event_type,count) %>% 
      gvisComboChart(options=list(title = 'User Behavior Pattern by month',width=2000, height=1000,series = "{3: {type: 'bars'}}",lineWidth = 5))
    
    
  })
  
  output$active_day <- renderGvis({
    df_tot %>% 
      filter(month == input$month) %>% 
      group_by(Date,event_type) %>% 
      summarise(count = n()) %>% 
      spread(event_type,count) %>% 
      gvisComboChart(options=list(title = 'User Behavior Pattern by day',width=2000, height=1000,series = "{3: {type: 'bars'}}",lineWidth = 5))
    
    
  })
  
  output$active_week <- renderGvis({
    df_tot %>% 
      filter(Date %in% seq(as.Date(input$week), length = 7, by = "days")) %>% 
      group_by(wday,event_type) %>% 
      summarise(count = n()) %>% 
      spread(event_type,count) %>% 
      gvisComboChart(options=list(title = 'User Behavior Pattern by week',width=2000, height=1000,series = "{3: {type: 'bars'}}",lineWidth = 5))
    
    
  })
  
  output$active_hour <- renderGvis({
    df_tot %>% 
      filter(Date == input$date) %>% 
      group_by(hour,event_type) %>% 
      summarise(count = n()) %>% 
      spread(event_type,count) %>% 
      gvisComboChart(options=list(title = 'User Behavior Pattern by hour',width=2000, height=1000,series = "{3: {type: 'bars'}}",lineWidth = 5))
    
    
  })
  
  output$category <- renderPlotly({
    df_tot %>% 
      group_by(category_id) %>% 
      summarise(total = sum(event_type == input$type)) %>% 
      arrange(by = desc(total)) %>% 
      top_n(20,total) %>% 
      ggplot(aes(x=reorder(category_id,total),y = total)) + geom_col() + 
      coord_flip() + 
      theme_economist() + 
      scale_fill_economist() +
      ylab(paste0("Total ",input$type)) + xlab("Category_id") +
      ggtitle(paste0("Top 20 Most ",input$type ," Category"))
    
    
  })
  
  output$product <- renderPlotly({
    df_tot %>% 
      group_by(product_id) %>% 
      summarise(total = sum(event_type == input$type)) %>% 
      arrange(by = desc(total)) %>% 
      top_n(20,total) %>% 
      ggplot(aes(x=reorder(product_id,total),y = total)) + geom_col() + 
      coord_flip() + 
      theme_economist() + 
      scale_fill_economist() +
      ylab(paste0("Total ",input$type)) + xlab("Product_id") +
      ggtitle(paste0("Top 20 Most ",input$type ," Product"))
    
    
  })
  
  
  
  
  
  
})