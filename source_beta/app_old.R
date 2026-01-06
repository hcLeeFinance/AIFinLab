library(shiny)
library(quantmod)
library(zoo)

# --- UI (前端介面) ---
ui <- fluidPage(
  titlePanel("財金資訊平台：CAPM Beta 係數分析"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("參數設定 (Parameters)"),
      hr(),
      textInput("stock_sym", "個股代碼 (Symbol)", value = "AAPL"),
      textInput("benchmark_sym", "市場基準 (Benchmark)", value = "^GSPC"),
      
      dateRangeInput("dates", "分析期間",
                     start = Sys.Date() - 365,
                     end = Sys.Date()),
      
      selectInput("ret_type", "收益率計算",
                  choices = c("Log Return (連續複利)" = "log", 
                              "Arithmetic (簡單收益率)" = "arithmetic")),
      
      hr(),
      actionButton("run_analysis", "執行分析 (Run)", 
                   class = "btn-primary btn-lg", icon = icon("chart-line"))
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # --- 修改重點：分頁 1 佈局調整 ---
        tabPanel("迴歸分析 (Regression)", 
                 br(),
                 # 1. 標題與 Beta 值
                 wellPanel(
                   h2(textOutput("beta_value"), style = "color: #2c3e50; font-weight: bold; margin-top: 0px;"),
                   p("Model: Market Model Regression (Ordinary Least Squares)")
                 ),
                 
                 # 2. 圖表放上面 (加大高度)
                 plotOutput("regPlot", height = "500px"),
                 
                 br(), 
                 hr(), # 分隔線
                 
                 # 3. 統計摘要放下面 (這就是您要的 Call/Residuals/Coefficients)
                 h4("模型統計摘要 (Model Summary)"),
                 verbatimTextOutput("model_summary") 
        ),
        
        # 分頁 2: 股價走勢
        tabPanel("股價走勢 (Price History)",
                 br(),
                 plotOutput("pricePlot", height = "500px")
        ),
        
        # 分頁 3: 數據檢視
        tabPanel("收益率數據 (Data Table)", 
                 br(),
                 downloadButton("downloadData", "下載 CSV"),
                 br(), br(),
                 dataTableOutput("tbl")
        )
      )
    )
  )
)

# --- Server (後端邏輯) ---
server <- function(input, output) {
  
  # 1. 資料抓取
  market_data <- eventReactive(input$run_analysis, {
    withProgress(message = '連線 Yahoo Finance...', value = 0, {
      tryCatch({
        incProgress(0.2)
        stock_xts <- getSymbols(input$stock_sym, src = "yahoo", 
                                from = input$dates[1], to = input$dates[2], 
                                auto.assign = FALSE)
        incProgress(0.5)
        bench_xts <- getSymbols(input$benchmark_sym, src = "yahoo", 
                                from = input$dates[1], to = input$dates[2], 
                                auto.assign = FALSE)
        
        merged_price <- merge(Ad(stock_xts), Ad(bench_xts), join = "inner")
        names(merged_price) <- c("Stock", "Benchmark")
        return(na.omit(merged_price))
        
      }, error = function(e) {
        showNotification(paste("下載失敗:", e$message), type = "error")
        return(NULL)
      })
    })
  })
  
  # 2. 計算收益率
  returns_data <- reactive({
    req(market_data())
    prices <- market_data()
    if(input$ret_type == "log") {
      ret <- diff(log(prices)) 
    } else {
      ret <- ROC(prices, type = "discrete")
    }
    return(na.omit(ret))
  })
  
  # 3. 建立模型
  fit_model <- reactive({
    req(returns_data())
    dat <- returns_data()
    lm(Stock ~ Benchmark, data = as.data.frame(dat))
  })
  
  # --- Outputs ---
  
  output$beta_value <- renderText({
    req(fit_model())
    beta <- coef(fit_model())[2]
    desc <- if(beta > 1.2) "(高波動)" else if(beta > 0.8) "(貼近大盤)" else "(低波動)"
    paste0("Beta (β): ", format(round(beta, 4), nsmall = 4), " ", desc)
  })
  
  # 這裡的輸出會包含 Call, Residuals, Coefficients 等完整訊息
  output$model_summary <- renderPrint({
    req(fit_model())
    summary(fit_model())
  })
  
  output$regPlot <- renderPlot({
    req(returns_data(), fit_model())
    dat <- as.data.frame(returns_data())
    
    # 迴歸散佈圖
    plot(dat$Benchmark, dat$Stock,
         main = paste("Regression Analysis:", input$stock_sym),
         xlab = paste("Market Returns:", input$benchmark_sym), 
         ylab = paste("Stock Returns:", input$stock_sym),
         pch = 19, col = rgb(0.2, 0.4, 0.8, 0.5),
         las = 1)
    abline(fit_model(), col = "#e74c3c", lwd = 3)
    grid(col = "gray", lty = "dotted")
    legend("topleft", legend = paste("Beta =", round(coef(fit_model())[2], 2)),
           text.col = "#e74c3c", bty = "n", cex = 1.2)
  })
  
  output$pricePlot <- renderPlot({
    req(market_data())
    prices <- market_data()
    prices_norm <- prices / rep(coredata(prices)[1,], each = nrow(prices)) * 100
    
    # 乾淨白底圖表
    par(bg = "white")
    plot.zoo(prices_norm, plot.type = "single", 
             col = c("#2980b9", "#f39c12"), lwd = 2.5,
             xlab = "", ylab = "Normalized Price",
             main = "Price Performance", las = 1, xaxt = "n")
    axis.Date(1, at = index(prices_norm), format = "%Y-%m")
    axis(2, las = 1)
    legend("topleft", legend = c(input$stock_sym, input$benchmark_sym), 
           col = c("#2980b9", "#f39c12"), lty = 1, lwd = 2.5, bty = "n")
  })
  
  output$tbl <- renderDataTable({
    req(returns_data())
    df <- as.data.frame(returns_data())
    df_display <- round(df * 100, 2)
    colnames(df_display) <- paste0(colnames(df_display), " (%)")
    df_display <- cbind(Date = format(index(returns_data()), "%Y-%m-%d"), df_display)
    df_display[order(df_display$Date, decreasing = TRUE), ]
  }, options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE))
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("beta_analysis.csv") },
    content = function(file) { write.csv(as.data.frame(returns_data()), file) }
  )
}

shinyApp(ui, server)