library(shiny)
library(zoo)

# ==============================================================================
# FinLab Beta Calculator (Custom Date Range Version)
# 架構: 前端 JS (讀取日期 -> Proxy 抓 JSON) -> R (運算)
# ==============================================================================

ui <- fluidPage(
  titlePanel("財金資訊平台：CAPM Beta 係數分析"),
  
  # --- JavaScript 核心邏輯 (支援自訂日期) ---
  tags$head(tags$script(HTML("
    // 透過 JS 抓取 Yahoo Finance Chart API (支援自訂日期區間)
    async function fetchYahooData() {
      
      // 1. 讀取介面上的輸入值
      const stockSym = document.getElementById('stock_sym').value;
      const benchSym = document.getElementById('benchmark_sym').value;
      
      // 讀取 Shiny 的日期選擇器 (因為它是兩個 input，我們用 jQuery 抓取)
      // Shiny 的 dateRangeInput 結構是兩個 input[type='text']
      const startStr = $('#dates input:eq(0)').val(); // 格式: YYYY-MM-DD
      const endStr = $('#dates input:eq(1)').val();   // 格式: YYYY-MM-DD

      if (!stockSym || !benchSym || !startStr || !endStr) {
        alert('請確認股票代碼與日期皆已輸入');
        return;
      }

      // 2. 將日期轉換為 Unix Timestamp (Yahoo API 需要秒數)
      const p1 = Math.floor(new Date(startStr).getTime() / 1000);
      // 結束日期加 1 天 (86400秒)，確保包含當天收盤數據
      const p2 = Math.floor(new Date(endStr).getTime() / 1000) + 86400;

      // UI 狀態控制
      $('#status_msg').text(`正在下載 ${startStr} 至 ${endStr} 的數據...`);
      $('#run_btn').prop('disabled', true);
      $('#run_btn').html('<i class=\"fa fa-spinner fa-spin\"></i> 處理中...');

      // 定義抓取函數
      async function getStockData(symbol) {
        const baseUrl = 'https://query1.finance.yahoo.com/v8/finance/chart/';
        
        // 【關鍵修改】使用 period1 與 period2 參數
        const targetUrl = `${baseUrl}${symbol}?period1=${p1}&period2=${p2}&interval=1d&events=history`;
        const proxyUrl = 'https://corsproxy.io/?' + encodeURIComponent(targetUrl);
        
        const response = await fetch(proxyUrl);
        if (!response.ok) throw new Error(`無法下載 ${symbol} (HTTP ${response.status})`);
        
        const data = await response.json();
        if (!data.chart || !data.chart.result || data.chart.result.length === 0) {
          throw new Error(`Yahoo 回傳 ${symbol} 無數據`);
        }
        
        const result = data.chart.result[0];
        const timestamps = result.timestamp;
        
        // 處理可能缺漏的 indicators
        if (!result.indicators.quote || !result.indicators.quote[0]) throw new Error('數據格式錯誤');
        
        const adjClose = result.indicators.adjclose ? result.indicators.adjclose[0].adjclose : null;
        const close = result.indicators.quote[0].close;
        
        // 優先用 Adj Close，若無則用 Close
        const prices = adjClose || close;
        
        // 轉 CSV 字串
        let csvContent = 'Date,Price\\n';
        for (let i = 0; i < timestamps.length; i++) {
          if (timestamps[i] && prices[i]) {
            let date = new Date(timestamps[i] * 1000);
            let dateStr = date.toISOString().split('T')[0];
            csvContent += `${dateStr},${prices[i]}\\n`;
          }
        }
        return csvContent;
      }

      try {
        // 平行下載
        const [stockCSV, benchCSV] = await Promise.all([
          getStockData(stockSym),
          getStockData(benchSym)
        ]);
        
        $('#status_msg').text('下載成功！正在進行 R 迴歸運算...');
        
        // 傳送給 R
        Shiny.setInputValue('js_data', {
          stock: stockCSV,
          bench: benchCSV,
          timestamp: new Date().getTime() // 強制刷新
        });

      } catch (error) {
        console.error(error);
        $('#status_msg').text('錯誤: ' + error.message);
        alert('抓取失敗：' + error.message);
      } finally {
        $('#run_btn').prop('disabled', false);
        $('#run_btn').html('<i class=\"fa fa-cloud-download\"></i> 執行分析 (Run)');
      }
    }
  "))),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("參數設定"),
      hr(),
      textInput("stock_sym", "個股代碼 (Symbol)", value = "AAPL", placeholder = "e.g. AAPL, 2330.TW"),
      textInput("benchmark_sym", "市場基準 (Benchmark)", value = "^GSPC", placeholder = "e.g. ^GSPC, ^TWII"),
      
      # 日期選擇器 (id = dates)
      dateRangeInput("dates", "資料期間",
                     start = Sys.Date() - 365,
                     end = Sys.Date()),
      
      helpText("點擊按鈕後，將依照您選擇的日期區間抓取資料。"),
      
      # 按鈕 (呼叫 fetchYahooData，不需傳參，直接在函數內讀 DOM)
      tags$button(
        id = "run_btn",
        class = "btn btn-primary btn-lg",
        type = "button",
        style = "width: 100%;",
        HTML('<i class="fa fa-cloud-download"></i> 執行分析 (Run)'),
        onclick = "fetchYahooData()"
      ),
      br(), br(),
      tags$div(id = "status_msg", style = "color: #e74c3c; font-weight: bold;", "準備就緒")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # --- 分頁 1: 迴歸分析 ---
        tabPanel("迴歸分析 (Regression)", 
                 br(),
                 wellPanel(
                   h2(textOutput("beta_value"), style = "color: #2c3e50; font-weight: bold; margin: 0;"),
                   p("Model: CAPM Market Model (OLS)")
                 ),
                 plotOutput("regPlot", height = "450px"),
                 hr(),
                 h4("模型統計摘要 (Model Summary)"),
                 verbatimTextOutput("model_summary")
        ),
        
        # --- 分頁 2: 股價走勢 ---
        tabPanel("股價走勢 (Price History)",
                 br(),
                 plotOutput("pricePlot", height = "500px")
        ),
        
        # --- 分頁 3: 數據表格 ---
        tabPanel("收益率數據 (Data)", 
                 br(),
                 dataTableOutput("tbl")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 1. 接收資料
  market_data <- reactive({
    req(input$js_data)
    
    tryCatch({
      df_stock <- read.csv(text = input$js_data$stock, stringsAsFactors = FALSE)
      df_bench <- read.csv(text = input$js_data$bench, stringsAsFactors = FALSE)
      
      z_stock <- zoo(df_stock$Price, order.by = as.Date(df_stock$Date))
      z_bench <- zoo(df_bench$Price, order.by = as.Date(df_bench$Date))
      
      merged <- merge(z_stock, z_bench, all = FALSE)
      names(merged) <- c("Stock", "Benchmark")
      
      # 若資料過少 (例如選到假日或未來)，回傳 NULL
      if(nrow(merged) < 2) stop("有效交易日數據不足 (可能選到了假日或區間太短)")
      
      return(merged)
    }, error = function(e) {
      showNotification(paste("資料處理錯誤:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # 2. 計算收益率
  returns_data <- reactive({
    req(market_data())
    prices <- market_data()
    ret <- diff(prices) / lag(prices, -1)
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
    desc <- if(beta > 1.2) "(高波動 High Volatility)" 
    else if(beta > 0.8) "(貼近大盤 Market Beta)" 
    else if(beta > 0) "(低波動 Low Volatility)" 
    else "(反向 Inverse)"
    paste0("Beta (β): ", format(round(beta, 4), nsmall = 4), " ", desc)
  })
  
  output$model_summary <- renderPrint({
    req(fit_model())
    summary(fit_model())
  })
  
  output$regPlot <- renderPlot({
    req(returns_data(), fit_model())
    dat <- as.data.frame(returns_data())
    
    plot(dat$Benchmark, dat$Stock,
         main = paste("Regression:", input$stock_sym, "vs", input$benchmark_sym),
         xlab = "Market Returns", ylab = "Stock Returns",
         pch = 19, col = rgb(0.2, 0.4, 0.8, 0.5), las = 1, cex = 1.2)
    abline(fit_model(), col = "#e74c3c", lwd = 3)
    grid(col = "gray", lty = "dotted")
    legend("topleft", legend = paste("Beta =", round(coef(fit_model())[2], 2)),
           text.col = "#e74c3c", bty = "n", cex = 1.2)
  })
  
  output$pricePlot <- renderPlot({
    req(market_data())
    prices <- market_data()
    prices_norm <- prices / rep(coredata(prices)[1,], each = nrow(prices)) * 100
    
    par(bg = "white")
    plot.zoo(prices_norm, plot.type = "single", 
             col = c("#2980b9", "#f39c12"), lwd = 2.5,
             xlab = "", ylab = "Normalized Price (Base=100)",
             main = "Price Performance Comparison",
             las = 1, xaxt = "n")
    
    axis.Date(1, at = index(prices_norm), format = "%Y-%m")
    axis(2, las = 1)
    legend("topleft", legend = c(input$stock_sym, input$benchmark_sym), 
           col = c("#2980b9", "#f39c12"), lty = 1, lwd = 2.5, bty = "n", cex = 1.2)
  })
  
  output$tbl <- renderDataTable({
    req(returns_data())
    df <- as.data.frame(returns_data())
    df_disp <- round(df * 100, 2)
    colnames(df_disp) <- paste0(colnames(df_disp), " (%)")
    df_disp <- cbind(Date = format(index(returns_data()), "%Y-%m-%d"), df_disp)
    df_disp[order(df_disp$Date, decreasing = TRUE), ]
  }, options = list(pageLength = 10, searching = FALSE))
}

shinyApp(ui, server)