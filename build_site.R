# AIFinLab Shinylive 自動化編譯腳本
if (!requireNamespace("shinylive", quietly = TRUE)) install.packages("shinylive")

library(shinylive)

# 定義要編譯的模組清單 (對應 source_xxx -> xxx)
# modules <- c("beta", "opt")
modules <- c("beta")

cat("--- AIFinLab 系統編譯啟動 ---\n")

for (mod in modules) {
  src_dir <- paste0("source_", mod)
  dest_dir <- mod
  
  if (dir.exists(src_dir)) {
    cat(sprintf("正在編譯模組 [%s]: %s -> %s\n", mod, src_dir, dest_dir))
    
    # 執行 Shinylive 輸出
    # 建議加上 subdir = FALSE 確保檔案直接放在目錄下
    shinylive::export(appdir = src_dir, destdir = dest_dir)
    
  } else {
    cat(sprintf("警告: 找不到原始碼目錄 %s，跳過此模組。\n", src_dir))
  }
}

cat("--- AIFinLab 編譯完成 ---\n")

# httpuv::runStaticServer("beta",port = 8001)
# httpuv::runStaticServer("opt",port = 8002)
