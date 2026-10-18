# 由 Windows 工作排程器呼叫：把 GitHub 上的更新（例如 GitHub Actions 重算的房價儀表板、晨報）
# 拉到本機 AIFinLab。只做 fast-forward；本機有未提交修改或分歧時會安全地停止，不會合併或覆蓋。
# 結果記錄在 %LOCALAPPDATA%\AIFinLab-autopull.log
$repo = Split-Path -Parent $PSScriptRoot
$log = Join-Path $env:LOCALAPPDATA 'AIFinLab-autopull.log'

$out = & git -C $repo pull --ff-only origin main 2>&1
$line = '{0}  exit={1}  {2}' -f (Get-Date -Format 'yyyy-MM-dd HH:mm:ss'), $LASTEXITCODE, (($out | ForEach-Object { "$_" }) -join ' | ')
Add-Content -Path $log -Value $line -Encoding utf8

# 記錄檔只保留最近 200 行
$lines = Get-Content -Path $log -Encoding utf8
if ($lines.Count -gt 200) { $lines | Select-Object -Last 200 | Set-Content -Path $log -Encoding utf8 }
