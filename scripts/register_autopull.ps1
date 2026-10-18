# 在這台電腦登記「AIFinLab-AutoPull」排程：每天 09:00 與每次登入時執行 autopull.ps1。
# L: 是隨身碟、會帶到不同電腦，每台電腦執行一次即可：
#   powershell -NoProfile -ExecutionPolicy Bypass -File L:\hcLee\AIFT\AIFinLab\scripts\register_autopull.ps1
# 移除排程：Unregister-ScheduledTask -TaskName AIFinLab-AutoPull -Confirm:$false
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
$script = Join-Path $PSScriptRoot 'autopull.ps1'

# 隨身碟在不同電腦上的檔案擁有者不同，git 需要把此資料夾列為信任目錄
$safe = ($repo -replace '\\', '/')
$existing = @(git config --global --get-all safe.directory 2>$null)
if ($existing -notcontains $safe) { git config --global --add safe.directory $safe }

$action = New-ScheduledTaskAction -Execute 'powershell.exe' `
    -Argument "-NoProfile -WindowStyle Hidden -ExecutionPolicy Bypass -File `"$script`""
$triggers = @(
    (New-ScheduledTaskTrigger -Daily -At '09:00'),
    (New-ScheduledTaskTrigger -AtLogOn -User "$env:USERDOMAIN\$env:USERNAME")
)
$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -AllowStartIfOnBatteries `
    -DontStopIfGoingOnBatteries -ExecutionTimeLimit (New-TimeSpan -Minutes 10)
Register-ScheduledTask -TaskName 'AIFinLab-AutoPull' -Action $action -Trigger $triggers -Settings $settings `
    -Description 'Pull GitHub updates (house price dashboard, morning report) into AIFinLab' -Force | Out-Null
Write-Output "已登記排程 AIFinLab-AutoPull（每天 09:00 與登入時），執行 $script"
