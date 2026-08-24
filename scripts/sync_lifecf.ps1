[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$repoRoot = Split-Path -Parent $PSScriptRoot
$workspaceRoot = Split-Path -Parent $repoRoot
$source = Join-Path $workspaceRoot 'LifeCF\App\index.html'
$targetDirectory = Join-Path $repoRoot 'lifecf'
$target = Join-Path $targetDirectory 'index.html'

if (-not (Test-Path -LiteralPath $source -PathType Leaf)) {
    throw "LifeCF source file was not found: $source"
}

if (-not (Test-Path -LiteralPath $targetDirectory -PathType Container)) {
    New-Item -ItemType Directory -Path $targetDirectory | Out-Null
}

$needsCopy = -not (Test-Path -LiteralPath $target -PathType Leaf)
if (-not $needsCopy) {
    $sourceHash = (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash
    $targetHash = (Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash
    $needsCopy = $sourceHash -ne $targetHash
}

if ($needsCopy) {
    Copy-Item -LiteralPath $source -Destination $target -Force
    Write-Output "Synchronized: $source -> $target"
} else {
    Write-Output 'LifeCF deployment file is already up to date.'
}
