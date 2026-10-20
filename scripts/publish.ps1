[CmdletBinding()]
param(
    [string]$Message,
    [switch]$NoPush
)

$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path -Parent $PSScriptRoot

function Invoke-Git {
    param([Parameter(Mandatory = $true)][string[]]$GitArguments)

    & git -C $repoRoot @GitArguments
    if ($LASTEXITCODE -ne 0) {
        throw "git $($GitArguments -join ' ') failed with exit code $LASTEXITCODE"
    }
}

& (Join-Path $PSScriptRoot 'sync_lifecf.ps1')

# The house-price dashboard outputs are written only by the GitHub Action; never commit
# local preview rebuilds of them (they would conflict with the bot's binary xlsx commits).
$botOutputs = @('houseprice/index.html', 'houseprice/segments.js', 'houseprice/last_season.txt', 'houseprice/*.xlsx')
$dirtyOutputs = & git -C $repoRoot status --porcelain -- @botOutputs
if ($LASTEXITCODE -ne 0) {
    throw "git status failed with exit code $LASTEXITCODE"
}
if ($dirtyOutputs) {
    Write-Warning ("Skipping local changes to bot-owned house-price outputs (not committed):`n" +
        ($dirtyOutputs -join "`n") +
        "`nDiscard them with: git checkout -- houseprice/index.html houseprice/last_season.txt houseprice/*.xlsx" +
        "`n(houseprice/segments.js: git checkout -- it if tracked, delete it if untracked)")
}

Invoke-Git -GitArguments (@('add', '-A', '--', '.') + ($botOutputs | ForEach-Object { ":(exclude)$_" }))

& git -C $repoRoot diff --cached --quiet
$hasStagedChanges = $LASTEXITCODE -eq 1
if ($LASTEXITCODE -notin @(0, 1)) {
    throw "git diff --cached --quiet failed with exit code $LASTEXITCODE"
}

if ($hasStagedChanges) {
    if ([string]::IsNullOrWhiteSpace($Message)) {
        $Message = 'Update AIFinLab ' + (Get-Date -Format 'yyyy-MM-dd HH:mm')
    }
    Invoke-Git -GitArguments @('commit', '-m', $Message)
} else {
    Write-Output 'No new changes to commit.'
}

if ($NoPush) {
    Write-Output 'NoPush verification completed; GitHub was not contacted.'
} else {
    Invoke-Git -GitArguments @('pull', '--no-edit', 'origin', 'main')
    Invoke-Git -GitArguments @('push')
}
