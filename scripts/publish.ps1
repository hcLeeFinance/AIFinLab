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

Invoke-Git -GitArguments @('add', '-A')

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
    Invoke-Git -GitArguments @('push')
}
