<#
  Installationstool mit Profilen je Modul. Ohne Parameter werden alle Module installiert.
  Module: validator, config23x, config30x, zugferd, vis23x, vis30x, jre, fop, mustang, saxon

  Beispiele:
  - Alle Module installieren (Default ohne Parameter):
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\installtools.ps1

  - Nur Validator und 3.0.x-Konfiguration:
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\installtools.ps1 -Modules validator, config30x

  - Nur JRE installieren:
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\installtools.ps1 -Modules jre

  - Nur Visualization 2.3.x installieren:
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\installtools.ps1 -Modules vis23x
#>

param(
  [Parameter(Position=0, HelpMessage='Module: validator, config23x, config30x, zugferd, vis23x, vis30x, jre, fop, mustang, saxon')]
  [string[]]$Modules
)

$Root = $PSScriptRoot
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
$ProgressPreference = 'SilentlyContinue'

Add-Type -AssemblyName System.Net.Http

function Remove-Dir($path) {
  if ([string]::IsNullOrWhiteSpace($path)) { return }
  if (Test-Path $path) { Remove-Item $path -Recurse -Force -ErrorAction SilentlyContinue }
}
function Remove-File($path) {
  if ([string]::IsNullOrWhiteSpace($path)) { return }
  if (Test-Path $path) { Remove-Item $path -Force -ErrorAction SilentlyContinue }
}

function Invoke-Download {
  param(
    [Parameter(Mandatory=$true)][string]$Uri,
    [Parameter(Mandatory=$true)][string]$Destination,
    [string]$Label
  )

  $activity = if ([string]::IsNullOrWhiteSpace($Label)) { "Downloading $([IO.Path]::GetFileName($Destination))" } else { $Label }
  $previousPreference = $ProgressPreference
  $client = [System.Net.Http.HttpClient]::new()
  $buffer = New-Object byte[] (4MB)
  $response = $null
  $stream = $null
  $fileStream = $null
  try {
    $ProgressPreference = 'Continue'
    $response = $client.GetAsync($Uri, [System.Net.Http.HttpCompletionOption]::ResponseHeadersRead).Result
    if (-not $response.IsSuccessStatusCode) {
      throw "Download failed ($Uri): $($response.StatusCode) $($response.ReasonPhrase)"
    }

    $content = $response.Content
    $stream = $content.ReadAsStreamAsync().Result
    $targetDir = Split-Path -Path $Destination -Parent
    if ($targetDir -and -not (Test-Path $targetDir)) {
      New-Item -ItemType Directory -Path $targetDir -Force | Out-Null
    }

    $fileStream = [System.IO.File]::Open($Destination, [System.IO.FileMode]::Create, [System.IO.FileAccess]::Write, [System.IO.FileShare]::None)
    $totalLength = $content.Headers.ContentLength
    $bytesReadTotal = 0L

    while (($bytesRead = $stream.Read($buffer, 0, $buffer.Length)) -gt 0) {
      $fileStream.Write($buffer, 0, $bytesRead)
      $bytesReadTotal += $bytesRead
      if ($totalLength -and $totalLength -gt 0) {
        $percent = [math]::Round(($bytesReadTotal / $totalLength) * 100, 2)
        $status = "{0:N2} MB / {1:N2} MB" -f ($bytesReadTotal / 1MB), ($totalLength / 1MB)
        Write-Progress -Activity $activity -Status $status -PercentComplete $percent
      }
      else {
        $status = "{0:N2} MB heruntergeladen" -f ($bytesReadTotal / 1MB)
        Write-Progress -Activity $activity -Status $status
      }
    }

    Write-Progress -Activity $activity -Completed
  }
  catch {
    Remove-File $Destination
    throw
  }
  finally {
    if ($fileStream) { $fileStream.Dispose() }
    if ($stream) { $stream.Dispose() }
    if ($response) { $response.Dispose() }
    if ($client) { $client.Dispose() }
    $ProgressPreference = $previousPreference
  }
}

function Install-Validator {
  Write-Host 'Installing: validator'
  Remove-Dir (Join-Path $Root 'validator')
  $targetDir = Join-Path $Root 'validator'
  New-Item -Path $targetDir -ItemType Directory -Force | Out-Null
  $destinationJar = Join-Path $targetDir 'validator-1.6.1-standalone.jar'
  Invoke-Download -Uri "https://github.com/itplr-kosit/validator/releases/download/v1.6.1/validator-1.6.1-standalone.jar" -Destination $destinationJar -Label 'validator download'
}

function Install-Config23x {
  Write-Host 'Installing: validator-configuration23x'
  Remove-Dir (Join-Path $Root 'validator-configuration23x')
  $zip = Join-Path $Root 'validator-configuration23x.zip'
  Invoke-Download -Uri "https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2023-05-12/validator-configuration-xrechnung_2.3.1_2023-05-12.zip" -Destination $zip -Label 'config23x download'
  Expand-Archive $zip -DestinationPath (Join-Path $Root 'validator-configuration23x') -Force
  Remove-File $zip
}

function Install-Config30x {
  Write-Host 'Installing: validator-configuration30x'
  Remove-Dir (Join-Path $Root 'validator-configuration30x')
  $zip = Join-Path $Root 'validator-configuration30x.zip'
  Invoke-Download -Uri "https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/v2026-01-31/xrechnung-3.0.2-validator-configuration-2026-01-31.zip" -Destination $zip -Label 'config30x download'
  Expand-Archive $zip -DestinationPath (Join-Path $Root 'validator-configuration30x') -Force
  Remove-File $zip
}

function Install-Zugferd {
  Write-Host 'Installing: validator-configuration-zugferd'
  Remove-Dir (Join-Path $Root 'validator-configuration-zugferd')
  $zip = Join-Path $Root 'validator-configuration-zugferd.zip'
  Invoke-Download -Uri "https://github.com/LandrixSoftware/validator-configuration-zugferd/releases/download/validation-configuration-zugferd-2.3.3-20251208/validation-configuration-zugferd-2.3.3-2025-12-08.zip" -Destination $zip -Label 'zugferd download'
  Expand-Archive $zip -DestinationPath (Join-Path $Root 'validator-configuration-zugferd') -Force
  Remove-File $zip
}

function Install-Vis23x {
  Write-Host 'Installing: visualization23x'
  Remove-Dir (Join-Path $Root 'visualization23x')
  $zip = Join-Path $Root 'visualization23x.zip'
  Invoke-Download -Uri "https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2023-05-12/xrechnung-2.3.1-xrechnung-visualization-2023-05-12.zip" -Destination $zip -Label 'visualization23x download'
  Expand-Archive $zip -DestinationPath (Join-Path $Root 'visualization23x') -Force
  Remove-File $zip
}

function Install-Vis30x {
  Write-Host 'Installing: visualization30x'
  Remove-Dir (Join-Path $Root 'visualization30x')
  $zip = Join-Path $Root 'visualization30x.zip'
  Invoke-Download -Uri "https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2026-01-31/xrechnung-3.0.2-visualization-2026-01-31.zip" -Destination $zip -Label 'visualization30x download'
  Expand-Archive $zip -DestinationPath (Join-Path $Root 'visualization30x') -Force
  Remove-File $zip
}

function Install-Jre {
  Write-Host 'Installing: jre -> java'
  Remove-Dir (Join-Path $Root 'java')
  Remove-Dir (Join-Path $Root 'jdk-17.0.6+10-jre')
  $zip = Join-Path $Root 'jre.zip'
  Invoke-Download -Uri "https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.6%2B10/OpenJDK17U-jre_x64_windows_hotspot_17.0.6_10.zip" -Destination $zip -Label 'jre download'
  Expand-Archive $zip -DestinationPath $Root -Force
  $jreFolder = Join-Path $Root 'jdk-17.0.6+10-jre'
  if (-not (Test-Path $jreFolder)) {
    $candidate = Get-ChildItem -Path $Root -Directory -Filter 'jdk*-jre' | Select-Object -First 1
    if ($candidate) { $jreFolder = $candidate.FullName }
  }
  if (Test-Path $jreFolder) { Move-Item $jreFolder (Join-Path $Root 'java') -Force }
  Remove-File $zip
}

function Install-Fop {
  Write-Host 'Installing: apache-fop'
  Remove-Dir (Join-Path $Root 'apache-fop')
  Remove-Dir (Join-Path $Root 'fop-2.8')
  $zip = Join-Path $Root 'fop.zip'
  Invoke-Download -Uri "https://archive.apache.org/dist/xmlgraphics/fop/binaries/fop-2.8-bin.zip" -Destination $zip -Label 'apache fop download'
  Expand-Archive $zip -DestinationPath $Root -Force
  $fopFolder = Join-Path $Root 'fop-2.8'
  if (-not (Test-Path $fopFolder)) {
    $candidate = Get-ChildItem -Path $Root -Directory -Filter 'fop-*' | Sort-Object Name -Descending | Select-Object -First 1
    if ($candidate) { $fopFolder = $candidate.FullName }
  }
  if (Test-Path $fopFolder) { Move-Item $fopFolder (Join-Path $Root 'apache-fop') -Force }
  Remove-File $zip
}

function Install-Saxon {
  Write-Host 'Installing: saxon'
  Remove-Dir (Join-Path $Root 'saxon')
  $zip = Join-Path $Root 'saxon.zip'
  Invoke-Download -Uri "https://github.com/Saxonica/Saxon-HE/releases/download/SaxonHE12-9/SaxonHE12-9J.zip" -Destination $zip -Label 'saxon download'
  New-Item -Path (Join-Path $Root 'saxon') -ItemType Directory -Force | Out-Null
  Expand-Archive $zip -DestinationPath (Join-Path $Root 'saxon') -Force
  Remove-File $zip
}

function Install-Mustang {
  Write-Host 'Installing: mustangproject (Mustang-CLI)'
  Remove-Dir (Join-Path $Root 'mustangproject')
  New-Item -Path (Join-Path $Root 'mustangproject') -ItemType Directory -Force | Out-Null
  $headers = @{ 'User-Agent' = 'PowerShell' }
  $releaseJson = (Invoke-WebRequest -Headers $headers -Uri 'https://api.github.com/repos/ZUGFeRD/mustangproject/releases/latest').Content | ConvertFrom-Json
  $asset = $releaseJson.assets | Where-Object { $_.name -match '^Mustang-CLI.*\.jar$' } | Select-Object -First 1
  if ($null -ne $asset) {
    Invoke-Download -Uri $asset.browser_download_url -Destination (Join-Path $Root 'mustangproject\Mustang-CLI.jar') -Label 'Mustang-CLI download'
    Set-Content (Join-Path $Root 'mustangproject\Mustang-CLI-version.md') -Value $asset.name
  } else {
    Set-Content (Join-Path $Root 'mustangproject\Mustang-CLI-version.md') -Value ($releaseJson.name)
  }
}

$all = @('validator','config23x','config30x','zugferd','vis23x','vis30x','jre','fop','saxon','mustang')
if (-not $Modules -or $Modules.Count -eq 0) { $Modules = $all }
$Modules = $Modules | ForEach-Object { $_.ToLower() }

# Validierung der Modulnamen
$invalid = $Modules | Where-Object { $_ -notin $all }
if ($invalid) {
  Write-Error ("Unbekannte Module: {0}. Erlaubt: {1}" -f ($invalid -join ', '), ($all -join ', '))
  exit 1
}

foreach ($m in $Modules) {
  switch ($m) {
    'validator'   { Install-Validator }
    'config23x'   { Install-Config23x }
    'config30x'   { Install-Config30x }
    'zugferd'     { Install-Zugferd }
    'vis23x'      { Install-Vis23x }
    'vis30x'      { Install-Vis30x }
    'jre'         { Install-Jre }
    'fop'         { Install-Fop }
    'saxon'       { Install-Saxon }
    'mustang'     { Install-Mustang }
  }
}
