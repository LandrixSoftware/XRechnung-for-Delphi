<#
  Baut + laeuft den Test der pascalnativen PDF-Anhangsextraktion
  (intf.XRechnungPdfExtract) - wahlweise mit FreePascal oder Delphi.

  Der Test laeuft ueber einen Verzeichnisbaum voller PDFs (Standard: das
  Nachbar-Repo ZUGFeRD-for-Delphi) und vergleicht die Extraktion ueber den
  PDF-Objektgraphen mit einem naiven Rohdaten-Scanner als Kontrollgruppe.
  Zusaetzlich laeuft immer ein Selbsttest, der ohne Fremddateien auskommt:
  er baut ein PDF mit inkrementellem Update, das den Rechnungsanhang ersetzt,
  und prueft, dass die GUELTIGE Fassung geliefert wird und nicht die
  ueberschriebene Vorversion.

  Beispiele:
    .\run-pdfextract-test-win.ps1
    .\run-pdfextract-test-win.ps1 -Compiler Delphi
    .\run-pdfextract-test-win.ps1 -PdfRoot "d:\Pfad\zu\PDFs"
    .\run-pdfextract-test-win.ps1 -ShowAll

  ExitCode wird durchgereicht (0 = alles bestanden).
#>
[CmdletBinding()]
param(
  [ValidateSet('FPC','Delphi')]
  [string]$Compiler = 'FPC',
  # Pfad zu fpc.exe (sonst $env:FPC bzw. bekannte fpcupdeluxe/Lazarus-Pfade).
  [string]$Fpc = $env:FPC,
  # Verzeichnisbaum mit den zu pruefenden PDFs.
  [string]$PdfRoot = 'd:\Projekte\src-ZUGFeRD-for-Delphi',
  # Voller Compiler- + Testlog statt kompakter Summary.
  [switch]$ShowAll
)

$ErrorActionPreference = 'Stop'

$repo    = Split-Path (Split-Path (Split-Path $PSScriptRoot -Parent) -Parent) -Parent
$testDir = Join-Path $repo 'Tests\PdfExtract'
$outDir  = Join-Path $testDir 'out'
$unitDir = Join-Path $outDir 'units'
New-Item -ItemType Directory -Force $outDir, $unitDir | Out-Null

function Resolve-FpcExecutable {
  param([string]$RequestedFpc)
  $candidates = @()
  if (-not [string]::IsNullOrWhiteSpace($RequestedFpc)) { $candidates += $RequestedFpc }
  $candidates += @(
    'D:\Bin\fpc\fpcupdeluxe\fpc\bin\x86_64-win64\fpc.exe',
    'D:\Bin\fpc\fpcupdeluxe\fpc\bin\aarch64-win64\fpc.exe',
    'D:\Bin\fpc\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe'
  )
  foreach ($c in $candidates) {
    if (Test-Path $c) { return (Resolve-Path $c).Path }
  }
  $cmd = Get-Command fpc -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }
  throw 'fpc.exe nicht gefunden - bitte -Fpc <Pfad> angeben oder $env:FPC setzen.'
}

$exe      = Join-Path $outDir 'XRechnungPdfExtractTest.exe'
$buildLog = Join-Path $outDir 'XRechnungPdfExtractTest.buildlog'

if ($Compiler -eq 'FPC') {
  $fpcExe = Resolve-FpcExecutable $Fpc
  Write-Host "FPC: $fpcExe"
  & $fpcExe -MDelphiUnicode -B `
      "-Fu$repo;$testDir" "-FU$unitDir" "-FE$outDir" "-o$exe" `
      (Join-Path $testDir 'XRechnungPdfExtractTest.lpr') *>&1 |
    Tee-Object -FilePath $buildLog | Out-Null
} else {
  $dcc = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\DCC32.EXE'
  if (-not (Test-Path $dcc)) { throw "DCC32 nicht gefunden: $dcc" }
  Write-Host "Delphi: $dcc"
  & $dcc -B -U"$repo;$testDir" -E"$outDir" -N"$unitDir" `
      -NS"System;Vcl;Winapi;System.Win;Xml" `
      (Join-Path $testDir 'XRechnungPdfExtractTest.dpr') *>&1 |
    Tee-Object -FilePath $buildLog | Out-Null
}

if (-not (Test-Path $exe)) {
  Write-Host 'Build fehlgeschlagen - Log:' -ForegroundColor Red
  Get-Content $buildLog | Select-Object -Last 30
  exit 1
}
if ($ShowAll) { Get-Content $buildLog }

if (-not (Test-Path $PdfRoot)) {
  Write-Host "PDF-Verzeichnis nicht gefunden: $PdfRoot" -ForegroundColor Yellow
  Write-Host 'Es laeuft nur der Selbsttest.'
}

$runLog = Join-Path $outDir 'XRechnungPdfExtractTest.runlog'
& $exe $PdfRoot *>&1 | Tee-Object -FilePath $runLog | Out-Null
$code = $LASTEXITCODE

if ($ShowAll) {
  Get-Content $runLog
} else {
  Get-Content $runLog | Select-String -Pattern '^(=|-|PDF-Dateien|  |Rechnung|PASS|FAIL|---)' |
    ForEach-Object { $_.Line }
}
exit $code
