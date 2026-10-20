<#
  Baut + laeuft den FreePascal-Praefixtest NATIV unter Windows mit FPC.

  Prueft Erkennung und Lesen von Rechnungen mit frei gewaehlten
  Namespace-Praefixen (Issue #92): jedes Golden-File aus ValidXMLExamples\
  wird mit fremden Praefixen und mit Default-Namespace umgeschrieben,
  eingelesen, zurueckgeschrieben und KANONISCH gegen das Original verglichen.
  Dazu Negativfaelle (SelfBilledInvoice, InvoiceResponse), die abgelehnt
  werden muessen.

  Ergebnis: Tests\FreePascal\out\XRechnungPrefixTest.exe. Konsole zeigt nur
  die kompakte PASS/FAIL-Summary; ExitCode wird durchgereicht (0 = alles ok).

  Beispiele:
    .\run-xrechnung-prefixtest-win.ps1
    .\run-xrechnung-prefixtest-win.ps1 -ShowAll          # voller Compiler-/Testlog
    .\run-xrechnung-prefixtest-win.ps1 -Fpc D:\pfad\fpc.exe

  Logs: Tests\FreePascal\out\XRechnungPrefixTest.buildlog (Compiler),
        Tests\FreePascal\out\XRechnungPrefixTest.runlog   (Testlauf).
#>
[CmdletBinding()]
param(
  # Pfad zu fpc.exe (sonst $env:FPC bzw. bekannte fpcupdeluxe/Lazarus-Pfade).
  [string]$Fpc = $env:FPC,
  # Voller Compiler- + Testlog statt kompakter Summary.
  [switch]$ShowAll
)

$ErrorActionPreference = 'Stop'

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
  throw "fpc.exe nicht gefunden. Per -Fpc oder `$env:FPC setzen. Geprueft: $($candidates -join ', ')"
}

$Fpc = Resolve-FpcExecutable $Fpc

# Zielplattform nur erzwingen, wenn der gefundene Compiler ein x86_64-Compiler
# ist. Auf einer aarch64-Installation gibt es ppcrossx64 in der Regel nicht,
# und -Px86_64 laesst FPC dann mit "Failed to execute" abbrechen - dort wird
# nativ gebaut.
$TargetArgs = @()
if ($Fpc -match 'x86_64-win64') { $TargetArgs = @('-Twin64','-Px86_64') }
$FpcArch = if ($Fpc -match 'aarch64-win64') { 'aarch64-win64' } else { 'x86_64-win64' }

# Repo-Root: Tests\FreePascal\Scripts -> Tests\FreePascal -> Tests -> Repo
$Root   = (Resolve-Path "$PSScriptRoot\..\..\..").Path
$TestSrc = Join-Path $Root 'Tests\FreePascal'
$Out     = Join-Path $TestSrc 'out'
$Lpr     = Join-Path $TestSrc 'XRechnungPrefixTest.lpr'
$Exe     = Join-Path $Out 'XRechnungPrefixTest.exe'
$BuildLog = Join-Path $Out 'XRechnungPrefixTest.buildlog'
$RunLog   = Join-Path $Out 'XRechnungPrefixTest.runlog'

New-Item -ItemType Directory -Force -Path $Out | Out-Null

# Unit-Suchpfade: Repo-Root (Kern-Units + Shim + Helper), Tests\FreePascal
# (XRechnungXmlCompare).
$fu = @($Root, $TestSrc) -join ';'

Write-Host ">>> XRechnungPrefixTest ($FpcArch)"

# --- Build (Compiler-Output gefiltert; voller Log in $BuildLog) ---
$build = & $Fpc -MDelphiUnicode -B @TargetArgs `
  "-Fu$fu" "-FU$Out" "-FE$Out" "-o$Exe" $Lpr 2>&1
$buildRc = $LASTEXITCODE
$build | Set-Content -Path $BuildLog -Encoding utf8

if ($ShowAll) {
  $build
} else {
  # Nur Errors/Fatals; die (erwarteten, gutartigen) Implicit-string-Warnungen
  # ausblenden.
  $build | Where-Object { $_ -match '(Error|Fatal):' }
}

if ($buildRc -ne 0) {
  Write-Error "FAIL(compile): XRechnungPrefixTest (rc=$buildRc) - Details: $BuildLog"
  exit $buildRc
}

# --- Testlauf ---
$runOutput = & $Exe $Root 2>&1
$runRc = $LASTEXITCODE
$runOutput | Set-Content -Path $RunLog -Encoding utf8

if ($ShowAll) {
  $runOutput
} else {
  $runOutput | Where-Object { $_ -match '^(PASS|FAIL|>>>|\[DIFF\]|\[LOAD\]|\[SAVE\]|\[REJECT\]|\[VERSION\]|---)' } |
    ForEach-Object { if ($_.Length -gt 200) { $_.Substring(0,200) + ' ...' } else { $_ } }
}

if ($runRc -eq 0) {
  exit 0
}
Write-Error "FAIL: Praefixtest fehlgeschlagen (rc=$runRc). Voller Log: $RunLog"
exit $runRc
