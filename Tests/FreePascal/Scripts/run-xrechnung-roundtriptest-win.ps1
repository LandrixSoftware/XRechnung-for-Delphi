<#
  Baut + laeuft den FreePascal-Roundtriptest NATIV unter Windows mit FPC.

  Prueft den LESEPFAD: liest jedes Golden-File aus ValidXMLExamples\ mit der
  FPC-portierten XRechnung-Bibliothek ein (fcl-xml + XPath ueber
  intf.XRechnungXmlShim / intf.XRechnungHelper), schreibt die TInvoice wieder
  heraus und vergleicht das Ergebnis KANONISCH gegen das Golden-File.

  Lesen und Schreiben sind fuer alle Golden-Files verlustfrei - unter Delphi
  ebenso wie unter FPC. Jede Abweichung ist damit ein echter Lesefehler.

  Ergebnis: Tests\FreePascal\out\XRechnungRoundtripTest.exe. Konsole zeigt nur
  die kompakte PASS/FAIL-Summary; ExitCode wird durchgereicht (0 = alles ok).

  Beispiele:
    .\run-xrechnung-roundtriptest-win.ps1
    .\run-xrechnung-roundtriptest-win.ps1 -ShowAll          # voller Compiler-/Testlog
    .\run-xrechnung-roundtriptest-win.ps1 -Fpc D:\pfad\fpc.exe

  Logs: Tests\FreePascal\out\XRechnungRoundtripTest.buildlog (Compiler),
        Tests\FreePascal\out\XRechnungRoundtripTest.runlog   (Testlauf).
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
$Lpr     = Join-Path $TestSrc 'XRechnungRoundtripTest.lpr'
$Exe     = Join-Path $Out 'XRechnungRoundtripTest.exe'
$BuildLog = Join-Path $Out 'XRechnungRoundtripTest.buildlog'
$RunLog   = Join-Path $Out 'XRechnungRoundtripTest.runlog'

New-Item -ItemType Directory -Force -Path $Out | Out-Null

# Unit-Suchpfade: Repo-Root (Kern-Units + Shim + Helper), Tests\FreePascal
# (XRechnungXmlCompare).
$fu = @($Root, $TestSrc) -join ';'

Write-Host ">>> XRechnungRoundtripTest ($FpcArch)"

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
  Write-Error "FAIL(compile): XRechnungRoundtripTest (rc=$buildRc) - Details: $BuildLog"
  exit $buildRc
}

# --- Testlauf ---
$runOutput = & $Exe $Root 2>&1
$runRc = $LASTEXITCODE
$runOutput | Set-Content -Path $RunLog -Encoding utf8

if ($ShowAll) {
  $runOutput
} else {
  $runOutput | Where-Object { $_ -match '^(PASS|FAIL|>>>|\[DIFF\]|\[LOAD\]|\[SAVE\]|\[VERSION\]|---)' } |
    ForEach-Object { if ($_.Length -gt 200) { $_.Substring(0,200) + ' ...' } else { $_ } }
}

if ($runRc -eq 0) {
  exit 0
}
Write-Error "FAIL: Roundtriptest fehlgeschlagen (rc=$runRc). Voller Log: $RunLog"
exit $runRc
