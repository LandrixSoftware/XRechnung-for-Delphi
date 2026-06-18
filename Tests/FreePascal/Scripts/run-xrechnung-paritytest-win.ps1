<#
  Baut + laeuft den FreePascal-Paritaetstest NATIV unter Windows mit FPC.

  Der Test erzeugt alle gueltigen XML-Beispiele mit der FPC-portierten
  XRechnung-Bibliothek (Schreibpfad ueber fcl-xml) und vergleicht sie KANONISCH
  (DOM-Baum, Whitespace-/Attributreihenfolge-unabhaengig) gegen die von der
  Delphi-Version erzeugten Golden-Files unter ValidXMLExamples\.

  Ergebnis: Tests\FreePascal\out\XRechnungParityTest.exe. Konsole zeigt nur die
  kompakte PASS/FAIL-Summary; ExitCode wird durchgereicht (0 = alle identisch).

  Beispiele:
    .\run-xrechnung-paritytest-win.ps1
    .\run-xrechnung-paritytest-win.ps1 -ShowAll          # voller Compiler-/Testlog
    .\run-xrechnung-paritytest-win.ps1 -Fpc D:\pfad\fpc.exe

  Logs: Tests\FreePascal\out\XRechnungParityTest.buildlog (Compiler),
        Tests\FreePascal\out\XRechnungParityTest.runlog   (Testlauf).
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
    'D:\Bin\fpc\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe'
  )
  foreach ($c in $candidates) {
    if (Test-Path $c) { return (Resolve-Path $c).Path }
  }
  throw "fpc.exe nicht gefunden. Per -Fpc oder `$env:FPC setzen. Geprueft: $($candidates -join ', ')"
}

$Fpc = Resolve-FpcExecutable $Fpc

# Repo-Root: Tests\FreePascal\Scripts -> Tests\FreePascal -> Tests -> Repo
$Root   = (Resolve-Path "$PSScriptRoot\..\..\..").Path
$TestSrc = Join-Path $Root 'Tests\FreePascal'
$Out     = Join-Path $TestSrc 'out'
$Lpr     = Join-Path $TestSrc 'XRechnungParityTest.lpr'
$Exe     = Join-Path $Out 'XRechnungParityTest.exe'
$BuildLog = Join-Path $Out 'XRechnungParityTest.buildlog'
$RunLog   = Join-Path $Out 'XRechnungParityTest.runlog'

New-Item -ItemType Directory -Force -Path $Out | Out-Null

# Unit-Suchpfade: Repo-Root (die 4 portierten Units + Shim), Samples
# (XRechnungUnit2TestCases), Tests\FreePascal (Generator).
$fu = @($Root, (Join-Path $Root 'Samples'), $TestSrc) -join ';'

Write-Host ">>> XRechnungParityTest (x86_64-win64)"

# --- Build (Compiler-Output gefiltert; voller Log in $BuildLog) ---
$build = & $Fpc -MDelphiUnicode -B -Twin64 -Px86_64 `
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
  Write-Error "FAIL(compile): XRechnungParityTest (rc=$buildRc) - Details: $BuildLog"
  exit $buildRc
}

# --- Testlauf ---
$runOutput = & $Exe $Root 2>&1
$runRc = $LASTEXITCODE
$runOutput | Set-Content -Path $RunLog -Encoding utf8

if ($ShowAll) {
  $runOutput
} else {
  # Summary-Zeile + (falls vorhanden) Abweichungen, aber ohne die langen
  # Base64-Diffs vollstaendig auf die Konsole zu kippen.
  $runOutput | Where-Object { $_ -match '^(PASS|FAIL|>>>|\[DIFF\]|\[FEHLT\]|---)' } |
    ForEach-Object { if ($_.Length -gt 200) { $_.Substring(0,200) + ' ...' } else { $_ } }
}

if ($runRc -eq 0) {
  exit 0
}
Write-Error "FAIL: Paritaetstest fehlgeschlagen (rc=$runRc). Voller Log: $RunLog"
exit $runRc
