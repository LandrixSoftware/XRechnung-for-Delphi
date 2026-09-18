# Cyber Resilience Act – XRechnung-for-Delphi

[Deutsch](#deutsch) | [English](#english) | [Quellen / Sources](#sources)

## Deutsch

Stand: 18. September 2026. Dieses Dokument beschreibt den dokumentierten Projektstand und
empfohlene Vorbereitungsarbeiten. Es ist keine Konformitätserklärung oder abgeschlossene
rechtliche Einordnung des Produkts.

### Einordnung und Termine

Der Cyber Resilience Act (CRA), Verordnung (EU) 2024/2847, betrifft Produkte mit digitalen
Elementen. Die wesentlichen Pflichten gelten ab **11. Dezember 2027**. Hersteller-Meldepflichten
nach Artikel 14 gelten bereits seit **11. September 2026** für aktiv ausgenutzte Schwachstellen
und schwerwiegende Sicherheitsvorfälle. Übergangsregeln für bereits in Verkehr gebrachte Produkte
sind gesondert zu berücksichtigen. [EU-Überblick](https://digital-strategy.ec.europa.eu/en/policies/cra-summary),
[Meldepflichten](https://digital-strategy.ec.europa.eu/en/policies/cra-reporting)

XRechnung-for-Delphi wird unter GPL und einer kommerziellen Lizenz angeboten. Öffentlicher Quellcode
allein begründet keine CRA-Ausnahme; kommerzielle Bereitstellung und die konkrete Rolle sind
entscheidend. Die Rolle von Landrix und die Pflichten für die Bibliothek sind deshalb ausdrücklich
zu bewerten. Dieses Dokument nimmt weder eine Ausnahme noch eine Einstufung als Open-Source-Steward
vorweg. [EU-Hinweise zu Open Source](https://digital-strategy.ec.europa.eu/en/policies/cra-open-source)

### Bibliothek und einbindende Anwendung

Als Arbeitsaufteilung für die Vorbereitung empfiehlt sich:

- **Bibliotheksanbieter:** eigene Produkteinordnung dokumentieren, Releases identifizieren,
  Komponenten und Sicherheitsbefunde erfassen, Meldeweg und Pflegeplanung festlegen.
- **Hersteller der Anwendung:** die tatsächlich verwendete Fassung einschließlich eigener Änderungen
  erfassen, die Risiken der Integration bewerten und Updates im eigenen Produkt ausliefern.
  Auch zusätzlich gebündelte Werkzeuge und die Betriebsumgebung berücksichtigen.

Diese Arbeitsaufteilung ersetzt nicht die Prüfung der gesetzlichen Pflichten beider Seiten.
Eine SBOM, ein bestandener Rechnungstest oder die Verwendung dieser Bibliothek allein belegt
keine CRA-Konformität des fertigen Produkts.

### Komponenten erfassen

Die folgende Übersicht ist eine Grundlage für die Bestandsaufnahme, **keine vollständige SBOM**.
Versionen und transitive Abhängigkeiten sind anhand des konkreten Builds zu ergänzen.

| Bereich | Im Build oder in der Auslieferung zu prüfen |
|---|---|
| Pascal-Bibliothek | Verwendete Units, Release und Git-Commit, lokale Änderungen, gegebenenfalls separate `Delphi6/`-Fassung |
| Delphi / Windows | Compiler und Laufzeitbibliotheken; XML-Verarbeitung über MSXML; PDF-Dekompression über `System.ZLib` |
| FreePascal | Compiler und Laufzeitbibliotheken; `fcl-xml` über den XML-Shim; PDF-Dekompression über `zstream` |
| Optionaler ZUGFeRD-Lesepfad | ZUGFeRD-for-Delphi und dessen verwendete Abhängigkeiten bei aktiviertem `ZUGFeRD_Support` |
| Optionale Java-Werkzeuge | Tatsächlich installierte bzw. ausgelieferte JRE, KoSIT-Validator, Saxon, Apache FOP, Mustang und deren Abhängigkeiten |
| Regeln und Visualisierung | Verwendete Schemas, Validator-Konfigurationen und Stylesheets einschließlich Herkunft und Version |

Ausgangspunkt sind die [README](../README.md), die
[Versionsinformationen](../intf.XRechnungVersionInfo.pas), der
[PDF-Extraktor](../intf.XRechnungPdfExtract.pas), der [XML-Shim](../intf.XRechnungXmlShim.pas)
und der [Installer](../Distribution/installtools.ps1). Die Installer-Historie ist kein Nachweis
der tatsächlich beim Kunden installierten Versionen. Build-Werkzeuge, eingebundene Komponenten
und vorausgesetzte Systemkomponenten sollten im Inventar unterscheidbar sein.

### Sicherheitsbewertung der Integration

Die folgenden Punkte sind Prüffragen, keine Aussagen über bereits nachgewiesene Schutzmaßnahmen
oder bekannte Sicherheitslücken:

- **XML:** DTDs, externe Entitäten und externe Ressourcen für jeden Parserpfad prüfen;
  Größen-, Tiefen- und Laufzeitgrenzen festlegen und mit fehlerhaften Eingaben testen.
- **PDF:** Objektverweise, verschachtelte Strukturen und dekomprimierte Datenmengen begrenzen;
  Verhalten bei beschädigten oder absichtlich aufwendigen Dateien prüfen.
- **Anhänge:** Namen und Pfade vor dem Speichern prüfen, unbeabsichtigtes Überschreiben verhindern
  und extrahierte Inhalte nicht automatisch ausführen.
- **Externe Werkzeuge:** Aufrufparameter, temporäre Dateien, Zugriffsrechte und Ressourcenlimits
  prüfen; Downloads und Updates nachvollziehbar auf Herkunft und Integrität prüfen.
- **Betrieb:** Rechte des verarbeitenden Prozesses minimieren; bei Bedarf Prozessisolation
  einsetzen und vertrauliche Rechnungsinhalte aus Diagnoseprotokollen fernhalten.

Vorhandene [FreePascal-Tests](../Tests/FreePascal/README.md) und
[PDF-Tests](../Tests/PdfExtract/README.md) sind Ausgangspunkte für weitere Prüfungen.
Funktionstests und Rechnungsvalidierung ersetzen keine Sicherheitsbewertung.

### Stand und nächste Arbeiten

Die folgenden Punkte sind mit dieser Dokumentation noch nicht als erledigt nachgewiesen:

- [ ] Anwendungsbereich, Herstellerrolle und gegebenenfalls Produktkategorie dokumentiert bewerten.
- [ ] Verantwortliche und Vertretung für Sicherheit sowie einen internen Melde- und Eskalationsablauf festlegen.
- [ ] Unterstützte Releases, Supportzeiträume, Enddaten und Umgang mit Rückportierungen veröffentlichen.
- [ ] Für einen konkreten Release eine maschinenlesbare SBOM erstellen, beispielsweise in CycloneDX oder SPDX;
  eingebundene und transitive Komponenten anhand der tatsächlichen Artefakte prüfen.
- [ ] Schwachstellenbeobachtung, Bewertung der Betroffenheit und Behebungsentscheidungen dokumentieren;
  dabei auch Befunde ohne CVE-Kennung berücksichtigen.
- [ ] Sicherheitsbewertung und gezielte Tests für die oben genannten Eingabepfade dokumentieren.
- [ ] Technische Dokumentation und erforderliche Konformitätsunterlagen anhand der festgestellten Pflichten erstellen.

Ein privater Kontaktweg und der bisher dokumentierte Supportstand stehen in
[SECURITY.md](../SECURITY.md#deutsch). Gesetzliche Meldefristen sind unabhängig von internen
Antwortzeiten zu behandeln; eine Meldung an Landrix ersetzt keine erforderliche Behördenmeldung.

Für jeden Release sollte ein Nachweispaket mindestens folgende **Arbeitsunterlagen** zusammenführen:
Release/Commit und Build-Konfiguration, Komponenteninventar/SBOM, Testberichte und Risikobewertung,
bekannte Einschränkungen und Abhilfen, Sicherheitsänderungen sowie festgelegte Supportdaten.
Das ist eine Arbeitsliste für dieses Projekt, kein vollständiges gesetzliches Konformitätspaket.

## English

As of 18 September 2026. This document describes the documented project status and recommended
preparation work. It is not a declaration of conformity or a completed legal classification
of the product.

### Context and dates

The Cyber Resilience Act (CRA), Regulation (EU) 2024/2847, concerns products with digital elements.
Its main obligations apply from **11 December 2027**. Manufacturer reporting obligations under
Article 14 have applied since **11 September 2026** to actively exploited vulnerabilities and
severe security incidents. Transitional rules for products already placed on the market need
separate consideration. [EU overview](https://digital-strategy.ec.europa.eu/en/policies/cra-summary),
[reporting obligations](https://digital-strategy.ec.europa.eu/en/policies/cra-reporting)

XRechnung-for-Delphi is offered under the GPL and a commercial licence. Public source code alone
does not establish a CRA exemption; commercial supply and the specific role matter.
Landrix's role and the obligations for the library therefore need an explicit assessment.
This document does not presume an exemption or classification as an open-source software steward.
[EU guidance on open source](https://digital-strategy.ec.europa.eu/en/policies/cra-open-source)

### Library and integrating application

The following allocation of preparation work is recommended:

- **Library supplier:** document its product classification, identify releases, record components
  and security findings, and define the reporting channel and maintenance plan.
- **Application manufacturer:** record the exact revision used, including local modifications,
  assess integration risks and deliver updates in its own product. Include any additional
  bundled tools and the operating environment.

This allocation does not replace an assessment of either party's statutory obligations.
A SBOM, a passing invoice test or use of this library alone does not demonstrate CRA compliance
of the finished product.

### Recording components

The following overview is a starting point for an inventory, **not a complete SBOM**.
Versions and transitive dependencies need to be added for the specific build.

| Area | Items to check in the build or distribution |
|---|---|
| Pascal library | Units used, release and Git commit, local modifications, separate `Delphi6/` version where applicable |
| Delphi / Windows | Compiler and runtime libraries; XML processing through MSXML; PDF decompression through `System.ZLib` |
| FreePascal | Compiler and runtime libraries; `fcl-xml` through the XML shim; PDF decompression through `zstream` |
| Optional ZUGFeRD reading path | ZUGFeRD-for-Delphi and its dependencies used when `ZUGFeRD_Support` is enabled |
| Optional Java tools | JRE, KoSIT validator, Saxon, Apache FOP, Mustang and their dependencies actually installed or distributed |
| Rules and visualisation | Schemas, validator configurations and stylesheets used, including origin and version |

Starting points are the [README](../README.md), [version information](../intf.XRechnungVersionInfo.pas),
[PDF extractor](../intf.XRechnungPdfExtract.pas), [XML shim](../intf.XRechnungXmlShim.pas)
and [installer](../Distribution/installtools.ps1). The installer history does not prove which versions
are actually installed at a customer's site. The inventory should distinguish build tools,
included components and required system components.

### Assessing integration security

The following are review questions, not claims of verified safeguards or known vulnerabilities:

- **XML:** review DTDs, external entities and external resources for every parser path;
  define size, depth and execution time limits and test malformed input.
- **PDF:** limit object references, nested structures and decompressed data sizes;
  check behaviour with damaged or deliberately expensive files.
- **Attachments:** check names and paths before saving, prevent unintended overwrites and
  do not automatically execute extracted content.
- **External tools:** review invocation arguments, temporary files, access permissions and resource
  limits; check and record the origin and integrity of downloads and updates.
- **Operation:** minimise privileges of the processing process; use process isolation where
  appropriate and keep confidential invoice content out of diagnostic logs.

Existing [FreePascal tests](../Tests/FreePascal/README.md) and
[PDF tests](../Tests/PdfExtract/README.md) provide starting points for further checks.
Functional tests and invoice validation do not replace a security assessment.

### Status and next steps

This documentation does not yet demonstrate completion of the following items:

- [ ] Document an assessment of scope, manufacturer role and product category where applicable.
- [ ] Assign security responsibilities and backup contacts, and define internal reporting and escalation procedures.
- [ ] Publish supported releases, support periods, end dates and the approach to backports.
- [ ] Create a machine-readable SBOM for a specific release, for example in CycloneDX or SPDX;
  verify included and transitive components against the actual artifacts.
- [ ] Document vulnerability monitoring, applicability assessments and remediation decisions,
  including findings without a CVE identifier.
- [ ] Document a security assessment and targeted tests for the input paths listed above.
- [ ] Prepare technical documentation and required conformity documents based on the obligations identified.

A private contact channel and the currently documented support status are described in
[SECURITY.md](../SECURITY.md#english). Statutory reporting deadlines need to be handled independently
of internal response times; reporting to Landrix does not replace any required report to authorities.

For each release, an evidence package should bring together at least the following **working records**:
release/commit and build configuration, component inventory/SBOM, test reports and risk assessment,
known limitations and mitigations, security changes and defined support dates.
This is a working list for this project, not a complete statutory conformity package.

<a id="sources"></a>

## Quellen / Sources

Die oben verlinkten Informationen der EU-Kommission wurden am 18. September 2026 geprüft;
der Verordnungstext ist ergänzend verlinkt. / The European Commission information linked above
was checked on 18 September 2026; links to the regulation are provided for further reference.

- [EU-Kommission: CRA-Überblick / European Commission: CRA summary](https://digital-strategy.ec.europa.eu/en/policies/cra-summary)
- [EU-Kommission: Meldepflichten / European Commission: reporting obligations](https://digital-strategy.ec.europa.eu/en/policies/cra-reporting)
- [EU-Kommission: Open Source / European Commission: open source](https://digital-strategy.ec.europa.eu/en/policies/cra-open-source)
- Verordnung / Regulation (EU) 2024/2847: [Deutsch](https://eur-lex.europa.eu/eli/reg/2024/2847/oj?locale=de) / [English](https://eur-lex.europa.eu/eli/reg/2024/2847/oj?locale=en)
