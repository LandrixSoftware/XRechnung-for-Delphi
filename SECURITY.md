# Sicherheit / Security

[Deutsch](#deutsch) | [English](#english)

## Deutsch

Stand: 18. September 2026

### Eine Schwachstelle melden

Bitte melden Sie mögliche Sicherheitslücken in XRechnung-for-Delphi an den bestehenden
Projektkontakt **[info@landrix.de](mailto:info@landrix.de)**, mit dem Betreff
`[Security] XRechnung-for-Delphi: kurze Beschreibung`.
Bitte eröffnen Sie für noch nicht behobene Schwachstellen zunächst kein öffentliches Issue
und veröffentlichen Sie keine ausnutzbaren Beispieldateien.

Hilfreich sind:

- Bibliotheksversion und möglichst Git-Commit sowie Angaben zu eigenen Änderungen.
- Compiler, Betriebssystem und Architektur; bei Delphi 6 die Angabe der separaten Fassung.
- Betroffener Aufruf, aktivierte Optionen und gegebenenfalls Versionen externer Werkzeuge.
- Schritte zur Reproduktion, erwartetes und beobachtetes Verhalten sowie mögliche Auswirkungen.
- Eine minimale Beispieldatei mit künstlichen Daten; keine echten Rechnungen, Zugangsdaten oder
  personenbezogenen Daten.
- Hinweise auf eine bekannte aktive Ausnutzung und eine erreichbare Kontaktadresse, soweit möglich.

Für sensible Details zunächst nur eine kurze Beschreibung senden und einen geeigneten
Übertragungsweg abstimmen. Die E-Mail-Adresse ist ein privater Kontaktweg, keine Zusage einer
Ende-zu-Ende-verschlüsselten Übertragung oder einer rund um die Uhr besetzten Meldestelle.
Eine Meldung an das Projekt ersetzt keine gegebenenfalls erforderliche Behördenmeldung.

### Unterstützte Versionen und Bearbeitung

Im Repository ist zum oben genannten Stand keine verbindliche Tabelle mit Sicherheits-Supportzeiträumen
oder End-of-Support-Daten je Release veröffentlicht. Eine ältere Version, die Delphi-6-Fassung
oder der aktuelle Entwicklungsstand darf daher nicht allein aufgrund ihrer Verfügbarkeit als
sicherheitsgepflegt eingeordnet werden. Die unterstützten Compiler in der README beschreiben
technische Kompatibilität, keinen Sicherheits-Supportzeitraum.

Meldungen können auch ältere Versionen betreffen. Bitte nennen Sie immer den genauen Stand.
Es werden hier keine festen Reaktionszeiten, Behebungsfristen oder Rückportierungen zugesagt.
Vertragliche Rechte und gesetzliche Pflichten bleiben unberührt; insbesondere ändert dieses
Dokument nicht die Lizenzbedingungen oder bestehende Updateberechtigungen.

Zur Bearbeitung einer Meldung sollten Betroffenheit, Reproduzierbarkeit, Schweregrad, mögliche
Abhilfe und die koordinierte Veröffentlichung geklärt werden. Ein verbindlicher interner Ablauf
mit Zuständigkeiten und Vertretung ist noch gesondert festzulegen; diese Beschreibung ist kein
Nachweis eines bereits eingerichteten Incident-Response-Prozesses.

### Hinweise zur Integration

XML- und PDF-Eingaben sowie eingebettete Anhänge sind als nicht vertrauenswürdig zu behandeln.
Hersteller der einbindenden Anwendung sollten insbesondere Ressourcenlimits, Parserkonfiguration,
Dateizugriffe und den Aufruf externer Werkzeuge prüfen. Rechnungsvalidierung ersetzt diese Prüfungen
nicht. Projektspezifische Prüfpunkte und offene Vorbereitungsarbeiten stehen in der
[CRA-Dokumentation](Documentation/Cyber-Resilience-Act.md#deutsch).

## English

As of 18 September 2026

### Reporting a vulnerability

Please report potential security vulnerabilities in XRechnung-for-Delphi to the existing
project contact **[info@landrix.de](mailto:info@landrix.de)**, using the subject
`[Security] XRechnung-for-Delphi: short description`.
Please do not initially open a public issue for an unresolved vulnerability or publish
files that demonstrate how to exploit it.

Useful information includes:

- Library version and preferably Git commit, together with details of local modifications.
- Compiler, operating system and architecture; specify if you use the separate Delphi 6 version.
- Affected API call, enabled options and versions of any external tools involved.
- Reproduction steps, expected and observed behaviour, and potential impact.
- A minimal sample using synthetic data; no real invoices, credentials or personal data.
- Any known active exploitation and a contact address, where possible.

For sensitive details, initially send only a brief description and agree on a suitable
transfer method. The email address is a private contact channel, not a promise of end-to-end
encrypted transmission or a reporting service staffed around the clock.
Reporting to the project does not replace any required report to authorities.

### Supported versions and handling

As of the date above, the repository does not publish a binding table of security support periods
or end-of-support dates per release. Availability alone therefore does not establish security
maintenance for an older version, the Delphi 6 version or the current development revision.
The supported compilers listed in the README describe technical compatibility, not a security
support period.

Reports may also concern older versions. Please always identify the exact revision.
This document does not promise fixed response times, remediation deadlines or backports.
Contractual rights and statutory obligations remain unaffected; in particular, this document
does not change licence terms or existing update entitlements.

Handling a report should establish affected versions, reproducibility, severity, possible
remediation and coordinated disclosure. A binding internal procedure with responsibilities and
backup contacts still needs to be defined separately; this description is not evidence of an
incident response process already being in place.

### Integration guidance

Treat XML and PDF input and embedded attachments as untrusted. Manufacturers integrating the library
should review resource limits, parser configuration, file access and external tool invocation.
Invoice validation does not replace these checks. Project-specific review topics and outstanding
preparation work are listed in the [CRA documentation](Documentation/Cyber-Resilience-Act.md#english).
