# Optionaler ZUGFeRD-Lesepfad (Compiler-Schalter `ZUGFeRD_Support`)

Zum Lesen von ZUGFeRD- und Factur-X-Rechnungen braucht die Bibliothek nichts weiter – der eigene
Lesecode verarbeitet EN16931 und EXTENDED. Wer darüber hinaus an Profil-Inhalte kommen möchte, die
das XRechnungs-Datenmodell nicht abbildet, kann die Schwesterbibliothek
[ZUGFeRD-for-Delphi](https://github.com/LandrixSoftware/ZUGFeRD-for-Delphi) einbinden. Sie liest die
Rechnung dann anstelle des eigenen Readers ein und stellt zusätzlich das vollständige
ZUGFeRD-Objektmodell zur Verfügung.

Dazu in [intf.XRechnung.pas](../intf.XRechnung.pas) den Schalter aktivieren:

```delphi
{$DEFINE ZUGFeRD_Support}
```

und das Verzeichnis von ZUGFeRD-for-Delphi in den Suchpfad aufnehmen.

## TZUGFeRDAdditionalContent

Eine Instanz dieser Klasse kann man optional an `TXRechnungInvoiceAdapter.LoadFrom…()` übergeben.
Sie nimmt die Inhalte auf, die das XRechnungs-Profil nicht kennt – zum Beispiel die abweichende
Rechnungsanschrift – und enthält außerdem die geladene Rechnung als komplettes Objekt, aus dem sich
beliebige weitere ZUGFeRD-Inhalte auslesen lassen.

```delphi
  TZUGFeRDAdditionalContent = class
  public
    ZUGFeRDInvoice : TZUGFeRDInvoiceDescriptor;

    InvoiceeTradePartyFound : Boolean;
    InvoiceeTradeParty : TInvoiceAccountingParty;

    SpecifiedLogisticsServiceChargeFound : Boolean;
  end;
```

```delphi
  var error : String;
  var inv : TInvoice := TInvoice.Create;
  var invAdditionalData : TZUGFeRDAdditionalContent := TZUGFeRDAdditionalContent.Create;
  try
    if TXRechnungInvoiceAdapter.LoadFromFile(inv, aFileName,
                                  error, invAdditionalData) then
    begin
      invAdditionalData.ZUGFeRDInvoice. .....
    end;
  finally
    invAdditionalData.Free;
    inv.Free;
  end;
```

Beachten Sie, dass der Schalter die Parameterliste der Lademethoden verändert: `_AdditionalContent`
steht vor `_ProcessPdfFiles`. Ein Aufruf, der ein PDF zulassen soll, lautet mit Schalter also
`LoadFromFile(inv, datei, error, nil, true)` und ohne Schalter `LoadFromFile(inv, datei, error, true)`.

Die Delphi-6-Fassung unter [Delphi6/](../Delphi6/) kennt diesen Schalter nicht.
