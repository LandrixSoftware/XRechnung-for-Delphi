object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XRechnung for Delphi v3.0.x'
  ClientHeight = 742
  ClientWidth = 1605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1605
    742)
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 359
    Width = 82
    Height = 13
    Caption = 'Weitere Beispiele'
  end
  object btCreateInvoice: TButton
    Left = 8
    Top = 328
    Width = 162
    Height = 25
    Caption = 'Erzeugen'
    TabOrder = 0
    OnClick = btCreateInvoiceClick
  end
  object Memo2: TMemo
    Left = 176
    Top = 8
    Width = 617
    Height = 544
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 176
    Top = 558
    Width = 1421
    Height = 169
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object rbPaymentTerms: TRadioGroup
    Left = 8
    Top = 131
    Width = 162
    Height = 99
    Caption = 'Zahlungsbedingungen'
    ItemIndex = 1
    Items.Strings = (
      'Keine'
      'Netto'
      '1 Skonto'
      '2 Skonto')
    TabOrder = 3
  end
  object cbAllowanceCharges: TCheckBox
    Left = 8
    Top = 236
    Width = 138
    Height = 17
    Caption = 'Nachlaesse/Zuschlaege'
    TabOrder = 4
  end
  object cbPrepaidAmount: TCheckBox
    Left = 8
    Top = 259
    Width = 129
    Height = 17
    Caption = 'Abschlagsrechnungen'
    TabOrder = 5
  end
  object cbAttachments: TCheckBox
    Left = 8
    Top = 282
    Width = 97
    Height = 17
    Caption = 'Mit Anhaengen'
    TabOrder = 6
  end
  object cbDeliveriyInf: TCheckBox
    Left = 8
    Top = 305
    Width = 97
    Height = 17
    Caption = 'Lieferanschrift'
    TabOrder = 7
  end
  object rbFormatVersion: TRadioGroup
    Left = 8
    Top = 52
    Width = 162
    Height = 73
    Caption = 'Ausgabeformat'
    ItemIndex = 0
    Items.Strings = (
      'XRechnung 3.0.2 UBL'
      'XRechnung 3.0.2 ZUGFeRD'
      'Extended 2.3.2 ZUGFeRD')
    TabOrder = 8
  end
  object Button2: TButton
    Left = 8
    Top = 598
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Datei laden'
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 626
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung validieren'
    TabOrder = 10
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 8
    Top = 655
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung als HTML'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 683
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung als PDF'
    TabOrder = 12
    OnClick = Button6Click
  end
  object PageControl1: TPageControl
    Left = 799
    Top = 8
    Width = 798
    Height = 544
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 13
    object TabSheet1: TTabSheet
      Caption = 'Pr'#252'fprotokoll'
    end
    object TabSheet2: TTabSheet
      Caption = 'HTML'
      ImageIndex = 1
    end
    object TabSheet3: TTabSheet
      Caption = 'PDF'
      ImageIndex = 2
    end
  end
  object Button9: TButton
    Left = 8
    Top = 570
    Width = 162
    Height = 25
    Caption = 'Erzeugen'
    TabOrder = 14
    OnClick = Button9Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 376
    Width = 162
    Height = 188
    ItemHeight = 13
    Items.Strings = (
      'Kleinunternehmerregelung'
      #167'13b UStG'
      'Austauschteilesteuer'
      'Differenzbesteuerung'
      'Titel/Positionsgruppen'
      'Gutschrift'
      'Rechnungskorrektur/Storno'
      'Minimalrechnung B2B'
      'Preiseinheit'
      'Lastschrift'
      'Innergem.Lief.EUoMwSt'
      'PayPal'
      'Kreditkarte'
      'Leistungszeitraum je Pos.')
    TabOrder = 15
  end
  object cbValidateWithJava: TCheckBox
    Left = 8
    Top = 8
    Width = 121
    Height = 17
    Caption = 'Validierung aktiv'
    Checked = True
    State = cbChecked
    TabOrder = 16
  end
  object Button1: TButton
    Left = 8
    Top = 712
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Alle erzeugen'
    TabOrder = 17
    OnClick = Button1Click
  end
  object cbVisualizeWithJava: TCheckBox
    Left = 8
    Top = 28
    Width = 125
    Height = 17
    Caption = 'Visualisierung aktiv'
    Checked = True
    State = cbChecked
    TabOrder = 18
  end
end
