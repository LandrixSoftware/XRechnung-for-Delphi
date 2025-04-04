object Form1: TForm1
  Left = 2088
  Top = 30
  Width = 1630
  Height = 749
  Caption = 'XRechnung for Delphi v3.0.x'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1614
    710)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 378
    Width = 82
    Height = 13
    Caption = 'Weitere Beispiele'
  end
  object btCreateInvoice: TButton
    Left = 8
    Top = 347
    Width = 129
    Height = 25
    Caption = 'Erzeugen'
    TabOrder = 0
    OnClick = btCreateInvoiceClick
  end
  object Memo2: TMemo
    Left = 144
    Top = 8
    Width = 649
    Height = 517
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 144
    Top = 531
    Width = 1453
    Height = 169
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object rbPaymentTerms: TRadioGroup
    Left = 8
    Top = 150
    Width = 129
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
    Top = 255
    Width = 138
    Height = 17
    Caption = 'Nachlaesse/Zuschlaege'
    TabOrder = 4
  end
  object cbPrepaidAmount: TCheckBox
    Left = 8
    Top = 278
    Width = 129
    Height = 17
    Caption = 'Abschlagsrechnungen'
    TabOrder = 5
  end
  object cbAttachments: TCheckBox
    Left = 8
    Top = 301
    Width = 97
    Height = 17
    Caption = 'Mit Anhaengen'
    TabOrder = 6
  end
  object cbDeliveriyInf: TCheckBox
    Left = 8
    Top = 324
    Width = 97
    Height = 17
    Caption = 'Lieferanschrift'
    TabOrder = 7
  end
  object rbFormat: TRadioGroup
    Left = 8
    Top = 32
    Width = 129
    Height = 53
    Caption = 'Ausgabeformat'
    ItemIndex = 0
    Items.Strings = (
      'UBL'
      'ZUGFeRD')
    TabOrder = 8
  end
  object Button2: TButton
    Left = 8
    Top = 597
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Datei laden'
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 625
    Width = 130
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung validieren'
    TabOrder = 10
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 8
    Top = 653
    Width = 130
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung als HTML'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 682
    Width = 130
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung als PDF'
    TabOrder = 12
    OnClick = Button6Click
  end
  object rbVersion: TRadioGroup
    Left = 8
    Top = 91
    Width = 129
    Height = 53
    Caption = 'Ausgabeversion'
    ItemIndex = 1
    Items.Strings = (
      '2.3.1'
      '3.0.2')
    TabOrder = 13
    OnClick = rbVersionClick
  end
  object PageControl1: TPageControl
    Left = 799
    Top = 8
    Width = 798
    Height = 517
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 14
    object TabSheet1: TTabSheet
      Caption = 'Pr'#252'fprotokoll'
      object WebBrowser2: TWebBrowser
        Left = 0
        Top = 0
        Width = 790
        Height = 489
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000A65100008A3200000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
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
    Top = 555
    Width = 130
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Erzeugen'
    TabOrder = 15
    OnClick = Button9Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 394
    Width = 130
    Height = 155
    Anchors = [akLeft, akTop, akBottom]
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
      'Leistungszeitraum je Pos.'
      'Third Party Payment BG-DEX-09')
    TabOrder = 16
  end
  object cbValidateWithJava: TCheckBox
    Left = 8
    Top = 8
    Width = 121
    Height = 17
    Caption = 'Validierung aktiv'
    Checked = True
    State = cbChecked
    TabOrder = 17
  end
end
