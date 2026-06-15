object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XRechnung for Delphi v3.0.x'
  ClientHeight = 793
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
    793)
  object Label3: TLabel
    Left = 8
    Top = 383
    Width = 82
    Height = 13
    Caption = 'Weitere Beispiele'
  end
  object btCreateInvoice: TButton
    Left = 8
    Top = 352
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
    Height = 595
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 176
    Top = 609
    Width = 1421
    Height = 169
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object rbPaymentTerms: TRadioGroup
    Left = 8
    Top = 163
    Width = 162
    Height = 99
    Caption = 'Zahlungsbedingungen'
    ItemIndex = 1
    Items.Strings = (
      'Keine'
      'Netto'
      '1 Skonto'
      '2 Skonto'
      '3 Skonto')
    TabOrder = 3
  end
  object cbAllowanceCharges: TCheckBox
    Left = 8
    Top = 268
    Width = 138
    Height = 17
    Caption = 'Nachlaesse/Zuschlaege'
    TabOrder = 4
  end
  object cbPrepaidAmount: TCheckBox
    Left = 8
    Top = 288
    Width = 129
    Height = 17
    Caption = 'Abschlagsrechnungen'
    TabOrder = 5
  end
  object cbAttachments: TCheckBox
    Left = 8
    Top = 308
    Width = 97
    Height = 17
    Caption = 'Mit Anhaengen'
    TabOrder = 6
  end
  object cbDeliveriyInf: TCheckBox
    Left = 8
    Top = 329
    Width = 97
    Height = 17
    Caption = 'Lieferanschrift'
    TabOrder = 7
  end
  object rbFormatVersion: TRadioGroup
    Left = 8
    Top = 68
    Width = 162
    Height = 89
    Caption = 'Ausgabeformat'
    ItemIndex = 0
    Items.Strings = (
      'XRechnung 3.0.2 UBL'
      'XRechnung 3.0.2 ZUGFeRD'
      'Extended 2.5 ZUGFeRD'
      'EN16931 2.5 ZUGFeRD')
    TabOrder = 8
  end
  object Button2: TButton
    Left = 8
    Top = 649
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Datei laden'
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 677
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Datei validieren'
    TabOrder = 10
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 8
    Top = 706
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'XRechnung als HTML'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 734
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
    Height = 595
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
      object Panel1: TPanel
        Left = 0
        Top = 540
        Width = 790
        Height = 27
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Button4: TButton
          Left = 8
          Top = 1
          Width = 97
          Height = 25
          Caption = 'Speichern unter'
          TabOrder = 0
          OnClick = Button4Click
        end
        object Button7: TButton
          Left = 111
          Top = 1
          Width = 97
          Height = 25
          Caption = 'Extern '#246'ffnen'
          TabOrder = 1
          OnClick = Button7Click
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'https://valitool.org'
      ImageIndex = 3
    end
  end
  object Button9: TButton
    Left = 8
    Top = 621
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Erzeugen'
    TabOrder = 14
    OnClick = Button9Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 400
    Width = 162
    Height = 215
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
      'Third Party Payment BG-DEX-09'
      '4 Nachkommastellen')
    TabOrder = 15
  end
  object cbValidateWithJava: TCheckBox
    Left = 8
    Top = 8
    Width = 138
    Height = 17
    Caption = 'Validierung JAVA aktiv'
    Checked = True
    State = cbChecked
    TabOrder = 16
  end
  object Button1: TButton
    Left = 8
    Top = 760
    Width = 162
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Alle erzeugen'
    TabOrder = 17
    OnClick = Button1Click
  end
  object cbVisualizeWithJava: TCheckBox
    Left = 8
    Top = 45
    Width = 125
    Height = 17
    Caption = 'Visualisierung aktiv'
    Checked = True
    State = cbChecked
    TabOrder = 18
  end
  object cbValidateWithExtern: TCheckBox
    Left = 8
    Top = 26
    Width = 162
    Height = 17
    Caption = 'Validierung Extern aktiv'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 19
  end
end
