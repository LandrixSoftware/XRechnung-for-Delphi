object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XRechnung for Delphi v3.0.x'
  ClientHeight = 695
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
    695)
  TextHeight = 13
  object Label3: TLabel
    Left = 9
    Top = 368
    Width = 82
    Height = 13
    Caption = 'Weitere Beispiele'
  end
  object btCreateInvoice: TButton
    Left = 9
    Top = 323
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
    Height = 497
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 144
    Top = 511
    Width = 1453
    Height = 169
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object rbPaymentTerms: TRadioGroup
    Left = 9
    Top = 126
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
    Left = 9
    Top = 231
    Width = 138
    Height = 17
    Caption = 'Nachlaesse/Zuschlaege'
    TabOrder = 4
  end
  object cbPrepaidAmount: TCheckBox
    Left = 9
    Top = 254
    Width = 129
    Height = 17
    Caption = 'Abschlagsrechnungen'
    TabOrder = 5
  end
  object cbAttachments: TCheckBox
    Left = 9
    Top = 277
    Width = 97
    Height = 17
    Caption = 'Mit Anhaengen'
    TabOrder = 6
  end
  object cbDeliveriyInf: TCheckBox
    Left = 9
    Top = 300
    Width = 97
    Height = 17
    Caption = 'Lieferanschrift'
    TabOrder = 7
  end
  object rbFormat: TRadioGroup
    Left = 8
    Top = 8
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
    Top = 561
    Width = 129
    Height = 25
    Caption = 'Datei laden'
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 592
    Width = 130
    Height = 25
    Caption = 'XRechnung validieren'
    TabOrder = 10
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 8
    Top = 623
    Width = 130
    Height = 25
    Caption = 'XRechnung als HTML'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 654
    Width = 130
    Height = 25
    Caption = 'XRechnung als PDF'
    TabOrder = 12
    OnClick = Button6Click
  end
  object rbVersion: TRadioGroup
    Left = 8
    Top = 67
    Width = 129
    Height = 53
    Caption = 'Ausgabeversion'
    ItemIndex = 1
    Items.Strings = (
      '2.3.1'
      '3.0.1')
    TabOrder = 13
    OnClick = rbVersionClick
  end
  object PageControl1: TPageControl
    Left = 799
    Top = 8
    Width = 798
    Height = 497
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 14
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
    Top = 518
    Width = 130
    Height = 25
    Caption = 'Erzeugen'
    TabOrder = 15
    OnClick = Button9Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 384
    Width = 130
    Height = 128
    ItemHeight = 13
    Items.Strings = (
      'Kleinunternehmerregelung'
      #167'13b UStG'
      'Austauschteilesteuer'
      'Differenzbesteuerung'
      'Titel/Positionsgruppen')
    TabOrder = 16
  end
end
