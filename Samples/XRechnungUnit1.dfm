object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XRechnung for Delphi v2.0.1'
  ClientHeight = 1185
  ClientWidth = 1910
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1910
    1185)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 8
    Width = 83
    Height = 13
    Caption = 'XRechnung 1.2.2'
  end
  object Label2: TLabel
    Left = 152
    Top = 506
    Width = 83
    Height = 13
    Caption = 'XRechnung 2.0.1'
  end
  object Label3: TLabel
    Left = 8
    Top = 309
    Width = 82
    Height = 13
    Caption = 'Weitere Beispiele'
  end
  object Memo1: TMemo
    Left = 152
    Top = 27
    Width = 649
    Height = 473
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button3: TButton
    Left = 8
    Top = 264
    Width = 129
    Height = 25
    Caption = 'Erzeugen'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Memo2: TMemo
    Left = 152
    Top = 525
    Width = 649
    Height = 473
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object WebBrowser1: TWebBrowser
    Left = 816
    Top = 27
    Width = 1081
    Height = 473
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ControlData = {
      4C000000B96F0000E33000000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Memo3: TMemo
    Left = 152
    Top = 1004
    Width = 1745
    Height = 169
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object WebBrowser2: TWebBrowser
    Left = 816
    Top = 525
    Width = 1081
    Height = 473
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    ControlData = {
      4C000000B96F0000E33000000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object rbPaymentTerms: TRadioGroup
    Left = 8
    Top = 67
    Width = 129
    Height = 99
    Caption = 'Zahlungsbedingungen'
    ItemIndex = 1
    Items.Strings = (
      'Keine'
      'Netto'
      '1 Skonto'
      '2 Skonto')
    TabOrder = 6
  end
  object cbAllowanceCharges: TCheckBox
    Left = 8
    Top = 172
    Width = 138
    Height = 17
    Caption = 'Nachlaesse/Zuschlaege'
    TabOrder = 7
  end
  object Button4: TButton
    Left = 8
    Top = 335
    Width = 129
    Height = 25
    Caption = 'Titel / Positionsgruppen'
    TabOrder = 8
    OnClick = Button4Click
  end
  object cbPrepaidAmount: TCheckBox
    Left = 8
    Top = 195
    Width = 129
    Height = 17
    Caption = 'Abschlagsrechnungen'
    TabOrder = 9
  end
  object btX1ConvertHTML: TButton
    Left = 632
    Top = 440
    Width = 137
    Height = 33
    Caption = 'XRechnung nach HTML'
    TabOrder = 10
    Visible = False
    OnClick = btX1ConvertHTMLClick
  end
  object btX2ConvertHTML: TButton
    Left = 632
    Top = 937
    Width = 137
    Height = 33
    Caption = 'XRechnung nach HTML'
    TabOrder = 11
    Visible = False
    OnClick = btX2ConvertHTMLClick
  end
  object Button1: TButton
    Left = 8
    Top = 366
    Width = 129
    Height = 25
    Caption = 'UStG '#167' 13b'
    TabOrder = 12
    OnClick = Button1Click
  end
  object cbAttachments: TCheckBox
    Left = 8
    Top = 218
    Width = 97
    Height = 17
    Caption = 'Mit Anhaengen'
    TabOrder = 13
  end
  object cbDeliveriyInf: TCheckBox
    Left = 8
    Top = 241
    Width = 97
    Height = 17
    Caption = 'Lieferanschrift'
    TabOrder = 14
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
    TabOrder = 15
  end
  object Button2: TButton
    Left = 8
    Top = 1142
    Width = 129
    Height = 25
    Caption = 'Laden'
    TabOrder = 16
    OnClick = Button2Click
  end
end
