object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XRechnung 2.0.0'
  ClientHeight = 1183
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
    Caption = 'XRechnung 2.0.0'
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
    Top = 132
    Width = 75
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
    TabOrder = 5
    ControlData = {
      4C000000B96F0000E33000000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 27
    Width = 129
    Height = 99
    Caption = 'Zahlungsbedingungen'
    ItemIndex = 1
    Items.Strings = (
      'Keine'
      'Netto'
      '1 Skonto'
      '2 Skonto')
    TabOrder = 12
  end
end