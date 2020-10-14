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
  object Label3: TLabel
    Left = 8
    Top = 253
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
    Top = 163
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
  object rbPaymentTerms: TRadioGroup
    Left = 8
    Top = 11
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
  object Button1: TButton
    Left = 8
    Top = 1150
    Width = 129
    Height = 25
    Caption = 'Viewer starten'
    TabOrder = 7
    OnClick = Button1Click
  end
  object pnStartDragX122: TPanel
    Left = 552
    Top = 440
    Width = 225
    Height = 33
    Caption = 'Ziehe XRechnung von hier in den Viewer'
    TabOrder = 8
    Visible = False
    OnMouseDown = pnStartDragX200MouseDown
  end
  object pnStartDragX200: TPanel
    Left = 552
    Top = 936
    Width = 225
    Height = 33
    Caption = 'Ziehe XRechnung von hier in den Viewer'
    TabOrder = 9
    Visible = False
    OnMouseDown = pnStartDragX200MouseDown
  end
  object cbAllowanceCharges: TCheckBox
    Left = 8
    Top = 116
    Width = 138
    Height = 17
    Caption = 'Nachlaesse/Zuschlaege'
    TabOrder = 10
  end
  object Button2: TButton
    Left = 8
    Top = 272
    Width = 129
    Height = 25
    Caption = 'Abschlagsrechnungen'
    TabOrder = 11
  end
  object Button4: TButton
    Left = 8
    Top = 303
    Width = 129
    Height = 25
    Caption = 'Titel / Positionsgruppen'
    TabOrder = 12
  end
  object cbPrepaidAmount: TCheckBox
    Left = 8
    Top = 139
    Width = 129
    Height = 17
    Caption = 'Anzahlungen'
    TabOrder = 19
  end
end
