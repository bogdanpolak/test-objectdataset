object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 325
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    574
    325)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 39
    Width = 558
    Height = 278
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      680600009619E0BD010000001800000005001200000003000000CA00044E616D
      6501004A0010000100055749445448020002003000074361706974616C01004A
      001000010005574944544802000200300009436F6E74696E656E7401004A0010
      000100055749445448020002003000044172656108000400100000000A506F70
      756C6174696F6E080004001000000003000D44454641554C545F4F5244455204
      00820001000000010000000B5052494D4152595F4B4559040082000100000001
      000000044C4349440200010009080000001241007200670065006E0074006900
      6E006100184200750065006E006F0073002000410069007200650073001A5300
      6F00750074006800200041006D0065007200690063006100000000806B314541
      00000030BECD7E410000000E42006F006C0069007600690061000C4C00610020
      00500061007A001A53006F00750074006800200041006D006500720069006300
      6100000000004FC3304100000000E8D85B410000000C4200720061007A006900
      6C0010420072006100730069006C00690061001A53006F007500740068002000
      41006D006500720069006300610000000080DB3B604100000000D8EDA1410000
      000C430061006E006100640061000C4F00740074006100770061001A4E006F00
      720074006800200041006D0065007200690063006100000000602A0763410000
      0000BA4579410000000A4300680069006C00650010530061006E007400690061
      0067006F001A53006F00750074006800200041006D0065007200690063006100
      000000009E19274100000000502D69410000001043006F006C006F006D006200
      690061000C4200610067006F00740061001A53006F0075007400680020004100
      6D006500720069006300610000000000DB60314100000000A4787F4100000008
      43007500620061000C48006100760061006E0061001A4E006F00720074006800
      200041006D006500720069006300610000000000C0F5FB4000000000C8376441
      0000000E450063007500610064006F0072000A51007500690074006F001A5300
      6F00750074006800200041006D00650072006900630061000000000038CD1B41
      00000000C83764410000001645006C002000530061006C007600610064006F00
      720018530061006E002000530061006C007600610064006F0072001A4E006F00
      720074006800200041006D0065007200690063006100000000004060D4400000
      0000C83754410000000C47007500790061006E00610014470065006F00720067
      00650074006F0077006E001A53006F00750074006800200041006D0065007200
      69006300610000000000C83D0A4100000000006A28410000000E4A0061006D00
      6100690063006100104B0069006E006700730074006F006E001A4E006F007200
      74006800200041006D0065007200690063006100000000000050C64000000000
      D01243410000000C4D0065007800690063006F00164D0065007800690063006F
      00200043006900740079001A4E006F00720074006800200041006D0065007200
      690063006100000000004C043E4100000000B71F9541000000124E0069006300
      6100720061006700750061000E4D0061006E0061006700750061001A4E006F00
      720074006800200041006D006500720069006300610000000000C0F700410000
      000030C14D410000001050006100720061006700750061007900104100730075
      006E00630069006F006E001A53006F00750074006800200041006D0065007200
      69006300610000000000C0D0184100000000C8C6514100000008500065007200
      7500084C0069006D0061001A53006F00750074006800200041006D0065007200
      690063006100000000005F9C334100000000709974410000003055006E006900
      740065006400200053007400610074006500730020006F006600200041006D00
      6500720069006300610014570061007300680069006E00670074006F006E001A
      4E006F00720074006800200041006D006500720069006300610000000040D7DB
      614100000000FBB4AD410000000E5500720075006700750061007900144D006F
      006E007400650076006900640065006F001A53006F0075007400680020004100
      6D006500720069006300610000000000608005410000000048E7464100000012
      560065006E0065007A00750065006C0061000E43006100720061006300610073
      001A53006F00750074006800200041006D006500720069006300610000000000
      5ED52B410000000092C97241}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 144
    Top = 8
    object ClientDataSet1Name: TWideStringField
      FieldName = 'Name'
      Size = 24
    end
    object ClientDataSet1Capital: TWideStringField
      FieldName = 'Capital'
      Size = 24
    end
    object ClientDataSet1Continent: TWideStringField
      FieldName = 'Continent'
      Size = 24
    end
    object ClientDataSet1Area: TFloatField
      FieldName = 'Area'
    end
    object ClientDataSet1Population: TFloatField
      FieldName = 'Population'
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 240
    Top = 8
  end
end
