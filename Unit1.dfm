object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 452
  ClientWidth = 620
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
    620
    452)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 39
    Width = 604
    Height = 74
    Anchors = [akLeft, akTop, akRight]
    Caption = 'GroupBox1'
    TabOrder = 1
  end
  object DrawGrid1: TDrawGrid
    Left = 8
    Top = 119
    Width = 604
    Height = 325
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    OnDrawCell = DrawGrid1DrawCell
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
    Active = True
    Aggregates = <>
    Params = <>
    Left = 144
    Top = 8
    Data = {
      DE0600009619E0BD010000001800000005001400000003000000CA00044E616D
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
      0000C83754410000000E4700650072006D0061006E0079000C4200650072006C
      0069006E000C4500750072006F007000650000000000C0CC1541000000509B97
      93410000000C47007500790061006E00610014470065006F0072006700650074
      006F0077006E001A53006F00750074006800200041006D006500720069006300
      610000000000C83D0A4100000000006A28410000000E4A0061006D0061006900
      63006100104B0069006E006700730074006F006E001A4E006F00720074006800
      200041006D0065007200690063006100000000000050C64000000000D0124341
      0000000C4D0065007800690063006F00164D0065007800690063006F00200043
      006900740079001A4E006F00720074006800200041006D006500720069006300
      6100000000004C043E4100000000B71F9541000000124E006900630061007200
      61006700750061000E4D0061006E0061006700750061001A4E006F0072007400
      6800200041006D006500720069006300610000000000C0F700410000000030C1
      4D410000001050006100720061006700750061007900104100730075006E0063
      0069006F006E001A53006F00750074006800200041006D006500720069006300
      610000000000C0D0184100000000C8C65141000000085000650072007500084C
      0069006D0061001A53006F00750074006800200041006D006500720069006300
      6100000000005F9C334100000000709974410000000C50006F006C0061006E00
      64000C5700610072007300610077000C4500750072006F007000650000000000
      9C15134100000080295682410000003055006E00690074006500640020005300
      7400610074006500730020006F006600200041006D0065007200690063006100
      14570061007300680069006E00670074006F006E001A4E006F00720074006800
      200041006D006500720069006300610000000040D7DB614100000000FBB4AD41
      0000000E5500720075006700750061007900144D006F006E0074006500760069
      00640065006F001A53006F00750074006800200041006D006500720069006300
      610000000000608005410000000048E7464100000012560065006E0065007A00
      750065006C0061000E43006100720061006300610073001A53006F0075007400
      6800200041006D0065007200690063006100000000005ED52B410000000092C9
      7241}
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
    Left = 368
    Top = 8
  end
  object ClientDataSet2: TClientDataSet
    Active = True
    Aggregates = <>
    IndexFieldNames = 'Country'
    MasterFields = 'Name'
    MasterSource = DataSource1
    PacketRecords = 0
    Params = <>
    Left = 240
    Top = 8
    Data = {
      DF3600009619E0BD0100000018000000040025010000030000006D00024C5004
      00010010000000044E616D6501004A0010000100055749445448020002003C00
      07436F756E74727901004A0010000100055749445448020002003C000A506F70
      756C6174696F6E04000100100000000000000025010000104D0061006E006E00
      6800650069006D000E4700650072006D0061006E007900B4AB04000000240100
      000842006F006E006E000E4700650072006D0061006E0079002CB10400000023
      010000124200690065006C006500660065006C0064000E4700650072006D0061
      006E00790084E504000000220100001257007500700070006500720074006100
      6C000E4700650072006D0061006E007900589305000000210100000C42006F00
      6300680075006D000E4700650072006D0061006E00790004EC05000000200100
      000E4400720065007300640065006E000E4700650072006D0061006E007900D4
      38070000001F0100000E4C006500690070007A00690067000E4700650072006D
      0061006E007900D46A070000001E010000124E007500720065006D0062006500
      720067000E4700650072006D0061006E0079002C6D070000001D010000104400
      75006900730062007500720067000E4700650072006D0061006E00790078D507
      0000001C0100000E480061006E006F007600650072000E4700650072006D0061
      006E007900CCE0070000001B0100000C4200720065006D0065006E000E470065
      0072006D0061006E0079001C0E080000001A010000144400FC00730073006500
      6C0064006F00720066000E4700650072006D0061006E00790044AE0800000019
      010000125300740075007400740067006100720074000E4700650072006D0061
      006E007900ECDD08000000180100001044006F00720074006D0075006E006400
      0E4700650072006D0061006E00790050F708000000170100000A450073007300
      65006E000E4700650072006D0061006E00790000FC0800000016010000124600
      720061006E006B0066007500720074000E4700650072006D0061006E00790080
      FC0A000000150100000E43006F006C006F0067006E0065000E4700650072006D
      0061006E007900B4BA0E000000140100000C4D0075006E006900630068000E47
      00650072006D0061006E007900781612000000130100000E480061006D006200
      7500720067000E4700650072006D0061006E00790054BA19000000120100000C
      4200650072006C0069006E000E4700650072006D0061006E007900F8F8310000
      00110100000C4B00690065006C00630065000C50006F006C0061006E0064004E
      2403000000100100000A54006F007200750044010C50006F006C0061006E0064
      001B27030000000F0100001253006F0073006E006F0077006900650063000C50
      006F006C0061006E0064007A65030000000E0100000A5200610064006F006D00
      0C50006F006C0061006E006400596E030000000D0100001643007A0019017300
      74006F00630068006F00770061000C50006F006C0061006E0064007CB2030000
      000C0100000C4700640079006E00690061000C50006F006C0061006E00640082
      D1030000000B0100001242006900610042017900730074006F006B000C50006F
      006C0061006E006400FF7C040000000A010000104B00610074006F0077006900
      630065000C50006F006C0061006E006400049004000000090100000C4C007500
      62006C0069006E000C50006F006C0061006E0064000933050000000801000012
      42007900640067006F0073007A0063007A000C50006F006C0061006E006400AE
      6A05000000070100001053007A0063007A006500630069006E000C50006F006C
      0061006E006400A52F06000000060100000C470064006100440173006B000C50
      006F006C0061006E006400941007000000050100000C50006F007A006E006100
      44010C50006F006C0061006E006400794308000000040100000E570072006F00
      63004201610077000C50006F006C0061006E00640093B8090000000301000008
      4101F30064007A010C50006F006C0061006E00640040A90A000000020100000C
      4B00720061006B00F30077000C50006F006C0061006E00640050A20B00000001
      0100000C5700610072007300610077000C50006F006C0061006E006400B4AF1A
      000000000100001045006C002000560069006700ED00610012560065006E0065
      007A00750065006C00610091D103000000FF0000000C430075006D0061006E00
      E10012560065006E0065007A00750065006C0061001DEF03000000FE00000028
      530061006E007400610020005400650072006500730061002000640065006C00
      200054007500790012560065006E0065007A00750065006C00610088FB030000
      00FD000000124C006100200047007500610069007200610012560065006E0065
      007A00750065006C006100C82104000000FC00000014500075006E0074006F00
      2000460069006A006F0012560065006E0065007A00750065006C006100193A04
      000000FB000000144C006F007300200054006500710075006500730012560065
      006E0065007A00750065006C006100804304000000FA0000000E420061007200
      69006E006100730012560065006E0065007A00750065006C0061009156040000
      00F90000001C430069007500640061006400200042006F006C00ED0076006100
      720012560065006E0065007A00750065006C006100627104000000F80000000C
      4D00E900720069006400610012560065006E0065007A00750065006C006100E2
      D704000000F70000000E5400750072006D00650072006F0012560065006E0065
      007A00750065006C006100AA4205000000F60000000E4D006100740075007200
      ED006E0012560065006E0065007A00750065006C006100574506000000F50000
      0012420061007200630065006C006F006E00610012560065006E0065007A0075
      0065006C006100B0710B000000F40000001C4300690075006400610064002000
      470075006100790061006E00610012560065006E0065007A00750065006C0061
      00AE710E000000F30000000E4D0061007200610063006100790012560065006E
      0065007A00750065006C006100F0DD13000000F2000000184200610072007100
      75006900730069006D00650074006F0012560065006E0065007A00750065006C
      006100006A18000000F100000010560061006C0065006E006300690061001256
      0065006E0065007A00750065006C006100727227000000F0000000124D006100
      7200610063006100690062006F0012560065006E0065007A00750065006C0061
      0056883F000000EF0000000E430061007200610063006100730012560065006E
      0065007A00750065006C006100401555000000EE000000244300690075006400
      6100640020006400650020006C006100200043006F007300740061000E550072
      0075006700750061007900C87301000000ED0000000A530061006C0074006F00
      0E55007200750067007500610079005C9601000000EC000000144D006F006E00
      7400650076006900640065006F000E5500720075006700750061007900C42014
      000000EB0000000E410074006C0061006E00740061003055006E006900740065
      006400200053007400610074006500730020006F006600200041006D00650072
      00690063006100061407000000EA000000084D006500730061003055006E0069
      00740065006400200053007400610074006500730020006F006600200041006D
      0065007200690063006100113307000000E9000000144C006F006E0067002000
      420065006100630068003055006E006900740065006400200053007400610074
      006500730020006F006600200041006D00650072006900630061001C3C070000
      00E8000000164B0061006E00730061007300200043006900740079003055006E
      006900740065006400200053007400610074006500730020006F006600200041
      006D0065007200690063006100F24007000000E7000000145300610063007200
      61006D0065006E0074006F003055006E00690074006500640020005300740061
      0074006500730020006F006600200041006D0065007200690063006100D87C07
      000000E60000000C46007200650073006E006F003055006E0069007400650064
      00200053007400610074006500730020006F006600200041006D006500720069
      006300610074EF07000000E50000000C54007500630073006F006E003055006E
      006900740065006400200053007400610074006500730020006F006600200041
      006D0065007200690063006100B91C08000000E40000001641006C0062007500
      71007500650072007100750065003055006E0069007400650064002000530074
      00610074006500730020006F006600200041006D006500720069006300610011
      8808000000E3000000124D0069006C007700610075006B00650065003055006E
      006900740065006400200053007400610074006500730020006F006600200041
      006D00650072006900630061005B2809000000E2000000144C006F0075006900
      7300760069006C006C0065003055006E00690074006500640020005300740061
      0074006500730020006F006600200041006D0065007200690063006100C66309
      000000E100000012420061006C00740069006D006F00720065003055006E0069
      00740065006400200053007400610074006500730020006F006600200041006D
      0065007200690063006100197D09000000E0000000124C006100730020005600
      65006700610073003055006E0069007400650064002000530074006100740065
      00730020006F006600200041006D0065007200690063006100838409000000DF
      0000001A4F006B006C00610068006F006D006100200043006900740079003055
      006E006900740065006400200053007400610074006500730020006F00660020
      0041006D006500720069006300610032A209000000DE0000001050006F007200
      74006C0061006E0064003055006E006900740065006400200053007400610074
      006500730020006F006600200041006D0065007200690063006100F5A5090000
      00DD000000124E00610073006800760069006C006C0065003055006E00690074
      0065006400200053007400610074006500730020006F006600200041006D0065
      00720069006300610012FD09000000DC0000000E4D0065006D00700068006900
      73003055006E006900740065006400200053007400610074006500730020006F
      006600200041006D00650072006900630061009A010A000000DB0000000C4200
      6F00730074006F006E003055006E006900740065006400200053007400610074
      006500730020006F006600200041006D0065007200690063006100012E0A0000
      00DA00000014570061007300680069006E00670074006F006E003055006E0069
      00740065006400200053007400610074006500730020006F006600200041006D
      0065007200690063006100E4410A000000D90000000E44006500740072006F00
      690074003055006E006900740065006400200053007400610074006500730020
      006F006600200041006D0065007200690063006100FC540A000000D80000000E
      45006C0020005000610073006F003055006E0069007400650064002000530074
      00610074006500730020006F006600200041006D0065007200690063006100A4
      640A000000D70000000C440065006E007600650072003055006E006900740065
      006400200053007400610074006500730020006F006600200041006D00650072
      00690063006100316A0A000000D60000000E530065006100740074006C006500
      3055006E006900740065006400200053007400610074006500730020006F0066
      00200041006D0065007200690063006100A3710A000000D50000001243006800
      610072006C006F007400740065003055006E0069007400650064002000530074
      00610074006500730020006F006600200041006D0065007200690063006100D9
      9E0C000000D40000001446006F0072007400200057006F007200740068003055
      006E006900740065006400200053007400610074006500730020006F00660020
      0041006D006500720069006300610027B70C000000D30000001043006F006C00
      75006D006200750073003055006E006900740065006400200053007400610074
      006500730020006F006600200041006D0065007200690063006100BAF80C0000
      00D20000001849006E006400690061006E00610070006F006C00690073003055
      006E006900740065006400200053007400610074006500730020006F00660020
      0041006D0065007200690063006100B5040D000000D10000001A530061006E00
      20004600720061006E0063006900730063006F003055006E0069007400650064
      00200053007400610074006500730020006F006600200041006D006500720069
      006300610030320D000000D0000000184A00610063006B0073006F006E007600
      69006C006C0065003055006E0069007400650064002000530074006100740065
      00730020006F006600200041006D0065007200690063006100BF3E0D000000CF
      0000000C410075007300740069006E003055006E006900740065006400200053
      007400610074006500730020006F006600200041006D00650072006900630061
      00F6370E000000CE00000010530061006E0020004A006F00730065003055006E
      006900740065006400200053007400610074006500730020006F006600200041
      006D00650072006900630061005CAB0F000000CD0000000C440061006C006C00
      610073003055006E006900740065006400200053007400610074006500730020
      006F006600200041006D00650072006900630061007CD613000000CC00000012
      530061006E00200044006900650067006F003055006E00690074006500640020
      0053007400610074006500730020006F006600200041006D0065007200690063
      006100F04815000000CB00000016530061006E00200041006E0074006F006E00
      69006F003055006E006900740065006400200053007400610074006500730020
      006F006600200041006D0065007200690063006100956D16000000CA0000000E
      500068006F0065006E00690078003055006E0069007400650064002000530074
      00610074006500730020006F006600200041006D006500720069006300610091
      D917000000C9000000185000680069006C006100640065006C00700068006900
      61003055006E006900740065006400200053007400610074006500730020006F
      006600200041006D0065007200690063006100D2EA17000000C80000000E4800
      6F007500730074006F006E003055006E00690074006500640020005300740061
      0074006500730020006F006600200041006D0065007200690063006100A00923
      000000C70000000E4300680069006300610067006F003055006E006900740065
      006400200053007400610074006500730020006F006600200041006D00650072
      00690063006100228329000000C6000000164C006F007300200041006E006700
      65006C00650073003055006E0069007400650064002000530074006100740065
      00730020006F006600200041006D00650072006900630061002B9B3C000000C5
      000000104E0065007700200059006F0072006B003055006E0069007400650064
      00200053007400610074006500730020006F006600200041006D006500720069
      0063006100057882000000C40000000E530075006C006C0061006E0061000850
      00650072007500561203000000C30000001050007500630061006C006C007000
      6100085000650072007500C33A03000000C200000012430061006A0061006D00
      6100720063006100085000650072007500EF7203000000C10000000649006300
      6100085000650072007500A6BA03000000C00000000E4A0075006C0069006100
      63006100085000650072007500DA2D04000000BF0000000A5400610063006E00
      6100085000650072007500FF7804000000BE000000104800750061006E006300
      610079006F00085000650072007500B59005000000BD00000010430068006900
      6D0062006F007400650008500065007200750044A905000000BC0000000A4300
      7500730063006F00085000650072007500D28406000000BB0000000A50006900
      750072006100085000650072007500D8A806000000BA0000000E490071007500
      690074006F00730008500065007200750080AC06000000B90000001043006800
      690063006C00610079006F00085000650072007500782909000000B800000010
      5400720075006A0069006C006C006F000850006500720075003E330C000000B7
      0000001041007200650071007500690070006100085000650072007500E7430D
      000000B6000000084C0069006D006100085000650072007500978D96000000B5
      00000016530061006E0020004C006F00720065006E007A006F00105000610072
      0061006700750061007900441E03000000B40000001E43006900750064006100
      64002000640065006C0020004500730074006500105000610072006100670075
      0061007900426403000000B3000000104100730075006E0063006900F3006E00
      105000610072006100670075006100790070D007000000B2000000084C006500
      F3006E00124E0069006300610072006100670075006100E3A702000000B10000
      000E4D0061006E006100670075006100124E0069006300610072006100670075
      006100114E0E000000B00000000E44007500720061006E0067006F000C4D0065
      007800690063006F0035EA07000000AF0000002054007500780074006C006100
      20004700750074006900E9007200720065007A000C4D0065007800690063006F
      000E3208000000AE0000001654006C0061007100750065007000610071007500
      65000C4D0065007800690063006F00C6C908000000AD0000000E520065007900
      6E006F00730061000C4D0065007800690063006F009AFE08000000AC0000000E
      4D006F00720065006C00690061000C4D0065007800690063006F00071E090000
      00AB0000000E54006F00720072006500F3006E000C4D0065007800690063006F
      00444A09000000AA000000184300680069006D0061006C006800750061006300
      E1006E000C4D0065007800690063006F001F5809000000A90000001251007500
      65007200E9007400610072006F000C4D0065007800690063006F003F8F090000
      00A80000000C430061006E006300FA006E000C4D0065007800690063006F0052
      9609000000A70000001854006C0061006C006E006500700061006E0074006C00
      61000C4D0065007800690063006F0062F809000000A600000010410063006100
      700075006C0063006F000C4D0065007800690063006F00C7460A000000A50000
      0012470075006100640061006C007500700065000C4D0065007800690063006F
      0050470A000000A4000000104D00650078006900630061006C0069000C4D0065
      007800690063006F006F860A000000A300000010530061006C00740069006C00
      6C006F000C4D0065007800690063006F0027D40A000000A20000001448006500
      72006D006F00730069006C006C006F000C4D0065007800690063006F0035E90A
      000000A10000001C41006700750061007300630061006C00690065006E007400
      650073000C4D0065007800690063006F004A050B000000A00000001E53006100
      6E0020004C00750069007300200050006F0074006F007300ED000C4D00650078
      00690063006F0054070B0000009F0000000C4D00E90072006900640061000C4D
      0065007800690063006F008FDD0B0000009E000000124E006100750063006100
      6C00700061006E000C4D0065007800690063006F0093160C0000009D00000012
      4300680069006800750061006800750061000C4D0065007800690063006F0010
      590C0000009C00000010430075006C00690061006300E1006E000C4D00650078
      00690063006F0031D00D0000009B0000001C4E0065007A006100680075006100
      6C006300F30079006F0074006C000C4D0065007800690063006F00C9DA100000
      009A000000124D006F006E007400650072007200650079000C4D006500780069
      0063006F00985311000000990000000E5A00610070006F00700061006E000C4D
      0065007800690063006F00D36E1100000098000000084C006500F3006E000C4D
      0065007800690063006F00B2E712000000970000000E540069006A0075006100
      6E0061000C4D0065007800690063006F00F7D913000000960000000C4A007500
      E100720065007A000C4D0065007800690063006F002C2814000000950000000C
      50007500650062006C0061000C4D0065007800690063006F00CEE11500000094
      00000016470075006100640061006C0061006A006100720061000C4D00650078
      00690063006F008ED01600000093000000104500630061007400650070006500
      63000C4D0065007800690063006F00E7401900000092000000164D0065007800
      690063006F00200043006900740079000C4D0065007800690063006F00880E87
      00000091000000164D006F006E007400650067006F0020004200610079000E4A
      0061006D006100690063006100E8780100000090000000104B0069006E006700
      730074006F006E000E4A0061006D00610069006300610016F8080000008F0000
      0014470065006F0072006700650074006F0077006E000C47007500790061006E
      0061005BCE010000008E000000124D0065006A006900630061006E006F007300
      1645006C002000530061006C007600610064006F007200A63B030000008D0000
      000A410070006F00700061001645006C002000530061006C007600610064006F
      0072008552030000008C00000014530061006E0020004D006900670075006500
      6C001645006C002000530061006C007600610064006F0072002A55030000008B
      0000001253006F0079006100700061006E0067006F001645006C002000530061
      006C007600610064006F0072003F03040000008A00000012530061006E007400
      6100200041006E0061001645006C002000530061006C007600610064006F0072
      00C045040000008900000018530061006E002000530061006C00760061006400
      6F0072001645006C002000530061006C007600610064006F0072003D41080000
      00880000001450006F00720074006F007600690065006A006F000E4500630075
      00610064006F0072005A2703000000870000000A4D0061006E00740061000E45
      0063007500610064006F007200D15103000000860000000A440075007200E100
      6E000E450063007500610064006F007200B78503000000850000000E4D006100
      6300680061006C0061000E450063007500610064006F0072005C870300000084
      0000001A530061006E0074006F00200044006F006D0069006E0067006F000E45
      0063007500610064006F0072001B2204000000830000000C4300750065006E00
      630061000E450063007500610064006F007200C80805000000820000000A5100
      7500690074006F000E450063007500610064006F007200368818000000810000
      0012470075006100790061007100750069006C000E450063007500610064006F
      00720023C5220000008000000016530061006E0074006100200043006C006100
      72006100084300750062006100F423030000007F000000144700750061006E00
      7400E1006E0061006D006F00084300750062006100F12B030000007E0000000E
      48006F006C0067007500ED006E000843007500620061003A3A040000007D0000
      0010430061006D0061006700FC0065007900084300750062006100B5AA040000
      007C00000020530061006E0074006900610067006F0020006400650020004300
      7500620061000843007500620061007B7F060000007B0000000C480061007600
      61006E006100084300750062006100CA95200000007A00000012530069006E00
      630065006C0065006A006F001043006F006C006F006D0062006900610032A003
      000000790000001A46006C006F00720069006400610062006C0061006E006300
      61001043006F006C006F006D00620069006100DBE203000000780000000E5000
      6F00700061007900E1006E001043006F006C006F006D00620069006100E8ED03
      000000770000000E410072006D0065006E00690061001043006F006C006F006D
      00620069006100F2AE04000000760000000E500061006C006D00690072006100
      1043006F006C006F006D00620069006100964105000000750000001842007500
      65006E006100760065006E0074007500720061001043006F006C006F006D0062
      0069006100DE5C05000000740000000A4E0065006900760061001043006F006C
      006F006D006200690061005B62050000007300000014560061006C006C006500
      640075007000610072001043006F006C006F006D006200690061008467050000
      00720000000A420065006C006C006F001043006F006C006F006D006200690061
      0005AD0500000071000000124D0061006E0069007A0061006C00650073001043
      006F006C006F006D0062006900610044CC05000000700000001A560069006C00
      6C00610076006900630065006E00630069006F001043006F006C006F006D0062
      00690061003ECD050000006F000000104D006F006E00740065007200ED006100
      1043006F006C006F006D0062006900610055D2050000006E0000002253006100
      6E0020004A00750061006E00200064006500200050006100730074006F001043
      006F006C006F006D006200690061009AD6050000006D0000000C53006F006100
      6300680061001043006F006C006F006D006200690061005722060000006C0000
      0016530061006E007400610020004D0061007200740061001043006F006C006F
      006D006200690061002656060000006B0000000E500065007200650069007200
      61001043006F006C006F006D00620069006100A2C4060000006A0000000E5300
      6F006C0065006400610064001043006F006C006F006D006200690061001B0C07
      000000690000000C49006200610067007500E9001043006F006C006F006D0062
      0069006100E19A0700000068000000164200750063006100720061006D006100
      6E00670061001043006F006C006F006D00620069006100A0E107000000670000
      000C4300FA0063007500740061001043006F006C006F006D006200690061009C
      F708000000660000001243006100720074006100670065006E0061001043006F
      006C006F006D00620069006100819E0D00000065000000184200610072007200
      61006E007100750069006C006C0061001043006F006C006F006D006200690061
      00F77D110000006400000008430061006C0069001043006F006C006F006D0062
      0069006100E4582000000063000000104D006500640065006C006C00ED006E00
      1043006F006C006F006D006200690061005ECA21000000620000000C42006F00
      67006F007400E1001043006F006C006F006D0062006900610074A17700000061
      00000010520061006E00630061006700750061000A4300680069006C0065007B
      2803000000600000001443006F006E0063006500700063006900F3006E000A43
      00680069006C006500233C030000005F0000001250006500F10061006C006F00
      6C00E9006E000A4300680069006C006500FC4B030000005E0000000C54006500
      6D00750063006F000A4300680069006C0065000E77030000005D000000185300
      61006E0020004200650072006E006100720064006F000A4300680069006C0065
      008CA0030000005C000000144C0061007300200043006F006E00640065007300
      0A4300680069006C00650025D0030000005B00000014560061006C0070006100
      72006100ED0073006F000A4300680069006C0065004B05040000005A00000016
      41006E0074006F0066006100670061007300740061000A4300680069006C0065
      00475A04000000590000001856006900F10061002000640065006C0020004D00
      610072000A4300680069006C006500D3600400000058000000144C0061002000
      46006C006F0072006900640061000A4300680069006C006500FB930500000057
      0000000A4D00610069007000FA000A4300680069006C006500FF100700000056
      000000165000750065006E0074006500200041006C0074006F000A4300680069
      006C0065003B84070000005500000010530061006E0074006900610067006F00
      0A4300680069006C006500680F47000000540000001047006100740069006E00
      6500610075000C430061006E00610064006100850C04000000530000000E5600
      610075006700680061006E000C430061006E006100640061002D660400000052
      0000000E4D00610072006B00680061006D000C430061006E006100640061008D
      9A04000000510000000C4C006F006E0064006F006E000C430061006E00610064
      006100479605000000500000000A4C006100760061006C000C430061006E0061
      00640061009120060000004F0000000C5300750072007200650079000C430061
      006E006100640061001B25070000004E0000000C51007500E900620065006300
      0C430061006E006100640061000EE2070000004D00000010480061006D006900
      6C0074006F006E000C430061006E006100640061000DEF070000004C00000010
      4200720061006D00700074006F006E000C430061006E0061006400610087FE07
      0000004B00000012560061006E0063006F0075007600650072000C430061006E
      006100640061006E35090000004A00000010570069006E006E00690070006500
      67000C430061006E0061006400610041200A00000049000000164D0069007300
      730069007300730061007500670061000C430061006E00610064006100E3E20A
      0000004800000010450064006D006F006E0074006F006E000C430061006E0061
      0064006100A9640C000000470000000C4F00740074006100770061000C430061
      006E00610064006100BF7A0D000000460000000E430061006C00670061007200
      79000C430061006E0061006400610081BC1000000045000000104D006F006E00
      74007200650061006C000C430061006E006100640061006F2B19000000440000
      000E54006F0072006F006E0074006F000C430061006E0061006400610014E727
      0000004300000028410070006100720065006300690064006100200064006500
      200047006F006900E2006E00690061000C4200720061007A0069006C005BCD07
      00000042000000104C006F006E006400720069006E0061000C4200720061007A
      0069006C001B490800000041000000184A00750069007A002000640065002000
      46006F00720061000C4200720061007A0069006C003667080000004000000012
      4A006F0069006E00760069006C006C0065000C4200720061007A0069006C0069
      76080000003F0000000C43007500690061006200E1000C4200720061007A0069
      006C00F8C7080000003E00000020460065006900720061002000640065002000
      530061006E00740061006E0061000C4200720061007A0069006C00A056090000
      003D0000000E410072006100630061006A0075000C4200720061007A0069006C
      009684090000003C0000001053006F0072006F0063006100620061000C420072
      0061007A0069006C0003B9090000003B0000001043006F006E00740061006700
      65006D000C4200720061007A0069006C0094D1090000003A0000001455006200
      650072006C00E2006E006400690061000C4200720061007A0069006C0059FD09
      000000390000001C520069006200650069007200E3006F002000500072006500
      74006F000C4200720061007A0069006C008B0A0A000000380000002E4A006100
      62006F0061007400E3006F00200064006F007300200047007500610072006100
      720061007000650073000C4200720061007A0069006C00EF630A000000370000
      00265300E3006F0020004A006F007300E900200064006F007300200043006100
      6D0070006F0073000C4200720061007A0069006C004C640A000000360000000C
      4F0073006100730063006F000C4200720061007A0069006C0017940A00000035
      00000016530061006E0074006F00200041006E0064007200E9000C4200720061
      007A0069006C001DCC0A00000034000000164A006F00E3006F00200050006500
      730073006F0061000C4200720061007A0069006C00C2E90B0000003300000016
      4E006F007600610020004900670075006100E70075000C4200720061007A0069
      006C00214D0C000000320000002A5300E3006F0020004200650072006E006100
      720064006F00200064006F002000430061006D0070006F000C4200720061007A
      0069006C00E1610C00000031000000105400650072006500730069006E006100
      0C4200720061007A0069006C0098D30C0000003000000018430061006D007000
      6F0020004700720061006E00640065000C4200720061007A0069006C0076060D
      0000002F0000000A4E006100740061006C000C4200720061007A0069006C005C
      270D0000002E0000001E44007500710075006500200064006500200043006100
      78006900610073000C4200720061007A0069006C0042670D0000002D0000000C
      4D006100630065006900F3000C4200720061007A0069006C0007570F0000002C
      000000165300E3006F00200047006F006E00E70061006C006F000C4200720061
      007A0069006C00DFBE0F0000002B000000105300E3006F0020004C007500ED00
      73000C4200720061007A0069006C00053D100000002A00000010430061006D00
      700069006E00610073000C4200720061007A0069006C00399E11000000290000
      0012470075006100720075006C0068006F0073000C4200720061007A0069006C
      00C50514000000280000000E47006F006900E2006E00690061000C4200720061
      007A0069006C000C8D15000000270000000A420065006C00E9006D000C420072
      0061007A0069006C000CDD15000000260000001850006F00720074006F002000
      41006C0065006700720065000C4200720061007A0069006C00E2771600000025
      0000000C5200650063006900660065000C4200720061007A0069006C00288B18
      0000002400000010430075007200690074006900620061000C4200720061007A
      0069006C00E0721C000000230000000C4D0061006E006100750073000C420072
      0061007A0069006C00CDD31E000000220000001C420065006C006F0020004800
      6F00720069007A006F006E00740065000C4200720061007A0069006C00E50226
      000000210000001246006F007200740061006C0065007A0061000C4200720061
      007A0069006C00783E2700000020000000104200720061007300ED006C006900
      61000C4200720061007A0069006C0014862B0000001F00000010530061006C00
      7600610064006F0072000C4200720061007A0069006C008F4B2C0000001E0000
      001C520069006F0020006400650020004A0061006E006500690072006F000C42
      00720061007A0069006C00B279620000001D000000125300E3006F0020005000
      610075006C006F000C4200720061007A0069006C005584B50000001C0000000C
      50006F0074006F007300ED000E42006F006C0069007600690061007DAB020000
      001B0000000C54006100720069006A0061000E42006F006C0069007600690061
      0048BD020000001A0000000A530075006300720065000E42006F006C00690076
      0069006100A89F03000000190000000A4F007200750072006F000E42006F006C
      006900760069006100EB0904000000180000001443006F006300680061006200
      61006D00620061000E42006F006C0069007600690061003B9F09000000170000
      000C4C0061002000500061007A000E42006F006C006900760069006100C08D0B
      000000160000000E45006C00200041006C0074006F000E42006F006C00690076
      00690061008ADA0C0000001500000014530061006E0074006100200043007200
      75007A000E42006F006C0069007600690061007EFE1500000014000000244300
      6F006D006F0064006F0072006F00200052006900760061006400610076006900
      61001241007200670065006E00740069006E00610067C902000000130000000A
      4C0061006E00FA0073001241007200670065006E00740069006E006100B83C03
      000000120000000E5100750069006C006D00650073001241007200670065006E
      00740069006E0061009A8503000000110000002A530061006E00200053006100
      6C007600610064006F00720020006400650020004A0075006A00750079001241
      007200670065006E00740069006E006100210C04000000100000001842006100
      6800ED006100200042006C0061006E00630061001241007200670065006E0074
      0069006E006100B8BB040000000F000000144100760065006C006C0061006E00
      6500640061001241007200670065006E00740069006E0061001405050000000E
      0000001443006F0072007200690065006E007400650073001241007200670065
      006E00740069006E0061008039050000000D00000026530061006E0074006900
      610067006F002000640065006C002000450073007400650072006F0012410072
      00670065006E00740069006E006100900F060000000C0000000E4E0065007500
      71007500E9006E001241007200670065006E00740069006E006100D81C060000
      000B0000001652006500730069007300740065006E0063006900610012410072
      00670065006E00740069006E006100C0E8060000000A00000010530061006E00
      20004A00750061006E001241007200670065006E00740069006E00610097D107
      0000000900000010530061006E00740061002000460065001241007200670065
      006E00740069006E0061000C0008000000080000000A530061006C0074006100
      1241007200670065006E00740069006E006100707D08000000070000001A4D00
      610072002000640065006C00200050006C006100740061001241007200670065
      006E00740069006E006100C8D70A000000060000000E54007500630075006D00
      E1006E001241007200670065006E00740069006E006100BCC70D000000050000
      00104C006100200050006C006100740061001241007200670065006E00740069
      006E006100689D0E000000040000000E4D0065006E0064006F007A0061001241
      007200670065006E00740069006E00610070EC10000000030000000E52006F00
      73006100720069006F001241007200670065006E00740069006E006100223814
      000000020000000E4300F300720064006F00620061001241007200670065006E
      00740069006E0061009B9D1800000001000000184200750065006E006F007300
      2000410069007200650073001241007200670065006E00740069006E0061004A
      5C2A00}
    object ClientDataSet2LP: TIntegerField
      FieldName = 'LP'
    end
    object ClientDataSet2Name: TWideStringField
      FieldName = 'Name'
      Size = 30
    end
    object ClientDataSet2Country: TWideStringField
      FieldName = 'Country'
      Size = 30
    end
    object ClientDataSet2Population: TIntegerField
      FieldName = 'Population'
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 144
    Top = 56
  end
  object DataSource2: TDataSource
    DataSet = ClientDataSet2
    Left = 240
    Top = 56
  end
end
