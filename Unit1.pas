unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, DB, DBClient, DBGrids, ExtCtrls,
  uObjectDataSet, uData.Customers, uData.Cities;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Name: TWideStringField;
    ClientDataSet1Capital: TWideStringField;
    ClientDataSet1Continent: TWideStringField;
    ClientDataSet1Area: TFloatField;
    ClientDataSet1Population: TFloatField;
    Timer1: TTimer;
    ClientDataSet2: TClientDataSet;
    ClientDataSet2LP: TIntegerField;
    ClientDataSet2Name: TWideStringField;
    ClientDataSet2Country: TWideStringField;
    ClientDataSet2Population: TIntegerField;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    GroupBox1: TGroupBox;
    DrawGrid1: TDrawGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    Countries: TCountries;
    Cities: TCites;
    MaxPopulation: Double;
    MaxArea: Double;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  sCapitalInfo: string;
  wd: Integer;
  maxwd: Integer;
begin
  if gdFocused in State then
    DrawGrid1.Canvas.DrawFocusRect(Rect);
  if (ARow > 0) and (ACol = 1) then
  begin
    while Countries.RecNo > ARow do
      Countries.Prior;
    while Countries.RecNo < ARow do
      Countries.Next;
    DrawGrid1.Canvas.Font.Size := 17;
    DrawGrid1.Canvas.TextOut(Rect.Left + 5, Rect.Top + 3,
      Countries.FFieldName.value);
    DrawGrid1.Canvas.Font.Size := 9;
    DrawGrid1.Canvas.TextOut(Rect.Left + 5, Rect.Top + 31,
      Countries.FFieldContinent.value);
    sCapitalInfo := Countries.FFieldCapital.value;
    wd := DrawGrid1.Canvas.TextWidth(sCapitalInfo);
    DrawGrid1.Canvas.Font.Color := RGB(64, 0, 0);
    DrawGrid1.Canvas.TextOut(Rect.Right - 10 - wd, Rect.Top + 31, sCapitalInfo);
    DrawGrid1.Canvas.Font.Color := DrawGrid1.Font.Color;
  end;
  if (ARow > 0) and (ACol = 2) then
  begin
    DrawGrid1.Canvas.Font.Size := 9;
    DrawGrid1.Canvas.TextOut(Rect.Left + 5, Rect.Top + 5,
      FormatFloat('###,###,###,###', Countries.FFieldArea.value) + ' km2');
    DrawGrid1.Canvas.TextOut(Rect.Left + 5, Rect.Top + 25,
      FormatFloat('###,###,###,###', Countries.FFieldPopulation.value));
    maxwd := Rect.Right - Rect.Left - 10;
    wd := round(maxwd * Countries.FFieldArea.value / MaxArea);
    DrawGrid1.Canvas.Brush.Style := bsSolid;
    DrawGrid1.Canvas.Pen.Style := psClear;
    DrawGrid1.Canvas.Brush.Color := RGB(128, 255, 128);
    DrawGrid1.Canvas.Rectangle(Rect.Left + 5, Rect.Top + 20, Rect.Left + 5 + wd,
      Rect.Top + 24);
    wd := round(maxwd * Countries.FFieldPopulation.value / MaxPopulation);
    DrawGrid1.Canvas.Brush.Color := RGB(228, 228, 0);
    DrawGrid1.Canvas.Rectangle(Rect.Left + 5, Rect.Top + 40, Rect.Left + 5 + wd,
      Rect.Top + 44);
    DrawGrid1.Canvas.Pen.Style := psSolid;
    DrawGrid1.Canvas.Brush.Style := bsClear;
  end;
  (*
  Cities.ForEachMute(
    procedure()
    begin
      s := s + Cities.FFieldName.value + ', ';
      if Cities.FFieldPopulation.value < 600000 then
        inc(TopCities);
    end);
  *)
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Countries.Free;
  Cities.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Countries := TCountries.Create(ClientDataSet1);
  Cities := TCites.Create(ClientDataSet2);
  // ----
  DrawGrid1.DefaultRowHeight := 48;
  DrawGrid1.ColCount := 8;
  DrawGrid1.RowHeights[0] := 24;
  DrawGrid1.ColWidths[0] := 32;
  DrawGrid1.ColWidths[1] := 260;
  DrawGrid1.ColWidths[2] := 120;
  DrawGrid1.ColWidths[3] := 90;
  DrawGrid1.RowCount := Countries.RecordCount + 1;
  MaxPopulation := 0;
  MaxArea := 0;
  Countries.ForEachMute(
    procedure()
    begin
      if MaxArea < Countries.FFieldArea.value then
        MaxArea := Countries.FFieldArea.value;
      if MaxPopulation < Countries.FFieldPopulation.value then
        MaxPopulation := Countries.FFieldPopulation.value;
    end);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := IntToStr(Countries.Tag);
end;

end.
