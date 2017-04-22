unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, DB, DBClient, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.StorageBin,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.DBGrids;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Name: TWideStringField;
    ClientDataSet1Capital: TWideStringField;
    ClientDataSet1Continent: TWideStringField;
    ClientDataSet1Area: TFloatField;
    ClientDataSet1Population: TFloatField;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses uObjectDataSet;

type
  TCountries = class(TObjectDataSet)
  private
    FHighlited: Boolean;
  protected
  public
    FFieldName: TWideStringField;
    FFieldCapital: TWideStringField;
    FFieldContinent: TWideStringField;
    FFieldArea: TFloatField;
    FFieldPopulation: TFloatField;
    procedure AfterScroll(aDataSet: TDataSet);
  end;

procedure TCountries.AfterScroll(aDataSet: TDataSet);
begin
  FHighlited := (FFieldContinent.Value='Europe');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Countries: TCountries;
  GridRow: Integer;
begin
  Countries := TCountries.Create(ClientDataSet1);
  StringGrid1.ColCount := 6;
  StringGrid1.ColWidths[0] := 32;
  StringGrid1.ColWidths[1] := 100;
  StringGrid1.ColWidths[2] := 90;
  StringGrid1.ColWidths[3] := 90;
  StringGrid1.RowCount := Countries.FDataSet.RecordCount + 1;
  StringGrid1.Rows[0].CommaText := ',' + Countries.FFieldName.DisplayName +
    ',' + Countries.FFieldCapital.DisplayName + ',' +
    Countries.FFieldContinent.DisplayName + ',' +
    Countries.FFieldArea.DisplayName + ',' +
    Countries.FFieldPopulation.DisplayName;
  Countries.ForEach(
    procedure()
    begin
      GridRow := Countries.FDataSet.RecNo;
      StringGrid1.Cells[1, GridRow] := Countries.FFieldName.value;
      StringGrid1.Cells[2, GridRow] := Countries.FFieldCapital.value;
      StringGrid1.Cells[3, GridRow] := Countries.FFieldContinent.value;
      StringGrid1.Cells[4, GridRow] := Countries.FFieldArea.AsString;
      StringGrid1.Cells[5, GridRow] := Countries.FFieldPopulation.AsString;
    end);
  Countries.Free;
end;

end.
