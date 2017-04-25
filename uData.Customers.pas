unit uData.Customers;

interface

uses DB, SysUtils, uObjectDataSet;

type
  TCountries = class(TObjectDataSet)
  private
    FTag: integer;
  protected
  public
    FFieldName: TWideStringField;
    FFieldCapital: TWideStringField;
    FFieldContinent: TWideStringField;
    FFieldArea: TFloatField;
    FFieldPopulation: TFloatField;
    procedure AfterScroll; overload;
    procedure AfterScroll(aDataSet: TDataSet); overload;
    property Tag: integer read FTag;
  end;

implementation

procedure TCountries.AfterScroll(aDataSet: TDataSet);
begin
  if FFieldContinent.Value='North America' then
    inc(FTag);
end;

procedure TCountries.AfterScroll;
begin
  raise EAbstractError.Create('Tej metody nie mo¿e podczepiæ TObjectDataSet');
end;

end.
