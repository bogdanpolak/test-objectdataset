unit uData.Cities;

interface

uses DB, SysUtils, uObjectDataSet;

type
  TCites = class(TObjectDataSet)
  private
  protected
  public
    FFieldLP: TIntegerField;
    FFieldName: TWideStringField;
    FFieldCountry: TWideStringField;
    FFieldPopulation: TIntegerField;
  end;

implementation

end.
