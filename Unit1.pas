unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, DB, DBClient;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Name: TStringField;
    ClientDataSet1Capital: TStringField;
    ClientDataSet1Continent: TStringField;
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

uses
  Rtti;

type
  TMyModule = class
  public
    FFieldName: TStringField;
    FFieldCapital: TStringField;
    FFieldContinent: TStringField;
    FFieldArea: TFloatField;
    FFieldPopulation: TFloatField;
    FDataSet: TDataSet;
    constructor Create(DataSet: TDataSet);
  protected
    procedure AfterDelete(aDataSet: TDataSet);
    procedure AfterOpen(aDataSet: TDataSet);
    procedure AfterPost(aDataSet: TDataSet);
    procedure AfterRefresh(aDataSet: TDataSet);
    procedure AfterScroll(aDataSet: TDataSet);
    procedure BeforeDelete(aDataSet: TDataSet);
    procedure BeforeEdit(aDataSet: TDataSet);
    procedure BeforeInsert(aDataSet: TDataSet);
    procedure BeforePost(aDataSet: TDataSet);
    procedure OnCalcFields(aDataSet: TDataSet);
    procedure OnEditError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure OnNewRecord(aDataSet: TDataSet);
  private
    procedure HookEvents;
    procedure HookScrollEvents;
  end;
{$REGION 'Events to methods'}

procedure TMyModule.AfterDelete(aDataSet: TDataSet);
begin
  // AfterDelete;
end;

procedure TMyModule.AfterOpen(aDataSet: TDataSet);
begin
  // AfterOpen;
end;

procedure TMyModule.AfterPost(aDataSet: TDataSet);
begin
  // AfterPost;
end;

procedure TMyModule.AfterScroll(aDataSet: TDataSet);
begin
  // AfterScroll;
end;

procedure TMyModule.AfterRefresh(aDataSet: TDataSet);
begin
  // AfterRefresh;
end;

procedure TMyModule.BeforeDelete(aDataSet: TDataSet);
begin
  // BeforeDelete;
end;

procedure TMyModule.BeforeEdit(aDataSet: TDataSet);
begin
  // BeforeEdit;
end;

procedure TMyModule.BeforeInsert(aDataSet: TDataSet);
begin
  // BeforeInsert;
end;

procedure TMyModule.BeforePost(aDataSet: TDataSet);
begin
  // BeforePost;
end;

constructor TMyModule.Create(DataSet: TDataSet);
begin
  FDataSet := DataSet;
end;

procedure TMyModule.OnCalcFields(aDataSet: TDataSet);
begin
  // OnCalcFields;
end;

procedure TMyModule.OnNewRecord(aDataSet: TDataSet);
begin
  // OnNewRecord;
end;

procedure TMyModule.OnEditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  // OnEditError(E, Action);
end;
{$ENDREGION} procedure TMyModule.HookEvents;
begin
  Assert(not Assigned(FDataSet.BeforePost),
    'BeforePost is set on ' + FDataSet.name);
  FDataSet.BeforePost := BeforePost;

  Assert(not Assigned(FDataSet.BeforeDelete),
    'BeforeDelete is set on ' + FDataSet.name);
  FDataSet.BeforeDelete := BeforeDelete;

  Assert(not Assigned(FDataSet.BeforeEdit),
    'BeforeEdit is set on ' + FDataSet.name);
  FDataSet.BeforeEdit := BeforeEdit;

  Assert(not Assigned(FDataSet.BeforeInsert),
    'BeforeInsert is set on ' + FDataSet.name);
  FDataSet.BeforeInsert := BeforeInsert;

  Assert(not Assigned(FDataSet.AfterOpen),
    'AfterOpen is set on ' + FDataSet.name);
  FDataSet.AfterOpen := AfterOpen;

  Assert(not Assigned(FDataSet.AfterPost),
    'AfterPost is set on ' + FDataSet.name);
  FDataSet.AfterPost := AfterPost;

  Assert(not Assigned(FDataSet.AfterDelete),
    'AfterDelete is set on ' + FDataSet.name);
  FDataSet.AfterDelete := AfterDelete;

  Assert(not Assigned(FDataSet.OnCalcFields),
    'OnCalcFields is set on ' + FDataSet.name);
  FDataSet.OnCalcFields := OnCalcFields;

  Assert(not Assigned(FDataSet.OnNewRecord),
    'OnNewRecord is set on ' + FDataSet.name);
  FDataSet.OnNewRecord := OnNewRecord;

  Assert(not Assigned(FDataSet.OnEditError),
    'OnEditError is set on ' + FDataSet.name);
  FDataSet.OnEditError := OnEditError;

  // All scroll events
  Assert(not Assigned(FDataSet.AfterScroll),
    'AfterScroll is set on ' + FDataSet.name);
  Assert(not Assigned(FDataSet.AfterRefresh),
    'AfterRefresh is set on ' + FDataSet.name);
  HookScrollEvents;
end;

procedure TMyModule.HookScrollEvents;
begin
  FDataSet.AfterScroll := AfterScroll;
  FDataSet.AfterRefresh := AfterRefresh;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ctx: TRttiContext;
  rttitype: TRttiType;
  // rttiprop: TRttiIndexedProperty;
  value: TValue;
  mm: TMyModule;
  i: Integer;
  rttifield: TRttiField;
begin
  {
    rttitype := ctx.GetType(StringGrid1.ClassType);
    rttitype.Get
    rttiprop := rttitype.GetIndexedProperty('Cells');
    value := rttiprop.GetValue(StringGrid1, [1, 1]);
    rttiprop.SetValue(StringGrid1, [1, 1], value.ToString + ' hello');
    }
  mm := TMyModule.Create;
  rttitype := ctx.GetType(mm.ClassType);
  for i := 0 to ClientDataSet1.Fields.Count - 1 do
  begin
    rttifield := rttitype.GetField
      ('FField' + ClientDataSet1.Fields[i].FieldName);
    rttifield.SetValue(mm, ClientDataSet1.Fields[i]);
  end;
  ClientDataSet1.First;
  StringGrid1.RowCount := ClientDataSet1.RecordCount + 1;
  while not ClientDataSet1.Eof do
  begin
    StringGrid1.Cells[1, ClientDataSet1.RecNo] := mm.FFieldName.value;
    StringGrid1.Cells[2, ClientDataSet1.RecNo] := mm.FFieldCapital.value;
    StringGrid1.Cells[3, ClientDataSet1.RecNo] := mm.FFieldContinent.value;
    StringGrid1.Cells[4, ClientDataSet1.RecNo] := FloatToStr
      (mm.FFieldArea.value);
    ClientDataSet1.Next;
  end;
end;

end.
