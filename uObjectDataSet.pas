unit uObjectDataSet;

interface

uses
  Rtti, DB, SysUtils;

type
  TObjectDataSet = class
  public
    FDataSet: TDataSet;
    constructor Create(DataSet: TDataSet);
    procedure ForEach(OnEachRow: TProc);
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
    procedure ConnectDBFieldsToObjectFields;
  end;

implementation

resourcestring
  StrAttributeNotFound = 'Attribute: %s.%s not found. Can''t connect ' +
    'data field: %s';

{$REGION 'Methods with TDataSet events implementations'}

procedure TObjectDataSet.AfterDelete(aDataSet: TDataSet);
begin
  // AfterDelete;
end;

procedure TObjectDataSet.AfterOpen(aDataSet: TDataSet);
begin
  // AfterOpen;
end;

procedure TObjectDataSet.AfterPost(aDataSet: TDataSet);
begin
  // AfterPost;
end;

procedure TObjectDataSet.AfterScroll(aDataSet: TDataSet);
begin
  // AfterScroll;
end;

procedure TObjectDataSet.AfterRefresh(aDataSet: TDataSet);
begin
  // AfterRefresh;
end;

procedure TObjectDataSet.BeforeDelete(aDataSet: TDataSet);
begin
  // BeforeDelete;
end;

procedure TObjectDataSet.BeforeEdit(aDataSet: TDataSet);
begin
  // BeforeEdit;
end;

procedure TObjectDataSet.BeforeInsert(aDataSet: TDataSet);
begin
  // BeforeInsert;
end;

procedure TObjectDataSet.BeforePost(aDataSet: TDataSet);
begin
  // BeforePost;
end;

procedure TObjectDataSet.OnCalcFields(aDataSet: TDataSet);
begin
  // OnCalcFields;
end;

procedure TObjectDataSet.OnNewRecord(aDataSet: TDataSet);
begin
  // OnNewRecord;
end;

procedure TObjectDataSet.OnEditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  // OnEditError(E, Action);
end;
{$ENDREGION}

constructor TObjectDataSet.Create(DataSet: TDataSet);
begin
  FDataSet := DataSet;
  HookEvents;
  ConnectDBFieldsToObjectFields
end;

procedure TObjectDataSet.ForEach(OnEachRow: TProc);
var
  Bookmark: TBookmark;
begin
  Bookmark := FDataSet.GetBookmark;
  FDataSet.DisableControls;
  try
    FDataSet.First;
    while not FDataSet.Eof do
    begin
      OnEachRow;
      FDataSet.Next;
    end;
  finally
    FDataSet.GotoBookmark(Bookmark);
    FDataSet.FreeBookmark(Bookmark);
    FDataSet.EnableControls;
  end;
end;

procedure TObjectDataSet.ConnectDBFieldsToObjectFields;
var
  ctx: TRttiContext;
  rttitype: TRttiType;
  rttifield: TRttiField;
  i: Integer;
  DataFieldName: string;
  ObjectFieldName: string;
  msg: string;
begin
  rttitype := ctx.GetType(Self.ClassType);
  for i := 0 to Self.FDataSet.Fields.Count - 1 do
  begin
    DataFieldName := Self.FDataSet.Fields[i].FieldName;
    ObjectFieldName := 'FField' + DataFieldName;
    rttifield := rttitype.GetField(ObjectFieldName);
    if not Assigned(rttifield) then
    begin
      msg := Format(StrAttributeNotFound, [Self.ClassName, ObjectFieldName,
        DataFieldName]);
      raise Exception.Create(msg);
    end;
    rttifield.SetValue(Self, Self.FDataSet.Fields[i]);
  end;
end;

procedure TObjectDataSet.HookEvents;
begin
  Assert(not Assigned(FDataSet.BeforePost), 'BeforePost is set on ' +
    FDataSet.name);
  FDataSet.BeforePost := BeforePost;

  Assert(not Assigned(FDataSet.BeforeDelete), 'BeforeDelete is set on ' +
    FDataSet.name);
  FDataSet.BeforeDelete := BeforeDelete;

  Assert(not Assigned(FDataSet.BeforeEdit), 'BeforeEdit is set on ' +
    FDataSet.name);
  FDataSet.BeforeEdit := BeforeEdit;

  Assert(not Assigned(FDataSet.BeforeInsert), 'BeforeInsert is set on ' +
    FDataSet.name);
  FDataSet.BeforeInsert := BeforeInsert;

  Assert(not Assigned(FDataSet.AfterOpen), 'AfterOpen is set on ' +
    FDataSet.name);
  FDataSet.AfterOpen := AfterOpen;

  Assert(not Assigned(FDataSet.AfterPost), 'AfterPost is set on ' +
    FDataSet.name);
  FDataSet.AfterPost := AfterPost;

  Assert(not Assigned(FDataSet.AfterDelete), 'AfterDelete is set on ' +
    FDataSet.name);
  FDataSet.AfterDelete := AfterDelete;

  Assert(not Assigned(FDataSet.OnCalcFields), 'OnCalcFields is set on ' +
    FDataSet.name);
  FDataSet.OnCalcFields := OnCalcFields;

  Assert(not Assigned(FDataSet.OnNewRecord), 'OnNewRecord is set on ' +
    FDataSet.name);
  FDataSet.OnNewRecord := OnNewRecord;

  Assert(not Assigned(FDataSet.OnEditError), 'OnEditError is set on ' +
    FDataSet.name);
  FDataSet.OnEditError := OnEditError;

  Assert(not Assigned(FDataSet.AfterScroll), 'AfterScroll is set on ' +
    FDataSet.name);
  FDataSet.AfterScroll := AfterScroll;

  Assert(not Assigned(FDataSet.AfterRefresh), 'AfterRefresh is set on ' +
    FDataSet.name);
  FDataSet.AfterRefresh := AfterRefresh;
end;

end.
