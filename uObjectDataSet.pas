unit uObjectDataSet;

interface

uses
  DB, SysUtils;

type
  TObjectDataSet = class
  public
    FDataSet: TDataSet;
    constructor Create(DataSet: TDataSet);
    procedure ForEach(OnEachRow: TProc);
  protected
  private
    procedure ConnectDBFieldsToObjectFields;
    procedure ConnectMethodsToEvents;
  end;

implementation

uses
  Rtti, Generics.Collections;

resourcestring
  StrAttributeNotFound = 'Attribute: %s.%s not found. Can''t connect ' +
    'data field: %s';

constructor TObjectDataSet.Create(DataSet: TDataSet);
begin
  FDataSet := DataSet;
  ConnectDBFieldsToObjectFields;
  ConnectMethodsToEvents;
end;

procedure TObjectDataSet.ForEach(OnEachRow: TProc);
var
  bookmark: TBookmark;
begin
  bookmark := FDataSet.GetBookmark;
  FDataSet.DisableControls;
  try
    FDataSet.First;
    while not FDataSet.Eof do
    begin
      OnEachRow;
      FDataSet.Next;
    end;
    if FDataSet.BookmarkValid(bookmark) then
      FDataSet.GotoBookmark(bookmark);
  finally
    FDataSet.FreeBookmark(bookmark);
    FDataSet.EnableControls;
  end;
end;

procedure TObjectDataSet.ConnectDBFieldsToObjectFields;
var
  ctx: TRttiContext;
  typeRuntimeInfo: TRttiType;
  fieldRuntimeInfo: TRttiField;
  i: Integer;
  dataFieldName: string;
  objectFieldName: string;
  msg: string;
begin
  typeRuntimeInfo := ctx.GetType(Self.ClassType);
  for i := 0 to Self.FDataSet.Fields.Count - 1 do
  begin
    dataFieldName := Self.FDataSet.Fields[i].FieldName;
    objectFieldName := 'FField' + dataFieldName;
    fieldRuntimeInfo := typeRuntimeInfo.GetField(objectFieldName);
    if not Assigned(fieldRuntimeInfo) then
    begin
      msg := Format(StrAttributeNotFound, [Self.ClassName, objectFieldName,
        dataFieldName]);
      raise Exception.Create(msg);
    end;
    fieldRuntimeInfo.SetValue(Self, Self.FDataSet.Fields[i]);
  end;
end;

procedure TObjectDataSet.ConnectMethodsToEvents;
  function GetEventHandler(methodName: string): TDataSetNotifyEvent;
  var
    ctx: TRttiContext;
    methodRtti: TRttiMethod;
  begin
    methodRtti := ctx.GetType(Self.ClassType).GetMethod(methodName);
    if Assigned(methodRtti) then
    begin
      TMethod(Result).Code := methodRtti.CodeAddress;
      TMethod(Result).Data := Self;
    end
    else
      Result := nil;
  end;
  procedure SetDataSetEvent(eventName: string;
    eventHandler: TDataSetNotifyEvent);
  var
    ctx: TRttiContext;
    propertyRtti: TRttiProperty;
  begin
    propertyRtti := ctx.GetType(FDataSet.ClassInfo).GetProperty(eventName);
    { TODO : Dodaæ Assert sprawdzaj¹cy czy property by³o ustawione }
    // u¿yæ: propertyRtti.GetValue(FDataSet);
    propertyRtti.SetValue(FDataSet,
      TValue.From<TDataSetNotifyEvent>(eventHandler));
  end;

const
  SupportedEvents: TArray<String> = ['AfterDelete', 'AfterOpen', 'AfterPost',
    'AfterScroll', 'AfterRefresh', 'BeforeDelete', 'BeforeEdit', 'BeforeInsert',
    'BeforePost', 'OnCalcFields', 'OnNewRecord'];
var
  i: Integer;
  n: string;
  handler: TDataSetNotifyEvent;
begin
  for i := 0 to Length(SupportedEvents) - 1 do
  begin
    n := SupportedEvents[i];
    handler := GetEventHandler(n);
    if Assigned(handler) then
      SetDataSetEvent(n, handler);
  end;
end;

end.
