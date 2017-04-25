unit uObjectDataSet;

interface

uses
  DB, SysUtils;

type
  TObjectDataSet = class
  private
    FDataSet: TDataSet;
    procedure ForEach(NotifyDataSources: boolean; OnEachRow: TProc);
    procedure ConnectDBFieldsToObjectFields;
    procedure ConnectMethodsToEvents;
  protected
    function GetRecNo: integer;
  public
    constructor Create(DataSet: TDataSet);
    procedure ForEachWithNotification(OnEachRow: TProc);
    procedure ForEachMute(OnEachRow: TProc);
    procedure First;
    procedure Prior;
    procedure Next;
    function RecordCount: integer;
    property DataSet: TDataSet read FDataSet;
    property RecNo: integer read GetRecNo;
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

procedure TObjectDataSet.First;
begin
  FDataSet.First;
end;

procedure TObjectDataSet.ForEach(NotifyDataSources: boolean; OnEachRow: TProc);
var
  bookmark: TBookmark;
begin
  bookmark := FDataSet.GetBookmark;
  if NotifyDataSources then
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
    if NotifyDataSources then
      FDataSet.EnableControls;
  end;
end;

procedure TObjectDataSet.ForEachMute(OnEachRow: TProc);
begin
  ForEach(false, OnEachRow);
end;

procedure TObjectDataSet.ForEachWithNotification(OnEachRow: TProc);
begin
  ForEach(true, OnEachRow);
end;

function TObjectDataSet.GetRecNo: integer;
begin
  Result := FDataSet.RecNo;
end;

procedure TObjectDataSet.Next;
begin
  FDataSet.Next;
end;

procedure TObjectDataSet.Prior;
begin
  FDataSet.Prior;
end;

function TObjectDataSet.RecordCount: integer;
begin
  Result := FDataSet.RecordCount;
end;

procedure TObjectDataSet.ConnectDBFieldsToObjectFields;
var
  ctx: TRttiContext;
  typeRuntimeInfo: TRttiType;
  fieldRuntimeInfo: TRttiField;
  i: integer;
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
    Result := nil;
    for methodRtti in ctx.GetType(Self.ClassType).GetMethods do
      if methodRtti.Name = methodName then
      begin
        if Length(methodRtti.GetParameters) = 1 then
        { TODO : Sprawdziæ czy metoda ma jeden parametr typu TDataSet }
        begin
          TMethod(Result).Code := methodRtti.CodeAddress;
          TMethod(Result).Data := Self;
        end;
      end;
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
    propertyRtti.SetValue(FDataSet, TValue.From<TDataSetNotifyEvent>
        (eventHandler));
  end;

var
  SupportedEvents: TArray<String>;
var
  i: integer;
  n: string;
  handler: TDataSetNotifyEvent;
begin
  SupportedEvents := TArray<String>.Create('AfterDelete', 'AfterOpen',
    'AfterPost', 'AfterScroll', 'AfterRefresh', 'BeforeDelete', 'BeforeEdit',
    'BeforeInsert', 'BeforePost', 'OnCalcFields', 'OnNewRecord');
  // for i := 0 to Length(SupportedEvents) - 1 do
  for n in SupportedEvents do
  begin
    // n := SupportedEvents[i];
    handler := GetEventHandler(n);
    if Assigned(handler) then
      SetDataSetEvent(n, handler);
  end;
end;

end.
