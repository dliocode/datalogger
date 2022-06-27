unit DataLogger.Simple;

interface

uses
  DataLogger,
  System.JSON;

type
  TDataLoggerSimple = class
  public
    class function SetDataLogger(const ADataLogger: TDataLogger): TDataLogger;
  end;

function Trace(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Trace(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Trace(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function Debug(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Debug(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Debug(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function Info(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Info(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Info(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function Success(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Success(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Success(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function Warn(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Warn(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Warn(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function Error(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Error(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Error(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function Fatal(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function Fatal(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
function Fatal(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function CustomType(const ATypeCustom: string; const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function CustomType(const ATypeCustom: string; const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function &Type(const AType: TLoggerType; const AMessage: string; const ATag: string = ''): TDataLogger; overload;
function &Type(const AType: TLoggerType; const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
function SlineBreak: TDataLogger;

implementation

var
  FDataLogger: TDataLogger;

function GetDataLogger: TDataLogger;
begin
  if not Assigned(FDataLogger) then
    raise EDataLoggerException.Create('DataLogger not defined. Use TDataLoggerSimple.SetDataLogger!');

  Result := FDataLogger;
end;

function SetDataLogger(const ADataLogger: TDataLogger): TDataLogger;
begin
  FDataLogger := ADataLogger;
  Result := GetDataLogger;
end;

function Trace(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Trace(AMessage, ATag);
end;

function Trace(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Trace(AMessage, AArgs, ATag);
end;

function Trace(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Trace(AMessage, ATag);
end;

function Debug(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Debug(AMessage, ATag);
end;

function Debug(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Debug(AMessage, AArgs, ATag);
end;

function Debug(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Debug(AMessage, ATag);
end;

function Info(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Info(AMessage, ATag);
end;

function Info(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Info(AMessage, AArgs, ATag);
end;

function Info(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Info(AMessage, ATag);
end;

function Success(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Success(AMessage, ATag);
end;

function Success(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Success(AMessage, AArgs, ATag);
end;

function Success(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Success(AMessage, ATag);
end;

function Warn(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Warn(AMessage, ATag);
end;

function Warn(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Warn(AMessage, AArgs, ATag);
end;

function Warn(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Warn(AMessage, ATag);
end;

function Error(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Error(AMessage, ATag);
end;

function Error(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Error(AMessage, AArgs, ATag);
end;

function Error(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Error(AMessage, ATag);
end;

function Fatal(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Fatal(AMessage, ATag);
end;

function Fatal(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Fatal(AMessage, AArgs, ATag);
end;

function Fatal(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.Fatal(AMessage, ATag);
end;

function CustomType(const ATypeCustom: string; const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.CustomType(ATypeCustom, AMessage, ATag);
end;

function CustomType(const ATypeCustom: string; const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.CustomType(ATypeCustom, AMessage, ATag);
end;

function &Type(const AType: TLoggerType; const AMessage: string; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.&Type(AType, AMessage, ATag);
end;

function &Type(const AType: TLoggerType; const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
begin
  Result := GetDataLogger.&Type(AType, AMessage, ATag);
end;

function SlineBreak: TDataLogger;
begin
  Result := GetDataLogger.SlineBreak;
end;

{ TDataLoggerSimple }

class function TDataLoggerSimple.SetDataLogger(const ADataLogger: TDataLogger): TDataLogger;
begin
  FDataLogger := ADataLogger;
  Result := GetDataLogger;
end;

initialization

FDataLogger := nil;

end.
