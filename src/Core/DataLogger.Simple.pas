{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

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

function Trace(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Trace(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Trace(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Debug(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Debug(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Debug(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Info(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Info(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Info(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Success(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Success(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Success(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Warn(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Warn(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Warn(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Error(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Error(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Error(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Fatal(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Fatal(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Fatal(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Custom(const ALogLevel: string; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Custom(const ALogLevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Custom(const ALogLevel: string; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function Log(const ALogLevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Log(const ALogLevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function Log(const ALogLevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function T(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function T(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function T(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function D(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function D(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function D(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function I(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function I(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function I(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function S(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function S(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function S(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function W(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function W(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function W(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function E(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function E(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function E(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function F(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function F(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function F(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function C(const ALevel: string; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function C(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function C(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function L(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function L(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
function L(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;

function SlineBreak: TDataLogger;

function StartTransaction: TDataLogger;
function CommitTransaction: TDataLogger;
function RollbackTransaction: TDataLogger;
function InTransaction: Boolean;

implementation

var
  FDataLogger: TDataLogger;

function GetDataLogger: TDataLogger;
begin
  if not Assigned(FDataLogger) then
    Result := Logger
  else
    Result := FDataLogger;
end;

function SetDataLogger(const ADataLogger: TDataLogger): TDataLogger;
begin
  FDataLogger := ADataLogger;
  Result := GetDataLogger;
end;

function Trace(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Trace(AMessage, ATagName, ATargetProviderIndex);
end;

function Trace(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Trace(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Trace(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Trace(AMessage, ATagName, ATargetProviderIndex);
end;

function Debug(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Debug(AMessage, ATagName, ATargetProviderIndex);
end;

function Debug(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Debug(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Debug(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Debug(AMessage, ATagName, ATargetProviderIndex);
end;

function Info(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Info(AMessage, ATagName, ATargetProviderIndex);
end;

function Info(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Info(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Info(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Info(AMessage, ATagName, ATargetProviderIndex);
end;

function Success(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Success(AMessage, ATagName, ATargetProviderIndex);
end;

function Success(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Success(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Success(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Success(AMessage, ATagName, ATargetProviderIndex);
end;

function Warn(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Warn(AMessage, ATagName, ATargetProviderIndex);
end;

function Warn(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Warn(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Warn(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Warn(AMessage, ATagName, ATargetProviderIndex);
end;

function Error(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Error(AMessage, ATagName, ATargetProviderIndex);
end;

function Error(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Error(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Error(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Error(AMessage, ATagName, ATargetProviderIndex);
end;

function Fatal(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Fatal(AMessage, ATagName, ATargetProviderIndex);
end;

function Fatal(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Fatal(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Fatal(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.Fatal(AMessage, ATagName, ATargetProviderIndex);
end;

function Custom(const ALogLevel: string; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger;
begin
  Result := GetDataLogger.Custom(ALogLevel, AMessage, ATagName, ATargetProviderIndex);
end;

function Custom(const ALogLevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger;
begin
  Result := GetDataLogger.Custom(ALogLevel, AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Custom(const ALogLevel: string; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger;
begin
  Result := GetDataLogger.Custom(ALogLevel, AMessage, ATagName, ATargetProviderIndex);
end;

function Log(const ALogLevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger;
begin
  Result := GetDataLogger.Log(ALogLevel, AMessage, ATagName, ATargetProviderIndex);
end;

function Log(const ALogLevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger;
begin
  Result := GetDataLogger.Log(ALogLevel, AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function Log(const ALogLevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger;
begin
  Result := GetDataLogger.Log(ALogLevel, AMessage, ATagName, ATargetProviderIndex);
end;

function T(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.T(AMessage, ATagName, ATargetProviderIndex);
end;

function T(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.T(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function T(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.T(AMessage, ATagName, ATargetProviderIndex);
end;

function D(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.D(AMessage, ATagName, ATargetProviderIndex);
end;

function D(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.D(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function D(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.D(AMessage, ATagName, ATargetProviderIndex);
end;

function I(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.I(AMessage, ATagName, ATargetProviderIndex);
end;

function I(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.I(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function I(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.I(AMessage, ATagName, ATargetProviderIndex);
end;

function S(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.S(AMessage, ATagName, ATargetProviderIndex);
end;

function S(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.S(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function S(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.S(AMessage, ATagName, ATargetProviderIndex);
end;

function W(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.W(AMessage, ATagName, ATargetProviderIndex);
end;

function W(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.W(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function W(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.W(AMessage, ATagName, ATargetProviderIndex);
end;

function E(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.E(AMessage, ATagName, ATargetProviderIndex);
end;

function E(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.E(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function E(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.E(AMessage, ATagName, ATargetProviderIndex);
end;

function F(const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.F(AMessage, ATagName, ATargetProviderIndex);
end;

function F(const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.F(AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function F(const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.F(AMessage, ATagName, ATargetProviderIndex);
end;

function C(const ALevel: string; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.C(ALevel, AMessage, ATagName, ATargetProviderIndex);
end;

function C(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.C(ALevel, AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function C(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.C(ALevel, AMessage, ATagName, ATargetProviderIndex);
end;

function L(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.L(ALevel, AMessage, ATagName, ATargetProviderIndex);
end;

function L(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.L(ALevel, AMessage, AArgs, ATagName, ATargetProviderIndex);
end;

function L(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''; const ATargetProviderIndex: Integer = -1): TDataLogger; overload;
begin
  Result := GetDataLogger.L(ALevel, AMessage, ATagName, ATargetProviderIndex);
end;

function SlineBreak: TDataLogger;
begin
  Result := GetDataLogger.SlineBreak;
end;

function StartTransaction: TDataLogger;
begin
  Result := GetDataLogger.StartTransaction;
end;

function CommitTransaction: TDataLogger;
begin
  Result := GetDataLogger.CommitTransaction;
end;

function RollbackTransaction: TDataLogger;
begin
  Result := GetDataLogger.RollbackTransaction;
end;

function InTransaction: Boolean;
begin
  Result := GetDataLogger.InTransaction;
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
