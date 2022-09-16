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

unit DataLogger.Utils;

interface

uses
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows,
{$ELSEIF DEFINED(LINUX)}
    Posix.SysUtsname, Posix.Unistd, Posix.SysStat,
{$ELSEIF DEFINED(MACOS)}
    Macapi.Helpers, Macapi.CoreFoundation, Macapi.ObjectiveC, Macapi.ObjCRuntime,
{$ELSEIF DEFINED(IOS)}
    IOSApi.Foundation, IOSApi.Helpers,
{$ELSEIF DEFINED(ANDROID)}
    Androidapi.Helpers, Androidapi.JNI.OS, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Provider,
{$ENDIF}
  IdStack,
  System.IOUtils, System.SysUtils, System.Types, System.RTTI, System.JSON, REST.JSON;

type
  TLoggerUtils = class
  type
    TAppVersion = record
      Comments: string;
      CompanyName: string;
      FileDescription: string;
      FileVersion: string;
      InternalName: string;
      LegalCopyright: string;
      LegalTrademarks: string;
      OriginalFilename: string;
      ProductName: string;
      ProductVersion: string;
    end;
  private
    class var FAppName: string;
    class var FAppPath: string;
    class var FAppVersion: TAppVersion;
    class var FAppSize: string;
    class var FOS: string;
    class var FProcessId: string;
  public
    class function AppName: string;
    class function AppPath: string;
    class function AppVersion: TAppVersion;
    class function AppSize: Double;
    class function ComputerName: string;
    class function Username: string;
    class function OS: string;
    class function ProcessId: string;
    class function IPLocal: string;
  end;

  TLoggerRTTI = class
  private
  public
    class function CreateObject(const AName: string): TObject;
  end;

  TLoggerJSON = class
  private
  public
    class function Format(const AValue: TJSONValue; const AFormat: Boolean = False): string;
  end;

implementation

function GetSize(const Path: string): Int64;
{$IFDEF MSWINDOWS}

var
  LPath: string;
  LInfo: TWin32FileAttributeData;
begin
  if (Length(Path) < MAX_PATH) or TPath.IsExtendedPrefixed(Path) then
    LPath := Path
  else
    LPath := '\\?\' + Path;

  if GetFileAttributesEx(PChar(LPath), GetFileExInfoStandard, @LInfo) then
  begin
    Result := LInfo.nFileSizeHigh;
    Result := Result shl 32 + LInfo.nFileSizeLow;
  end
  else
  begin
    Result := -1;
  end;
end;

{$ELSE}

var
  LFileName: UTF8String;
  LStatBuf: _stat;
begin
  LFileName := UTF8Encode(Path);
  if stat(PAnsiChar(LFileName), LStatBuf) = 0 then
    Result := LStatBuf.st_size
  else
    Result := -1;
end;

{$ENDIF}

{ TLoggerUtils }

class function TLoggerUtils.AppName: string;
{$IF DEFINED(ANDROID)}

var
  LPackageManager: JPackageManager;
  LPackageName: JString;
begin
  if not Trim(FAppName).IsEmpty then
    Exit(FAppName);

  LPackageManager := TAndroidHelper.Activity.getPackageManager;
  LPackageName := TAndroidHelper.Context.getPackageName;

  Result := JStringToString(LPackageManager.getPackageInfo(LPackageName, 0).applicationInfo.loadLabel(LPackageManager).toString);

  FAppName := Result;
end;

{$ELSEIF DEFINED(IOS)}

begin
  if not Trim(FAppName).IsEmpty then
    Exit(FAppName);

  Result := TNSString.Wrap(CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, KCFBundleIdentifierKey)).UTF8String;

  FAppName := Result;
end;

{$ELSE}

var
  LAppPathFull: string;
begin
  if not Trim(FAppName).IsEmpty then
    Exit(FAppName);

  if IsLibrary then
    LAppPathFull := GetModuleName(0)
  else
    LAppPathFull := ParamStr(0);

  Result := TPath.GetFileNameWithoutExtension(LAppPathFull);

  FAppName := Result;
end;

{$ENDIF}

class function TLoggerUtils.AppPath: string;
{$IF DEFINED(ANDROID) OR DEFINED(IOS)}

begin
  if not Trim(FAppPath).IsEmpty then
    Exit(FAppPath);

  Result := TPath.GetDocumentsPath;

  FAppPath := Result;
end;

{$ELSE}

var
  LAppPathFull: string;
begin
  if not Trim(FAppPath).IsEmpty then
    Exit(FAppPath);

  if IsLibrary then
    LAppPathFull := GetModuleName(0)
  else
    LAppPathFull := ParamStr(0);

  Result := TPath.GetDirectoryName(LAppPathFull);

  FAppPath := Result;
end;

{$ENDIF}

class function TLoggerUtils.AppVersion: TAppVersion;
{$IF DEFINED(ANDROID)}

var
  LPackageInfo: JPackageInfo;
begin
  if not Trim(FAppPath).IsEmpty then
    Exit(FAppPath);

  LPackageInfo := TAndroidHelper.Activity.getPackageManager.getPackageInfo(TAndroidHelper.Context.getPackageName(), TJPackageManager.JavaClass.GET_ACTIVITIES);

  Result.FileVersion := IntToStr(LPackageInfo.VersionCode);
  Result.FileDescription := JStringToString(LPackageInfo.versionName);
end;

{$ELSEIF DEFINED(IOS)}

var
  AppKey: Pointer;
  AppBundle: NSBundle;
  BuildStr: NSString;
begin
  if not Trim(FAppVersion.FileVersion).IsEmpty then
    Exit(FAppVersion);

  try
    AppKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
    AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
    BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));

    Result.FileVersion := UTF8ToString(BuildStr.UTF8String);
  except
    Result.FileVersion := TNSString.Wrap(CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, kCFBundleVersionKey)).UTF8String;
  end;

  FAppVersion := Result;
end;

{$ELSEIF DEFINED(MSWINDOWS)}

var
  LAppPathFull: string;
  LInfoSize: DWORD;
  LDummy: DWORD;
  LInfo: array of Char;
  LBuffer: PChar;
  LKey: string;
  LP: Pointer;
  LLen: cardinal;
begin
  if not Trim(FAppVersion.FileVersion).IsEmpty then
    Exit(FAppVersion);

  Result := Default (TAppVersion);

  try
    if IsLibrary then
      LAppPathFull := GetModuleName(0)
    else
      LAppPathFull := ParamStr(0);

    LInfoSize := GetFileVersionInfoSize(pWideChar(LAppPathFull), LDummy);
    if LInfoSize = 0 then
      Exit;

    SetLength(LInfo, LInfoSize);
    GetFileVersionInfo(pWideChar(LAppPathFull), 0, LInfoSize, LInfo);
    VerQueryValue(LInfo, '\VarFileInfo\Translation', LP, LLen);

    if Assigned(LP) then
    begin
      LKey := Format('\StringFileInfo\%4.4x%4.4x', [LoWord(Longint(LP^)), HiWord(Longint(LP^))]);

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'Comments'])), Pointer(LBuffer), LInfoSize) then
        Result.Comments := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'CompanyName'])), Pointer(LBuffer), LInfoSize) then
        Result.CompanyName := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'FileDescription'])), Pointer(LBuffer), LInfoSize) then
        Result.FileDescription := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'FileVersion'])), Pointer(LBuffer), LInfoSize) then
        Result.FileVersion := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'InternalName'])), Pointer(LBuffer), LInfoSize) then
        Result.InternalName := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'LegalCopyright'])), Pointer(LBuffer), LInfoSize) then
        Result.LegalCopyright := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'OriginalFilename'])), Pointer(LBuffer), LInfoSize) then
        Result.OriginalFilename := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'ProductName'])), Pointer(LBuffer), LInfoSize) then
        Result.ProductName := LBuffer;

      if VerQueryValue(LInfo, PChar(Format('%s\%s', [LKey, 'ProductVersion'])), Pointer(LBuffer), LInfoSize) then
        Result.ProductVersion := LBuffer;
    end;
  except
  end;

  FAppVersion := Result;
end;

{$ELSE}

begin
  Result := default (TAppVersion);
end;

{$ENDIF}

class function TLoggerUtils.AppSize: Double;
var
  LAppPathFull: string;
begin
  if not Trim(FAppSize).IsEmpty then
    Exit(StrToFloatDef(FAppSize, 0));

  Result := 0;

  try
    if IsLibrary then
      LAppPathFull := GetModuleName(0)
    else
      LAppPathFull := ParamStr(0);

{$IF RTLVersion > 32} // 32 = Delphi Tokyo (10.2)
    Result := TFile.GetSize(LAppPathFull);
{$ELSE}
    Result := GetSize(LAppPathFull);
{$ENDIF}
    Result := Result / 1024; // Kb
  except
  end;

  FAppSize := Result.toString;
end;

class function TLoggerUtils.ComputerName: string;
{$IF DEFINED(ANDROID)}

begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);

  if Result.Trim.IsEmpty then
    Result := Format('%s %s', [JStringToString(TJBuild.JavaClass.MANUFACTURER), JStringToString(TJBuild.JavaClass.PRODUCT)]);
end;

{$ELSEIF DEFINED(IOS)}

begin
  Result := '';
end;

{$ELSEIF DEFINED(LINUX)}

var
  LName: utsname;
begin
  uname(LName);
  Result := string(AnsiString(LName.nodename));
end;

{$ELSEIF DEFINED(MSWINDOWS)}

var
  LBuffer: array [0 .. MAX_COMPUTERNAME_LENGTH + 1] of Char;
  LSize: cardinal;
begin
  LSize := High(LBuffer);

  if GetComputerName(LBuffer, LSize) then
    Result := string(LBuffer)
  else
    Result := '';
end;

{$ELSE}

begin
  Result := EmptyStr;
end;

{$ENDIF}

class function TLoggerUtils.Username: string;
{$IF DEFINED(MSWINDOWS)}
var
  LBuffer: array [0 .. 2 * MAX_COMPUTERNAME_LENGTH + 1] of Char;
  LSize: cardinal;
begin
  LSize := High(LBuffer);

  if GetUserName(LBuffer, LSize) then
    Result := string(LBuffer)
  else
    Result := '';
end;

{$ELSE}

begin
  Result := '';
end;

{$ENDIF}

class function TLoggerUtils.OS: string;
begin
  if not Trim(FOS).IsEmpty then
    Exit(FOS);

  Result := TOSVersion.toString;

  FOS := Result;
end;

class function TLoggerUtils.ProcessId: string;
var
  LProcessId: Integer;
begin
  if not Trim(FProcessId).IsEmpty then
    Exit(FProcessId);

{$IF DEFINED(MSWINDOWS)}
  LProcessId := GetCurrentProcessId;
{$ELSEIF DEFINED(LINUX)}
  LProcessId := getpid;
{$ELSE}
  LProcessId := 0;
{$ENDIF}
  Result := LProcessId.toString;

  FProcessId := Result;
end;

class function TLoggerUtils.IPLocal: string;
begin
  Result := 'INVALID';
  try
    TIdStack.IncUsage;
    try
      Result := GStack.LocalAddress;
    finally
      TIdStack.DecUsage;
    end;
  except
  end;
end;

{ TLoggerRTTI }

class function TLoggerRTTI.CreateObject(const AName: string): TObject;
var
  LName: string;
  LRttiContext: TRttiContext;
  LRttiTypes: TArray<TRttiType>;
  LRttiType: TRttiType;
  LFound: Boolean;
  LRttiInstanceType: TRttiInstanceType;
  LValue: TValue;
begin
  Result := nil;

  if AName.Trim.IsEmpty then
    Exit;

  LName := AName.ToLower;
  LRttiContext := TRttiContext.Create;
  try
    LRttiTypes := LRttiContext.GetTypes;
    LFound := False;

    LRttiType := nil;
    for LRttiType in LRttiTypes do
    begin
      if (SameText(LName, LRttiType.Name.ToLower)) then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
      Exit;

    LRttiInstanceType := LRttiType.AsInstance;
    LValue := LRttiInstanceType.GetMethod('Create').Invoke(LRttiInstanceType.MetaclassType, []);
  finally
    LRttiContext.Free;
  end;

  Result := LValue.AsObject;
end;

{ TLoggerJSON }

class function TLoggerJSON.Format(const AValue: TJSONValue; const AFormat: Boolean = False): string;
begin
  if AFormat then
{$IF RTLVersion > 32} // 32 = Delphi Tokyo (10.2)
    Result := AValue.Format
{$ELSE}
    Result := TJSON.Format(AValue)
{$ENDIF}
  else
    Result := AValue.toString;
end;

initialization

TLoggerUtils.FAppName := '';
TLoggerUtils.FAppPath := '';
TLoggerUtils.FAppVersion := Default (TLoggerUtils.TAppVersion);
TLoggerUtils.FAppSize := '';
TLoggerUtils.FOS := '';
TLoggerUtils.FProcessId := '';

end.
