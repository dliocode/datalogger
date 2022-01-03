{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}
unit DataLogger.Utils;

interface

uses
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}

{$IF DEFINED(LINUX)}
  Posix.SysUtsname, Posix.Unistd,
{$ENDIF}

{$IFDEF MACOS}
  Macapi.Helpers, Macapi.CoreFoundation, Macapi.ObjectiveC, Macapi.ObjCRuntime,
{$ENDIF}

{$IFDEF IOS}
  IOSApi.Foundation, IOSApi.Helpers,
{$ENDIF}

{$IF DEFINED(ANDROID)}
  Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Provider,
{$ENDIF}

  System.IOUtils, System.SysUtils;

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
  public
    class function AppName: string;
    class function AppPath: string;
    class function AppVersion: TAppVersion;
    class function ComputerName: string;
    class function Os: string;
    class function ProcessId: Integer;
    class function Username: string;
  end;

implementation

{ TLoggerUtils }
class function TLoggerUtils.AppName: string;
{$IF DEFINED(ANDROID)}
var
  LPackageManager: JPackageManager;
  LPackageName: JString;
begin
  LPackageManager := TAndroidHelper.Activity.getPackageManager;
  LPackageName := TAndroidHelper.Context.getPackageName;
  Result := JStringToString(LPackageManager.getPackageInfo(LPackageName, 0).applicationInfo.loadLabel(LPackageManager).toString);
end;
{$ELSEIF DEFINED(IOS)}
begin
  Result := TNSString.Wrap(CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, KCFBundleIdentifierKey)).UTF8String;
end;
{$ELSE}
var
  LAppPathFull: string;
begin
  if IsLibrary then
    LAppPathFull := GetModuleName(0)
  else
    LAppPathFull := ParamStr(0);
  Result := TPath.GetFileNameWithoutExtension(LAppPathFull);
end;
{$ENDIF}

class function TLoggerUtils.AppPath: string;
{$IF DEFINED(ANDROID) OR DEFINED(IOS)}
begin
  Result := TPath.GetDocumentsPath;
end;
{$ELSE}
var
  LAppPathFull: string;
begin
  if IsLibrary then
    LAppPathFull := GetModuleName(0)
  else
    LAppPathFull := ParamStr(0);
  Result := TPath.GetDirectoryName(LAppPathFull);
end;
{$ENDIF}

class function TLoggerUtils.AppVersion: TAppVersion;
{$IF DEFINED(ANDROID)}
var
  LPackageInfo: JPackageInfo;
begin
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
  try
    AppKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
    AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
    BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));
    Result.FileVersion := UTF8ToString(BuildStr.UTF8String);
  except
  end;
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
end;
{$ELSE}
begin
  Result := default(TAppVersion);
end;
{$ENDIF}

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
  Buf: array [0 .. MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Len: cardinal;
begin
  Len := high(Buf);
  if GetComputerName(Buf, Len) then
    Result := string(Buf)
  else
    Result := EmptyStr;
end;
{$ELSE}
begin
  Result := EmptyStr;
end;
{$ENDIF}

class function TLoggerUtils.Os: string;
begin
  Result := TOSVersion.toString;
end;

class function TLoggerUtils.ProcessId: Integer;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := GetCurrentProcessId;
{$ELSEIF DEFINED(LINUX)}
  Result := getpid;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

// {$IFDEF MACOS}
// function NSUserName: Pointer; cdecl; external '/System/Library/Frameworks/Foundation.framework/Foundation' name '_NSUserName';
// {$ENDIF}
//
class function TLoggerUtils.Username: string;
{$IF DEFINED(MSWINDOWS)}
var
  Buf: array [0 .. 2 * MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Len: cardinal;
begin
  Len := high(Buf);
  if GetUserName(Buf, Len) then
    Result := string(Buf)
  else
    Result := EmptyStr;
end;
// {$IFDEF MACOS}
// begin
// Result := TNSString.Wrap(NSUserName).UTF8String;
// end;
{$ELSE}
begin
  Result := '';
end;
{$ENDIF}

end.
