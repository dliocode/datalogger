{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2023 Danilo Lucas

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

// https://aws.amazon.com/cloudwatch
// https://docs.aws.amazon.com/cloudwatch/index.html
// https://docs.aws.amazon.com/general/latest/gr/create-signed-request.html

unit DataLogger.Provider.AWS.CloudWatch;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_AWS_CLOUDWATCH_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_AWS_CLOUDWATCH_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.DateUtils, System.Hash;

type
  TProviderAWSCloudWatch = class(TDataLoggerProvider<TProviderAWSCloudWatch>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_AWS_CLOUDWATCH_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_AWS_CLOUDWATCH_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  strict private
    FHTTP: TProviderHTTP;
    FServiceName: string;
    FAccessKey: string;
    FSecretKey: string;
    FRegion: string;
    FGroupName: string;
    FStreamName: string;
    function CalculateSignature(const AContent: string; const ADateStamp: string): string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function AccessKey(const AValue: string): TProviderAWSCloudWatch;
    function SecretKey(const AValue: string): TProviderAWSCloudWatch;
    function Region(const AValue: string): TProviderAWSCloudWatch;
    function GroupName(const AValue: string): TProviderAWSCloudWatch;
    function StreamName(const AValue: string): TProviderAWSCloudWatch;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

type
  TAWSSigner = class
  strict private
    FAccessKey: string;
    FSecretKey: string;
    FRegion: string;
    FServiceName: string;
    FHost: string;
    FContentType: string;
    FDateAMZ: string;
    FDateStamp: string;
    FMethod: string;
    FPayload: string;
    FQueryString: string;
  public
    property AccessKey: string read FAccessKey write FAccessKey;
    property SecretKey: string read FSecretKey write FSecretKey;
    property Region: string read FRegion write FRegion;
    property ServiceName: string read FServiceName write FServiceName;
    property Host: string read FHost write FHost;
    property ContentType: string read FContentType write FContentType;
    property DateAMZ: string read FDateAMZ write FDateAMZ;
    property DateStamp: string read FDateStamp write FDateStamp;
    property Method: string read FMethod write FMethod;
    property Payload: string read FPayload write FPayload;
    property QueryString: string read FQueryString write FQueryString;

    function CalculateSignature: string;
  end;

  { TProviderAWSCloudWatch }

constructor TProviderAWSCloudWatch.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP
    .ContentType('application/x-amz-json-1.1')
    .Method(TRESTMethod.tlmPost);

  FServiceName := 'logs';
end;

procedure TProviderAWSCloudWatch.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderAWSCloudWatch.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderAWSCloudWatch.AccessKey(const AValue: string): TProviderAWSCloudWatch;
begin
  Result := Self;
  FAccessKey := AValue.Trim;
end;

function TProviderAWSCloudWatch.SecretKey(const AValue: string): TProviderAWSCloudWatch;
begin
  Result := Self;
  FSecretKey := AValue.Trim;
end;

function TProviderAWSCloudWatch.Region(const AValue: string): TProviderAWSCloudWatch;
begin
  Result := Self;
  FRegion := AValue.Trim.ToLower;
end;

function TProviderAWSCloudWatch.GroupName(const AValue: string): TProviderAWSCloudWatch;
begin
  Result := Self;
  FGroupName := AValue.Trim;
end;

function TProviderAWSCloudWatch.StreamName(const AValue: string): TProviderAWSCloudWatch;
begin
  Result := Self;
  FStreamName := AValue.Trim;
end;

procedure TProviderAWSCloudWatch.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
begin
  if AJSON.Trim.IsEmpty then
    Exit;

  try
    LJO := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  except
    on E: Exception do
      Exit;
  end;

  if not Assigned(LJO) then
    Exit;

  try
    AccessKey(LJO.GetValue<string>('access_key', FAccessKey));
    SecretKey(LJO.GetValue<string>('secret_key', FSecretKey));
    Region(LJO.GetValue<string>('region', FRegion));
    GroupName(LJO.GetValue<string>('group_name', FGroupName));
    StreamName(LJO.GetValue<string>('stream_name', FStreamName));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderAWSCloudWatch.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('access_key', TJSONString.Create(FAccessKey));
    LJO.AddPair('secret_key', TJSONString.Create(FSecretKey));
    LJO.AddPair('region', TJSONString.Create(FRegion));
    LJO.AddPair('group_name', TJSONString.Create(FGroupName));
    LJO.AddPair('stream_name', TJSONString.Create(FStreamName));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderAWSCloudWatch.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LNow: TDateTime;
  LItem: TLoggerItem;
  LLog: string;
  LAuthorization: string;
  LJO: TJSONObject;
  LJA: TJSONArray;
  LJOEvents: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  LNow := TTimeZone.Local.ToUniversalTime(Now);

  FHTTP
    .URL(Format('https://%s.%s.amazonaws.com', [FServiceName, FRegion]))
    .AddHeader('X-Amz-Target', 'Logs_20140328.PutLogEvents')
    .AddHeader('X-Amz-Date', FormatDateTime('yyyymmdd', LNow) + 'T' + FormatDateTime('hhnnss', LNow) + 'Z');

  LJO := TJSONObject.Create;
  try
    LJA := TJSONArray.Create;
    LJO
      .AddPair('logGroupName', TJSONString.Create(FGroupName))
      .AddPair('logStreamName', TJSONString.Create(FStreamName))
      .AddPair('logEvents', LJA);

    for LItem in ACache do
    begin
      if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
        Continue;

      LLog := SerializeItem.LogItem(LItem).ToJSON;

      LJOEvents := TJSONObject.Create;
      LJA.Add(LJOEvents);

      LJOEvents
        .AddPair('timestamp', TJSONNumber.Create(LItem.TimeStampUNIX * 1000))
        .AddPair('message', TJSONString.Create(LLog));
    end;

    LLog := LJO.ToString;
  finally
    LJO.Free;
  end;

  LAuthorization := CalculateSignature(LLog, FormatDateTime('yyyymmdd', Now));

  LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
  LLogItemREST.LogItem := LItem;
  LLogItemREST.URL := '';
  LLogItemREST.Header := [TLogHeader.Create('Authorization', LAuthorization)];

  LItemREST := [LLogItemREST];

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries)
    .WaitTimeoutToSend(170);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

function TProviderAWSCloudWatch.CalculateSignature(const AContent: string; const ADateStamp: string): string;
var
  LAWSSigner: TAWSSigner;
begin
  LAWSSigner := TAWSSigner.Create;
  try
    LAWSSigner.AccessKey := FAccessKey;
    LAWSSigner.SecretKey := FSecretKey;
    LAWSSigner.Region := FRegion;
    LAWSSigner.ServiceName := FServiceName;
    LAWSSigner.Host := FHTTP.URL.Replace('https://', '').Replace('http://', '');
    LAWSSigner.ContentType := FHTTP.ContentType;
    LAWSSigner.DateAMZ := FHTTP.Header('X-Amz-Date');
    LAWSSigner.DateStamp := ADateStamp;
    LAWSSigner.Method := FHTTP.MethodString;
    LAWSSigner.Payload := AContent;
    LAWSSigner.QueryString := '';

    Result := LAWSSigner.CalculateSignature;
  finally
    LAWSSigner.Free;
  end;
end;

{ TAWSSignature }

function TAWSSigner.CalculateSignature: string;
  function Hash(const AData: string): string;
  begin
    Result := THashSHA2.GetHashString(AData, THashSHA2.TSHA2Version.SHA256);
  end;

  function SignHMAC(const AKey: TBytes; const AData: TBytes): TBytes;
  begin
    Result := THashSHA2.GetHMACAsBytes(AData, AKey, THashSHA2.TSHA2Version.SHA256);
  end;

  function SignatureKey(const AData: string): TBytes;
  var
    LDateKey: TBytes;
    LDateRegionKey: TBytes;
    LDateRegionServiceKey: TBytes;
    LSignatureKey: TBytes;
  begin
    LDateKey := SignHMAC(TEncoding.UTF8.GetBytes('AWS4' + FSecretKey), TEncoding.UTF8.GetBytes(AData));
    LDateRegionKey := SignHMAC(LDateKey, TEncoding.UTF8.GetBytes(FRegion));
    LDateRegionServiceKey := SignHMAC(LDateRegionKey, TEncoding.UTF8.GetBytes(FServiceName));
    LSignatureKey := SignHMAC(LDateRegionServiceKey, TEncoding.UTF8.GetBytes('aws4_request'));

    Result := LSignatureKey;
  end;

var
  LCanonicalMethod: string;
  LCanonicalURI: string;
  LCanonicalQueryString: string;
  LCanonicalHeaders: string;
  LCanonicalSignedHeaders: string;
  LSignaturePayload: string;
  LCanonicalRequest: string;
  LSignatureCanonical: string;
  LSignAlgorithm: string;
  LSignCredentialScope: string;
  LSignToSign: string;
  LSigningKey: TBytes;
  LSignature: string;
begin
  // CREATE A CANONICAL REQUEST
  LCanonicalMethod := FMethod;
  LCanonicalURI := '/';
  LCanonicalQueryString := FQueryString;
  LCanonicalHeaders := 'content-type:' + FContentType + #10 + 'host:' + FHost + #10 + 'x-amz-date:' + FDateAMZ + #10;
  LCanonicalSignedHeaders := 'content-type;host;x-amz-date';
  LSignaturePayload := Hash(FPayload);
  LCanonicalRequest := LCanonicalMethod + #10 + LCanonicalURI + #10 + LCanonicalQueryString + #10 + LCanonicalHeaders + #10 + LCanonicalSignedHeaders + #10 + LSignaturePayload;
  LSignatureCanonical := Hash(LCanonicalRequest);

  // CREATE THE STRING TO SIGN
  LSignAlgorithm := 'AWS4-HMAC-SHA256';
  LSignCredentialScope := Format('%s/%s/%s/aws4_request', [FDateStamp, FRegion, FServiceName]);
  LSignToSign := LSignAlgorithm + #10 + FDateAMZ + #10 + LSignCredentialScope + #10 + LSignatureCanonical;

  // CALCULATE THE SIGNATURE
  LSigningKey := SignatureKey(FDateStamp);
  LSignature := THash.DigestAsString(SignHMAC(LSigningKey, TEncoding.UTF8.GetBytes(LSignToSign)));

  // AUTHORIZATION
  Result := Format('%s Credential=%s/%s, SignedHeaders=%s, Signature=%s', [LSignAlgorithm, FAccessKey, LSignCredentialScope, LCanonicalSignedHeaders, LSignature]);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderAWSCloudWatch);

end.
