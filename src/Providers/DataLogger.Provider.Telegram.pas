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

// https://api.telegram.org/bot<TOKEN>/getUpdates

unit DataLogger.Provider.Telegram;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_TELEGRAM_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_TELEGRAM_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.JSON, System.TypInfo, System.Classes;

type
{$SCOPEDENUMS ON}
  TTelegramParseMode = (tpNone, tpHTML, tpMarkdown);
{$SCOPEDENUMS OFF}

  TProviderTelegram = class(TDataLoggerProvider<TProviderTelegram>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_TELEGRAM_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_TELEGRAM_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FBotToken: string;
    FChatId: string;
    FParseMode: TTelegramParseMode;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function BotToken(const AValue: string): TProviderTelegram;
    function ChatId(const AValue: string): TProviderTelegram;
    function ParseMode(const AValue: TTelegramParseMode): TProviderTelegram;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

type
  TProviderTelegramHelper = record helper for TTelegramParseMode
  public
    function ToString: string;
  end;

  { TProviderTelegram }

const
  TELEGRAM_API_UPDATE = 'https://api.telegram.org/bot%s/getUpdates'; // https://api.telegram.org/bot<TOKEN>/getUpdates

constructor TProviderTelegram.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  BotToken('');
  ChatId('');
  ParseMode(TTelegramParseMode.tpMarkdown);
end;

destructor TProviderTelegram.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderTelegram.BotToken(const AValue: string): TProviderTelegram;
begin
  Result := Self;
  FBotToken := AValue;
end;

function TProviderTelegram.ChatId(const AValue: string): TProviderTelegram;
begin
  Result := Self;
  FChatId := AValue;
end;

function TProviderTelegram.ParseMode(const AValue: TTelegramParseMode): TProviderTelegram;
begin
  Result := Self;
  FParseMode := AValue;
end;

procedure TProviderTelegram.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
  LValue: string;
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
    BotToken(LJO.GetValue<string>('bot_token', FBotToken));
    ChatId(LJO.GetValue<string>('chat_id', FChatId));

    LValue := GetEnumName(TypeInfo(TTelegramParseMode), Integer(FParseMode));
    ParseMode(TTelegramParseMode(GetEnumValue(TypeInfo(TTelegramParseMode), LJO.GetValue<string>('parse_mode', LValue))));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderTelegram.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('bot_token', TJSONString.Create(FBotToken));
    LJO.AddPair('chat_id', TJSONString.Create(FChatId));
    LJO.AddPair('parse_mode', TJSONString.Create(GetEnumName(TypeInfo(TTelegramParseMode), Integer(FParseMode))));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderTelegram.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
  LLog: string;
  LJO: TJSONObject;

  procedure SerializeMessageParseMode;
  const
    FormattingMarkdown: array [0 .. 17] of string = ('_', '*', '[', ']', '(', ')', '~', '`', '>', '#', '+', '-', '=', '|', '{', '}', '.', '!');
  var
    I: Integer;
  begin
    case FParseMode of
      TTelegramParseMode.tpHTML:
        begin
          case LItem.Level of
            TLoggerLevel.All:
              ;
            TLoggerLevel.Trace:
              ;
            TLoggerLevel.Debug:
              ;
            TLoggerLevel.Info:
              ;
            TLoggerLevel.Success:
              ;
            TLoggerLevel.Warn:
              LLog := '<u>' + LLog + '</u>';

            TLoggerLevel.Error, TLoggerLevel.Fatal:
              LLog := '<b>' + LLog + '</b>';

            TLoggerLevel.Custom:
              ;
          end;
        end;

      TTelegramParseMode.tpMarkdown:
        begin
          // https://core.telegram.org/bots/api#formatting-options
          for I := Low(FormattingMarkdown) to High(FormattingMarkdown) do
            LLog := LLog.Replace(FormattingMarkdown[I], '\' + FormattingMarkdown[I]);

          case LItem.Level of
            TLoggerLevel.All:
              ;
            TLoggerLevel.Trace:
              ;
            TLoggerLevel.Debug:
              ;
            TLoggerLevel.Info:
              ;
            TLoggerLevel.Success:
              ;
            TLoggerLevel.Warn:
              LLog := '__' + LLog + '__';

            TLoggerLevel.Error, TLoggerLevel.Fatal:
              LLog := '*' + LLog + '*';

            TLoggerLevel.Custom:
              ;
          end;
        end;
    end;
  end;

begin
  LItemREST := [];

  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    LLog := LLog.Replace('\', '\\');
    if (FParseMode <> TTelegramParseMode.tpNone) then
      SerializeMessageParseMode;

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('chat_id', TJSONString.Create(FChatId));
      LJO.AddPair('text', TJSONString.Create(LLog));
      LJO.AddPair('parse_mode', TJSONString.Create(FParseMode.ToString));

{$IF CompilerVersion > 32} // 32 = Delphi Tokyo (10.2)
      LLog := LJO.ToString;
{$ELSE}
      LLog := LJO.ToJSON;
{$ENDIF}
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.telegram.org/bot%s/sendMessage', [FBotToken]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

{ TProviderTelegramHelper }

function TProviderTelegramHelper.ToString: string;
begin
  case Self of
    TTelegramParseMode.tpNone:
      Result := 'HTML';
    TTelegramParseMode.tpHTML:
      Result := 'HTML';
    TTelegramParseMode.tpMarkdown:
      Result := 'MarkdownV2';
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderTelegram);

end.
