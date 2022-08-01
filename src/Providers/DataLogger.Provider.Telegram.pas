{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

//

unit DataLogger.Provider.Telegram;

interface

uses
{$IF DEFINED(DATALOGGER_TELEGRAM_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_TELEGRAM_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.NetEncoding, System.JSON, System.TypInfo;

type
{$SCOPEDENUMS ON}
  TTelegramParseMode = (tpNone, tpHTML, tpMarkdown);
{$SCOPEDENUMS OFF}

{$IF DEFINED(DATALOGGER_TELEGRAM_USE_INDY)}
  TProviderTelegram = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_TELEGRAM_USE_NETHTTPCLIENT)}
  TProviderTelegram = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderTelegram = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
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

    constructor Create; overload;
    constructor Create(const ABotToken: string; const AChatId: string; const AParseMode: TTelegramParseMode = TTelegramParseMode.tpMarkdown); overload; deprecated 'Use TProviderTelegram.Create.BotToken(''AAAAA'').ChatId(''00000000'').ParseMode(tpMarkdown) - This function will be removed in future versions';
  end;

implementation

{ TProviderTelegram }

const
  TELEGRAM_API_SENDMSG = 'https://api.telegram.org/bot%s/sendMessage?chat_id=%s&text=%s';
  TELEGRAM_API_UPDATE = 'https://api.telegram.org/bot%s/getUpdates'; // https://api.telegram.org/bot<TOKEN>/getUpdates
  TELEGRAM_API_MARKDOWN = '&parse_mode=MarkdownV2';
  TELEGRAM_API_HTML = '&parse_mode=HTML';

constructor TProviderTelegram.Create;
begin
  inherited Create;

  ContentType('application/json');
  BotToken('');
  ChatId('');
  ParseMode(TTelegramParseMode.tpMarkdown);
end;

constructor TProviderTelegram.Create(const ABotToken: string; const AChatId: string; const AParseMode: TTelegramParseMode = TTelegramParseMode.tpMarkdown);
begin
  Create;

  BotToken(ABotToken);
  ChatId(AChatId);
  ParseMode(AParseMode);
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
    LJO.AddPair('bot_token', FBotToken);
    LJO.AddPair('chat_id', FChatId);
    LJO.AddPair('parse_mode', GetEnumName(TypeInfo(TTelegramParseMode), Integer(FParseMode)));

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
  LMessage: string;
  LParseMode: string;

  procedure SerializeMessageParseMode;
  const
    FormattingMarkdown: array [0 .. 17] of string = ('_', '*', '[', ']', '(', ')', '~', '`', '>', '#', '+', '-', '=', '|', '{', '}', '.', '!');
  var
    I: Integer;
  begin
    case FParseMode of
      TTelegramParseMode.tpHTML:
        begin
          LParseMode := TELEGRAM_API_HTML;

          case LItem.&Type of
            TLoggerType.All:
              ;
            TLoggerType.Trace:
              ;
            TLoggerType.Debug:
              ;
            TLoggerType.Info:
              ;
            TLoggerType.Success:
              ;
            TLoggerType.Warn:
              LMessage := '<u>' + LMessage + '</u>';

            TLoggerType.Error, TLoggerType.Fatal:
              LMessage := '<b>' + LMessage + '</b>';

            TLoggerType.Custom:
              ;
          end;
        end;

      TTelegramParseMode.tpMarkdown:
        begin
          LParseMode := TELEGRAM_API_MARKDOWN;

          // https://core.telegram.org/bots/api#formatting-options
          for I := Low(FormattingMarkdown) to High(FormattingMarkdown) do
            LMessage := LMessage.Replace(FormattingMarkdown[I], '\' + FormattingMarkdown[I]);

          case LItem.&Type of
            TLoggerType.All:
              ;
            TLoggerType.Trace:
              ;
            TLoggerType.Debug:
              ;
            TLoggerType.Info:
              ;
            TLoggerType.Success:
              ;
            TLoggerType.Warn:
              LMessage := '__' + LMessage + '__';

            TLoggerType.Error, TLoggerType.Fatal:
              LMessage := '*' + LMessage + '*';

            TLoggerType.Custom:
              ;
          end;
        end;
    end;
  end;

begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LParseMode := '';
    LMessage := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp).Trim;

    if FParseMode <> TTelegramParseMode.tpNone then
      SerializeMessageParseMode;

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;

    LMessage := TNetEncoding.URL.Encode(LMessage);
    LLogItemREST.URL := Format(TELEGRAM_API_SENDMSG + LParseMode, [FBotToken, FChatId, LMessage]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmGet, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderTelegram);

end.
