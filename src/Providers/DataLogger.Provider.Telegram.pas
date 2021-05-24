{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************

  https://api.telegram.org/bot<TOKEN>/getUpdates
}

unit DataLogger.Provider.Telegram;

interface

uses
  DataLogger.Provider.REST.HTTPClient, DataLogger.Types,
  idURI,
  System.SysUtils, System.StrUtils;

type
  TProviderTelegram = class(TProviderRESTHTTPClient)
  private
    FBotToken: string;
    FChatId: Double;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const ABotToken: string; const AChatId: Double); reintroduce;
  end;

implementation

{ TProviderTelegram }

const
  TELEGRAM_API_SENDMSG = 'https://api.telegram.org/bot%s/sendMessage?chat_id=%f&text=%s';
  TELEGRAM_API_UPDATE = 'https://api.telegram.org/bot%s/getUpdates';
  TELGRAM_API_MARKDOWN = '&parse_mode=markdown';

constructor TProviderTelegram.Create(const ABotToken: string; const AChatId: Double);
begin
  FBotToken := ABotToken;
  FChatId := AChatId;

  inherited Create('', 'application/json');
end;

procedure TProviderTelegram.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
  LMessage: string;
  LURLMarkdown: string;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LURLMarkdown := '';
    LMessage := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatSettings);

    if not MatchText(LMessage.Trim, ['_', '*']) then
    begin
      LURLMarkdown := TELGRAM_API_MARKDOWN;

      case LItem.&Type of
        All:
          ;
        Trace:
          ;
        Debug:
          ;
        Info:
          ;
        Success:
          ;
        Warn:
          ;
        Error, Fatal:
          LMessage := '*' + LMessage.Trim + '*';
      end;
    end;

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := TIdURI.URLEncode(Format(TELEGRAM_API_SENDMSG + LURLMarkdown, [FBotToken, FChatId, LMessage]));
    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TLoggerMethod.tlmGet, LItemREST);
end;

end.
