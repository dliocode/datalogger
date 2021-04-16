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
  System.SysUtils;

type
  TChannelType = (tctPublic, tctPrivate);

  TProviderTelegram = class(TProviderRESTHTTPClient)
  private
    FBotToken: string;
    FChatId: Integer;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const ABotToken: string; const AChatId: Integer); reintroduce;
  end;

implementation

{ TProviderTelegram }

const
  TELEGRAM_API_SENDMSG = 'https://api.telegram.org/bot%s/sendMessage?chat_id=%d&text=%s&parse_mode=markdown';
  TELEGRAM_API_UPDATE = 'https://api.telegram.org/bot%s/getUpdates';

constructor TProviderTelegram.Create(const ABotToken: string; const AChatId: Integer);
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

    LMessage := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatSettings);

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

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := TIdURI.URLEncode(Format(TELEGRAM_API_SENDMSG, [FBotToken, FChatId, LMessage]));
    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TLoggerMethod.tlmGet, LItemREST);
end;

end.
