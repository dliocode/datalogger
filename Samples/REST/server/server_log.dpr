program server_log;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse;

begin
  THorse.Routes
    .Prefix('/api');

  THorse.Post('/log1',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('/log1 -> ' + Req.Body);
      Res.Status(THTTPStatus.Ok).Send('OK');
    end);

  THorse.Post('/log2',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('/log2 -> ' + Req.Body);
      Res.Status(THTTPStatus.Ok).Send('OK');
    end);

  THorse.Post('/log3',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Writeln('/log3 -> ' + Req.Body);
      Res.Status(THTTPStatus.Ok).Send('OK');
    end);

  THorse.Listen(9000);
end.
