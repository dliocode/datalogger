unit DataLogger.Transaction;

interface

uses
  DataLogger.Types,
  System.Generics.Collections;

type
  TDataLoggerTransaction = class
  public
    InTransaction: Boolean;
    ListItemTransaction: TDataLoggerListItemTransaction;

    destructor Destroy; override;
  end;

  TDataLoggerListTransaction = TObjectDictionary<string, TDataLoggerTransaction>;

implementation

{ TDataLoggerTransaction }

destructor TDataLoggerTransaction.Destroy;
begin
  if Assigned(ListItemTransaction) then
  begin
    ListItemTransaction.Free;
    ListItemTransaction := nil;
  end;

  inherited;
end;

end.
