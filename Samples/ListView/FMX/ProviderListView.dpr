program ProviderListView;

uses
  System.StartUpCopy,
  FMX.Forms,
  UProviderListView in 'UProviderListView.pas' {Form2},
  DataLogger in '..\..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Types in '..\..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.ListView in '..\..\..\src\Providers\DataLogger.Provider.ListView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
