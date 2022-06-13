program ProviderListBox;

uses
  Vcl.Forms,
  UProviderListBox in 'UProviderListBox.pas' {Form2},
  DataLogger in '..\..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Types in '..\..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.ListBox in '..\..\..\src\Providers\DataLogger.Provider.ListBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
