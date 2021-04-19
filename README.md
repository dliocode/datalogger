# DataLogger

DataLogger foi projetado para ser uma biblioteca simples com suporte a vários _providers_.

### Doação

Se este projeto o ajudar a reduzir o tempo de desenvolvimento, você pode me dar uma xícara de café :) <a href="https://www.paypal.com/donate?hosted_button_id=2T7W4PL7YGJZW" target="_blank" rel="noopener noreferrer"><img width="150" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif"></a>


## Instalação

### Para instalar em seu projeto usando [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/dliocode/datalogger
```

## Como usar

Existem duas maneiras diferentes de usar o DataLogger: 
Diretamente pelo ``` TLogger ``` ou instanciando seu próprio ``` TDataLogger ```.

O primeiro destina-se apenas a ser um log compartilhado conveniente para ser usado em todo o seu aplicativo, se você escolher.

### Provider

Um _provider_ serve essencialmente para armazenar seus logs. Cada instância de um TDataLogger pode ter vários _providers_ configurados.

Aqui temos uma lista de todos os _providers_ disponpiveis:

Console 
ElasticSearch 
Email
EventLog
Events
FileText
ListBox
ListView
Logstach
Memo
OutputDebugString
Redis
Rest
Slack
SysLog
Telegram


### Exemplos

### Uso Padrão

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  TLogger.AddProvider(TProviderConsole.Create);

  TLogger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal');

  Readln;
end.
```
### Criar uma nova instância do DataLogger

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

var
  LCustomLogger: TDataLogger;
begin
  LCustomLogger := TDataLogger.Builder;
  LCustomLogger.AddProvider(TProviderConsole.Create);

  LCustomLogger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal');

  Readln;
end.
```

### **DOCUMENTAÇÃO EM ANDAMENTO**
