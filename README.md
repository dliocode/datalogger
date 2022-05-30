<p align="center">
  <a href="https://user-images.githubusercontent.com/54585337/123354958-0f374800-d53b-11eb-8a2b-9c7041cfff47.png">
    <img alt="datalogger" src="https://user-images.githubusercontent.com/54585337/123354958-0f374800-d53b-11eb-8a2b-9c7041cfff47.png">
  </a>  
</p>
<br>
<p align="center">
  <img src="https://img.shields.io/github/v/release/dliocode/datalogger?style=flat-square">
  <img src="https://img.shields.io/github/stars/dliocode/datalogger?style=flat-square">
  <img src="https://img.shields.io/github/forks/dliocode/datalogger?style=flat-square">
  <img src="https://img.shields.io/github/contributors/dliocode/datalogger?color=orange&style=flat-square">
  <img src="https://tokei.rs/b1/github/dliocode/datalogger?color=red&category=lines">
  <img src="https://tokei.rs/b1/github/dliocode/datalogger?color=green&category=code">
  <img src="https://tokei.rs/b1/github/dliocode/datalogger?color=yellow&category=files">
</p>

# DataLogger

DataLogger foi projetado para ser uma biblioteca simples de log com suporte a vários _providers_.

Support: developer.dlio@gmail.com

## Instalação

### Para instalar em seu projeto usando [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/dliocode/datalogger
```

## Como usar

Existem duas maneiras diferentes de usar o DataLogger: 
Diretamente pelo ``` Logger ``` ou instanciando seu próprio ``` TDataLogger ```.

O primeiro destina-se apenas a ser um log compartilhado conveniente para ser usado em todo o seu aplicativo, se você escolher.

* **Uses necessária**: 
```
uses DataLogger;
``` 

### Provider

Um **_provider_** serve essencialmente para armazenar seus logs. 
Cada instância de um TDataLogger pode ter vários **_providers_** configurados.

Aqui temos uma lista de todos os _providers_ disponíveis:

|  Nome | Uses  |
| ------------ | ------------ |
| Console | DataLogger.Provider.Console |
| [ElasticSearch](https://www.elastic.co/pt/what-is/elasticsearch) | DataLogger.Provider.ElasticSearch |
| Email | DataLogger.Provider.Email |
| EventLog | DataLogger.Provider.EventLog |
| Events | DataLogger.Provider.Events |
| ListBox | DataLogger.Provider.ListBox |
| ListView | DataLogger.Provider.ListView |
| [Logstach](https://www.elastic.co/pt/logstash/) | DataLogger.Provider.Logstach |
| [MatterMost](https://mattermost.com/) | DataLogger.Provider.Mattermost |
| Memo | DataLogger.Provider.Memo |
| Memory | DataLogger.Provider.Memory |
| OutputDebugString | DataLogger.Provider.OutputDebugString |
| [Redis](https://redis.io/) | DataLogger.Provider.Redis |
| Rest | DataLogger.Provider.REST.HTTPClient<br />DataLogger.Provider.REST.Indy<br />DataLogger.Provider.REST.NetHTTPClient  |
| [SendEmail](https://github.com/dliocode/sendemail) | DataLogger.Provider.SendEmail |
| [Slack](https://slack.com/) | DataLogger.Provider.Slack |
| SysLog | DataLogger.Provider.SysLog |
| [Telegram](https://core.telegram.org/) | DataLogger.Provider.Telegram |
| TextFile | DataLogger.Provider.TextFile |

## Exemplos

### Uso Padrão

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  Logger
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

### Formato do Log

Por padrão o DataLogger possui um formato de log padrão: 
```
TLoggerFormat.DEFAULT_LOG_FORMAT = 
LOG_TIMESTAMP + ' [TID ' + LOG_THREADID + '] [PID ' + LOG_PROCESSID + '] [SEQ ' + LOG_SEQUENCE + '] [' + LOG_TYPE + '] [' + LOG_TAG + '] ' + LOG_MESSAGE;
```

#### Como definir um formato

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Quero exibir: [ Data e Hora ] [ Tipo ] [ Tag ] - Mensagem

  Logger.SetLogFormat(Format('[ %s ] [ %s ] [ %s ] - %s ',[TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_TYPE, TLoggerFormat.LOG_TAG, TLoggerFormat.LOG_MESSAGE]));

  Logger.Info('Minha mensagem no Log','CLASS_PRINCIPAL');
  
  // output: [ 2021-05-19 08:15:59:600 ] [ Info ] [ CLASS_PRINCIPAL ] - Minha mensagem no Log

  Readln;
end.
```

#### Como mudar o formato do Timestamp
* Formato padrão: `yyyy-mm-dd hh:mm:ss:zzz`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Quero exibir: [ Data e Hora ] [ Tipo ] [ Tag ] - Mensagem

  Logger.SetLogFormat(Format('[ %s ] [ %s ] [ %s ] - %s ',[TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_TYPE, TLoggerFormat.LOG_TAG, TLoggerFormat.LOG_MESSAGE]));

  // Definindo o formato do Timestamp
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss');

  Logger.Info('Minha mensagem no Log','CLASS_PRINCIPAL');

  // output: [ 19/05/2021 08:15:59 ] [ Info ] [ CLASS_PRINCIPAL ] - Minha mensagem no Log

  Readln;
end.
```

#### Tipos de Tag para criar o formato do log
* Comum 

```
// Exibe o nome do log.
// ex: Logger.SetName('SERVICE_REMOTE')
TLoggerFormat.LOG_NAME;

// Exibe a sequencia que o log foi gerado.
TLoggerFormat.LOG_SEQUENCE;

// Exibe a data e hora que foi gerado.
TLoggerFormat.LOG_TIMESTAMP;

// Exibe o Id da thread atual.
TLoggerFormat.LOG_THREADID;

// Exibe o id do processo do app.
TLoggerFormat.LOG_PROCESSID;

// Exibe o tipo do log, sendo eles: TRACE/DEBUG/INFO/SUCCESS/WARN/ERROR/FATAL
TLoggerFormat.LOG_TYPE;

// Exibe a tag do log, essa informação é preenchida a após a mensagem;
// Ex: Logger.Debug('Minha mensagem','Minha Tag');
TLoggerFormat.LOG_TAG;

// Exibe a mensagem do log, sem essa tag a mensagem não é exibida.
// Ex: Logger.Debug('Minha mensagem');
TLoggerFormat.LOG_MESSAGE;
```

* Especiais:

```
// Exibe o nome do app.
TLoggerFormat.LOG_APPNAME;

// Exibe a versão do app.
TLoggerFormat.LOG_APPVERSION;

// Exibe o diretório do app.
TLoggerFormat.LOG_APPPATH

// Exibe o tamanho do app em MB.
TLoggerFormat.LOG_APPSIZE

// Exibe o nome do computador.
TLoggerFormat.LOG_COMPUTERNAME;

// Exibe o nome do usuário do Sistema Operacional.
TLoggerFormat.LOG_USERNAME

// Exibe as informações do Sistema Operacional.
TLoggerFormat.LOG_OSVERSION

// Exibe o IP Local.
TLoggerFormat.LOG_IP_LOCAL

```

### **DOCUMENTAÇÃO EM ANDAMENTO**
