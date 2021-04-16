# DataLogger

DataLogger foi projetado para ser uma biblioteca simples com suporte a vários provedores.

### Doação

Se este projeto o ajudar a reduzir o tempo de desenvolvimento, você pode me dar uma xícara de café :) <a href="https://www.paypal.com/donate?hosted_button_id=2T7W4PL7YGJZW" target="_blank" rel="noopener noreferrer"><img width="150" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif"></a>


## Instalação

### Para instalar em seu projeto usando [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/dliocode/datalogger
```

### Instalação Manual


## Como usar

Existem duas maneiras diferentes de usar o DataLogger: 
Diretamente pelo ``` TLogger ``` ou instanciando seu próprio ``` TDataLogger ```.

O primeiro destina-se apenas a ser um log compartilhado conveniente para ser usado em todo o seu aplicativo, se você escolher.

### Provedores

Um provedor serve essencialmente para armazenar seus logs. Cada instância de um TDataLogger pode ter vários provedores configurados.

Aqui temos uma lista de todos os provedores dispoíveis:

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

### Using the Default Logger

```delphi
uses
  DataLogger,
  DataLogger.Console;

begin
  TLogger.AddProvider(TLoggerConsole.Create);

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
### Using custom logger

```delphi
uses
  DataLogger,
  DataLogger.Console;

var
	LCustomLogger: TDataLogger;
begin
	LCustomLogger := TDataLogger.Builder;

  TLogger.AddProvider(TLoggerConsole.Create);

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

## Options

### Providers

A provider is essentially for storing your logs. Each instance of a TDataLogger can have multiple providers configured at different levels.

For example, you might want the error logs to be stored in a remote, persistent location (such as a database), but all logs are sent to the console or a local file.

Define one provider at a time `TLogger.AddProvider()`
Define all providers at once `TLogger.SetProvider([])`

```delphi
  TLogger.AddProvider(<PROVIDER_1>);
  TLogger.SetProvider([<PROVIDER_1, PROVIDER_2, PROVIDER_3>]);
```
#### Found Providers

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


<hr>

### Types

`Trace, Debug, Info, Warn,  Success, Error, Fatal`

#### Sample:

```delphi
  TLogger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Success('My Success')
    .Error('My Error')
    .Fatal('My Fatal');
```

### Sample with Tag:

```delphi
  TLogger
    .Trace('My trace', 'MY_TAG')
    .Debug('My Debug', 'MY_TAG')
    .Info('My Info', 'MY_TAG')
    .Warn('My Warn', 'MY_TAG')
    .Success('My Success', 'MY_TAG')
    .Error('My Error', 'MY_TAG')
    .Fatal('My Fatal', 'MY_TAG');
```

<hr>

### Level

`Trace = 1, Debug = 2, Info = 3, Warn = 4, Success = 5, Error = 6, Fatal = 7`

Each `LoggerType` is given a specific integer priority. The higher the priority the
more important the message is considered to be.

DataLogger will output all levels after the chosen level, i.e if you choose
`LoggerType.Warn` DataLogger will output *Warn*, *Success*, *Error* and *Fatal* logs.

#### Sample:
```delphi
  TLogger.SetLogLevel(TLoggerType.Error);
```

<hr>

### Message Format

- Constant Types

```
uses 
	DataLogger.Types;

// Options
  LOG_TIMESTAMP
  LOG_THREADID
  LOG_PROCESSID
  LOG_TYPE
  LOG_TAG
  LOG_MESSAGE

  LOG_APPNAME
  LOG_APPVERSION
  LOG_APPPATH
  LOG_COMPUTERNAME
  LOG_USERNAME
  LOG_OSVERSION
```

- Defaut Message Format
```
  DEFAULT_LOG_FORMAT = LOG_TIMESTAMP + ' [TID ' + LOG_THREADID + '] [PID ' + LOG_PROCESSID + '] [' + LOG_TYPE + '] [' + LOG_TAG + '] ' + LOG_MESSAGE;
  
  TLogger.SetLogFormat(DEFAULT_LOG_FORMAT);  
```

#### Sample:

```delphi
uses
  DataLogger,
  DataLogger.Types,
  DataLogger.Console;

begin
  TLogger.AddProvider(TLoggerConsole.Create);

  TLogger.SetLogFormat(LOG_TIMESTAMP + ' - ' + LOG_MESSAGE);

  TLogger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Success('My Success')
    .Error('My Error')
    .Fatal('My Fatal');

  Readln;
end.
```
<hr>

### Only Types

You can activate only a few specific types.
Types that are not defined will be discarded.

```delphi
  TLogger.SetOnlyLogType([TLoggerType.Error]);
```

#### Sample:

```delphi
uses
  DataLogger,
  DataLogger.Types,
  DataLogger.Console;

begin
  TLogger.AddProvider(TLoggerConsole.Create);

  TLogger.SetOnlyLogType([TLoggerType.Error, TLoggerType.Warn]);

  TLogger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Success('My Success')
    .Error('My Error')
    .Fatal('My Fatal');

  Readln;
end.
```

### Disable Types

You can disable some specific types.
All messages of that type will be discarded.

```delphi
  TLogger.SetDisableLogType([TLoggerType.Debug]);
```

#### Sample:

```delphi
uses
  DataLogger,
  DataLogger.Types,
  DataLogger.Console;

begin
  TLogger.AddProvider(TLoggerConsole.Create);

  TLogger.SetDisableLogType([TLoggerType.Debug, TLoggerType.Info]);

  TLogger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Success('My Success')
    .Error('My Error')
    .Fatal('My Fatal');

  Readln;
end.
```
<hr>

### Exception

With `DataLogger` it is possible to capture and record exception events from your provider when saving.

```delphi
  TLogger.SetLogException(
    procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetryCount: Integer)
    begin

    end);
```

> Sender = Class that generated the error
> LogItem = Message information
> E = Exception information
> RetryCount = Number of retries information

#### Sample:

```delphi
uses
  DataLogger,
  DataLogger.Redis,
  System.SysUtils;

begin
  TLogger.AddProvider(TLoggerRedis.Create('127.0.0.1', 6380));

  TLogger.SetLogException(
    procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetryCount: Integer)
    begin
      Writeln(Format('Source: %s | MenssageException: %s | MessageLog: %s | RetryCount: %d', [Sender.ClassName, E.Message, LogItem.Message, RetryCount]));
    end);

  TLogger.Info('First message to Redis');

  Readln;
end.
```

## Providers

* [Providers](#providers)
  * [Console](#console)
  * [ElastSearch](#elasticsearch)
  * [Email](#email)
  * [EventLog](#eventlog)
  * [Events](#events)
  * [FileText](#filetext)
  * [ListBox](#listbox)
  * [ListView](#listview)
  * [Logstach](#Logstach)
  * [Memo](#Memo)
  * [OutputDebugString](#outputdebugstring)
  * [Redis](#redis)
  * [Rest](#rest)
  * [Slack](#slack)
  * [SysLog](#syslog)
  * [Telegram](#telegram)

### Console



### ElasticSearch

### Email

### EventLog

### Events

### FileText

### ListBox

### ListView

### Logstach

### Memo

### OutputDebugString

### Redis

### Rest

### Slack

### SysLog

### Telegram
