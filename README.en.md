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

  <p align="center">
 |
    <a href="https://github.com/dliocode/datalogger/blob/v3/README.md">Português Brasileiro</a> |  
    <a href="https://github.com/dliocode/datalogger/blob/v3/README.en.md">English</a> |
  </p>
</p>

# DataLogger

DataLogger was designed to be a simple logging library with support for many *providers*.

Support: developer.dlio@gmail.com

## ⚙️ Installation

### To install in your project using [boss](https://github.com/HashLoad/boss):

```sh
$ boss install github.com/dliocode/datalogger
```

### Manual Installation

Add the following folders to your project, under *Project > Options > Delphi Compiler > Search path*

    ../src/Core
    ../src/Providers

## 📌 Index

*   [How to use](#como-usar)
*   [Providers](#providers)
*   [Dependencies](#dependências)
*   [Extra Information](#informações-extras)
*   [Examples](#exemplos)
    *   [Standard Use](#uso-padrão)
    *   [Create a new DataLogger instance](#criar-uma-nova-instância-do-datalogger)
    *   [DataLogger Simple](#datalogger-simple)
    *   [Custom](#custom)
    *   [Specials](#especiais)
        *   [SlineBreak](#slinebreak)
        *   [UndoLast](#undolast)
    *   [Template (Log Format)](#template-formato-do-log)
        *   [Constants](#template-constantes)
    *   [SetTemplate](#settemplate)
        *   [How to define a template in specific providers](#como-definir-um-template-em-providers-específicos)
    *   [SetFormatTimestamp](#setformattimestamp)
    *   [Level](#level)
    *   [SetLevelName](#setlevelname)
    *   [SetLevel](#setlevel)
    *   [SetDisableLevel](#setdisablelevel)
    *   [SetOnlyLevel](#setonlylevel)
    *   [SetLogException](#setlogexception)
    *   [SetMaxRetries](#setmaxretries)
    *   [SetIgnoreTemplate](#setignoretemplate)
    *   [SetName](#setname)
    *   [SetLiveMode](#setlivemode)
    *   [Transaction](#transaction)

## How to use

There are two different ways to use DataLogger:
directly through `Logger` or instantiating your own `TDataLogger`.

[Samples](https://github.com/dliocode/datalogger/tree/main/Samples)

The first is just intended to be a convenient shared log to use across your application if you choose.

*   **Uses required**:

<!---->

    uses DataLogger;

## Providers

One ***provider*** it essentially serves to store your logs.
Each instance of a TDataLogger can have several ***providers*** configured.

Here we have a list with **60 *providers*** available:

| Name | Uses | Examples |
| --- | --- | --- |
| [AWS CloudWatch](https://aws.amazon.com/cloudwatch) | DataLogger.Provider.AWS.CloudWatch | [AWS CloudWatch](https://github.com/dliocode/datalogger/tree/main/Samples/AWSCloudWatch) |
| [Axiom](https://axiom.co/) | DataLogger.Provider.Axiom | [Axiom](https://github.com/dliocode/datalogger/tree/main/Samples/Axiom) |
| [CallMeBot](https://www.callmebot.com/) | DataLogger.Provider.CallMeBot.WhatsApp | [CallMeBot](https://github.com/dliocode/datalogger/tree/main/Samples/CallMeBot) |
| Console | DataLogger.Provider.Console | [Console](https://github.com/dliocode/datalogger/tree/main/Samples/Console) <br /> [Console Simple](https://github.com/dliocode/datalogger/tree/main/Samples/Console%20-%20Simple) |
| [Coralogix](https://coralogix.com/) | DataLogger.Provider.Coralogix | [Coralogix](https://github.com/dliocode/datalogger/tree/main/Samples/Coralogix) |
| CSV | DataLogger.Provider.CSV | [CSV](https://github.com/dliocode/datalogger/tree/main/Samples/CSV) |
| [Datadog](https://www.datadoghq.com/) | DataLogger.Provider.Datadog | [Datadog](https://github.com/dliocode/datalogger/tree/main/Samples/Datadog) |
| [Datalust](https://datalust.co/) | DataLogger.Provider.Datalust | [Datalust](https://github.com/dliocode/datalogger/tree/main/Samples/Datalust) |
| [Discord](https://discord.com/developers/docs/resources/webhook) | DataLogger.Provider.Discord.WebHook | [DiscordHook](https://github.com/dliocode/datalogger/tree/main/Samples/DiscordHook) |
| [Dynatrace](https://www.dynatrace.com/) | Data Logger.Provider.Dynatrace | [Dynatrace](https://github.com/dliocode/datalogger/tree/main/Samples/Dynatrace) |
| [ElasticSearch](https://www.elastic.co/pt/what-is/elasticsearch) | DataLogger.Provider.ElasticSearch | [ElasticSearch](https://github.com/dliocode/datalogger/tree/main/Samples/ElasticSearch) |
| [Teach him](https://elmah.io/) | DataLogger.Provider.Elmah | [Teach him](https://github.com/dliocode/datalogger/tree/main/Samples/Elmah) |
| Email | DataLogger.Provider.Email | [Email](https://github.com/dliocode/datalogger/tree/main/Samples/Email) |
| event log | DataLogger.Provider.EventLog | [EventLog](https://github.com/dliocode/datalogger/tree/main/Samples/EventLog) |
| Events | DataLogger.Provider.Events | [Events](https://github.com/dliocode/datalogger/tree/main/Samples/Events) <br /> [Events - With DataSet](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20DataSet) <br /> [Events - With SQLite](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20SQLLite) |
| [Firebase](https://firebase.google.com/) | Data Logger.Provider.Firebase.Realtime Database | [Firebase](https://github.com/dliocode/datalogger/tree/main/Samples/Firebase) |
| [Grafana](https://grafana.com/) | DataLogger.Provider.Grafana.Loki <br /> datalogger.provider.graphana.oncall.webhook | [Grafana](https://github.com/dliocode/datalogger/tree/main/Samples/Grafana) |
| [GraphJSON](https://graphjson.com/) | DataLogger.Provider.GraphJSON | [GraphJSON](https://github.com/dliocode/datalogger/tree/main/Samples/GraphJSON) |
| [Graylog](https://www.graylog.org/) | DataLogger.Provider.Graylog | [Graylog](https://github.com/dliocode/datalogger/tree/main/Samples/Graylog) |
| HTML | DataLogger.Provider.HTML | [HTML](https://github.com/dliocode/datalogger/tree/main/Samples/HTML) |
| ListBox | DataLogger.Provider.ListBox | [ListBox](https://github.com/dliocode/datalogger/tree/main/Samples/ListBox) |
| ListView | DataLogger.Provider.ListView | [ListView](https://github.com/dliocode/datalogger/tree/main/Samples/ListView) |
| [Logentries](https://logentries.com/) | DataLogger.Provider.Logentries | [Logentries](https://github.com/dliocode/datalogger/tree/main/Samples/Logentries) |
| [Logflare](https://logflare.app/) | DataLogger.Provider.Logflare | [Logflare](https://github.com/dliocode/datalogger/tree/main/Samples/Logflare) |
| [Loggly](https://www.loggly.com/) | DataLogger.Provider.Loggly | [Loggly](https://github.com/dliocode/datalogger/tree/main/Samples/Loggly) |
| [Logstash](https://www.elastic.co/pt/logstash/) | DataLogger.Provider.Logstash | [Logstash](https://github.com/dliocode/datalogger/tree/main/Samples/Logstash) |
| [Logtail](https://betterstack.com/logtail) | DataLogger.Provider.Logtail | [Logtail](https://github.com/dliocode/datalogger/tree/main/Samples/Logtail) |
| [Logz](https://logz.io/) | DataLogger.Provider.Logz | [Logz](https://github.com/dliocode/datalogger/tree/main/Samples/Logz) |
| [Mailgun](https://www.mailgun.com/) | DataLogger.Provider.Mailgun.Api | [Mailgun](https://github.com/dliocode/datalogger/tree/main/Samples/Mailgun-Api) |
| [Mailjet](https://www.mailjet.com/) | DataLogger.Provider.Mailjet.Api | [Mailjet](https://github.com/dliocode/datalogger/tree/main/Samples/Mailjet-Api) |
| [Mattermost](https://mattermost.com/) | DataLogger.Provider.Mattermost <br /> DataLogger.Provider.Mattermost.WebHooks | [Mattermost](https://github.com/dliocode/datalogger/tree/main/Samples/Mattermost) <br /> [MattermostHook](https://github.com/dliocode/datalogger/tree/main/Samples/MattermostHook) |
| Memo | DataLogger.Provider.Memo | [Memo](https://github.com/dliocode/datalogger/tree/main/Samples/Memo) <br /> [Memo and Text File](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20and%20TexFile) <br /> [Memo - Only Error/VCL](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20-%20Only%20Error/VCL) |
| Memory | DataLogger.Provider.Memory | [Memory](https://github.com/dliocode/datalogger/tree/main/Samples/Memory) |
| [mezma](https://www.mezmo.com/) | DataLogger.Provider.Mezmo | [mezma](https://github.com/dliocode/datalogger/tree/main/Samples/Mezmo) |
| [MongoDB](https://www.mongodb.com) | DataLogger.Provider.MongoDB.Cloud | [MongoDB](https://github.com/dliocode/datalogger/tree/main/Samples/MongoDB) |
| [Ntfy](https://ntfy.sh) | DataLogger.Provider.Ntfy | [Ntfy](https://github.com/dliocode/datalogger/tree/main/Samples/Ntfy) |
| [NewRelic](https://newrelic.com) | DataLogger.Provider.NewRelic | [NewRelic](https://github.com/dliocode/datalogger/tree/main/Samples/NewRelic) |
| Notification | DataLogger.Provider.Notification | [Notification](https://github.com/dliocode/datalogger/tree/main/Samples/Notification) |
| [New](https://novu.co/) | datalogger.provider.pain | [New](https://github.com/dliocode/datalogger/tree/main/Samples/Novu) |
| OutputDebugString | DataLogger.Provider.OutputDebugString | [OutputDebugString](https://github.com/dliocode/datalogger/tree/main/Samples/OutputDebugString) |
| [PaperTrail](https://papertrailapp.com) | DataLogger.Provider.PaperTrail | [PaperTrail](https://github.com/dliocode/datalogger/tree/main/Samples/PaperTrail) |
| [Postmark](https://postmarkapp.com) | DataLogger.Provider.Postmark.Api | [Postmark](https://github.com/dliocode/datalogger/tree/main/Samples/Postmark-Api) |
| [RabbitMQ](https://github.com/danieleteti/delphistompclient) | DataLogger.Provider.RabbitMQ | [RabbitMQ](https://github.com/dliocode/datalogger/tree/main/Samples/RabbitMQ) |
| [Redis](https://github.com/danieleteti/delphiredisclient) | DataLogger.Provider.Redis | [Redis](https://github.com/dliocode/datalogger/tree/main/Samples/Redis) |
| Rest | DataLogger.Provider.REST.HTTPClient <br /> DataLogger.Provider.REST.Indy <br /> DataLogger.Provider.REST.NetHTTPClient | [Rest](https://github.com/dliocode/datalogger/tree/main/Samples/REST) |
| RichEdit | DataLogger.Provider.RichEdit | [RichEdit](https://github.com/dliocode/datalogger/tree/main/Samples/RichEdit) |
| [Sematext](https://sematext.com/) | DataLogger.Provider.Sematext.Logs | [Sematext-Logs](https://github.com/dliocode/datalogger/tree/main/Samples/Sematext-Logs) |
| [SendChamp](https://www.sendchamp.com/) | DataLogger.Provider.SendChamp.SMS <br /> DataLogger.Provider.SendChamp.WhatsApp | [SendEmail](https://github.com/dliocode/datalogger/tree/main/Samples/SendChamp) |
| [SendEmail](https://github.com/dliocode/sendemail) | DataLogger.Provider.SendEmail | [SendEmail](https://github.com/dliocode/datalogger/tree/main/Samples/SendEmail) |
| [SendGrid](https://sendgrid.com/) | DataLogger.Provider.SendGrid.WebApi | [SendGridWebApi](https://github.com/dliocode/datalogger/tree/main/Samples/SendGrid-WebApi) |
| [Slack](https://slack.com/) | DataLogger.Provider.Slack.WebApi <br /> DataLogger.Provider.Slack.WebHook | [Slack](https://github.com/dliocode/datalogger/tree/main/Samples/Slack) |
| [Splunk](https://www.splunk.com/) | DataLogger.Provider.Splunk | [Splunk](https://github.com/dliocode/datalogger/tree/main/Samples/Splunk) |
| Socket | DataLogger.Provider.Socket | [Socket](https://github.com/dliocode/datalogger/tree/main/Samples/Socket) |
| [SumoLogic](https://www.sumologic.com/) | DataLogger.Provider.SumoLogic | [SumoLogic](https://github.com/dliocode/datalogger/tree/main/Samples/SumoLogic) |
| SysLog | DataLogger.Provider.SysLog <br /> DataLogger.Provider.SysLog.Indy | [SysLog](https://github.com/dliocode/datalogger/tree/main/Samples/SysLog) <br /> [SysLog Indy](https://github.com/dliocode/datalogger/tree/main/Samples/SysLogIndy) |
| [Telegram](https://core.telegram.org/) | DataLogger.Provider.Telegram | [Telegram](https://github.com/dliocode/datalogger/tree/main/Samples/Telegram) |
| TextFile | DataLogger.Provider.TextFile | [TextFile](https://github.com/dliocode/datalogger/tree/main/Samples/TextFile) |
| [UltraMSG](https://ultramsg.com) | DataLogger.Provider.UltraMSG.WhatsApp | [UltraMSG](https://github.com/dliocode/datalogger/tree/main/Samples/UltraMSG) |
| [Twilio](https://www.twilio.com/) | DataLogger.Provider.Twilio.SMS <br /> DataLogger.Provider.Twilio.WhatsApp | [Twilio](https://github.com/dliocode/datalogger/tree/main/Samples/Twilio) |
| [Z-API](https://z-api.io/) | DataLogger.Provider.ZAPI.WhatsApp | [Z-API](https://github.com/dliocode/datalogger/tree/main/Samples/ZAPI) |

## Dependencies

These dependencies occur when using some *providers*

| Provider | Dependency |
| --- | --- |
| DataLogger.Provider.RabbitMQ | [RabbitMQ](https://github.com/danieleteti/delphistompclient) |
| DataLogger.Provider.Redis | [Redis](https://github.com/danieleteti/delphiredisclient) |
| DataLogger.Provider.SendEmail | [SendEmail](https://github.com/dliocode/sendemail) |

## Extra Information

### Plataforma Android:

Permission is required **ACCESS_WIFI_STATE**: Used to capture the MAC Address of the device.

## Examples

### Standard Use

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  Logger
    .Trace('My Trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal')
    .Custom('Custom Level', 'My Custom')
    ;

  // or

  Logger
    .T('My Trace')
    .D('My Debug')
    .I('My Info')
    .W('My Warn')
    .E('My Error')
    .S('My Success')
    .F('My Fatal')
    .C('Custom Level', 'My Custom')
    ;

  Readln;

end.
```

### Create a new DataLogger instance

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
    .Trace('My Trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal')
    .Custom('Custom Level', 'My message with custom level');
  Readln;
end.
```

### DataLogger Simple

```delphi
uses
  DataLogger,
  DataLogger.Simple,
  DataLogger.Provider.Console;

begin
  // Defini o provider
  Logger.AddProvider(TProviderConsole.Create);

  //Defini no DataloggerSimple a instância do log a ser utilizado
  TDataLoggerSimple.SetDataLogger(Logger);

  // Só usar o modo simples;
  Trace('My message Trace');
  Debug('My message debug');
  Info('My message info');
  Success('My message success');
  Warn('My message warn');
  Error('My message error');
  Fatal('My message fatal');
  Custom('My Type', 'My message custom');

  Readln;
end.

```

## Custom

O *Custom* is how to define a name for your own *level*.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  Logger.Custom('Custom Level', 'My message with custom level!');

  // Output: 2022-12-01 09:00:05.500 [Custom Level] My message with custom level!

  Readln;
end.
```

## Specials

Functions that operate on some providers.

```delphi
  // Pula uma linha
  Logger.SlineBreak;

  // Desfaz o último log registrado
  Logger.UndoLast;
```

### SlineBreak

The function `SlineBreak` using to make the line break.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Gerando os logs
  Logger.Info('My message with level INFO');
  Logger.SlineBreak;
  Logger.Error('My message with level ERROR');

  // Output: 2022-12-01 09:00:05.500 [INFO] My message with level INFO
  // Output:
  // Output: 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

### UndoLast

The function `UndoLast` using to undo the last record made.

*   Some *Providers* do not have some feature that allows you to undo the last record sent.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Gerando os logs
  Logger.Info('My message with level INFO');

  // Output: 2022-12-01 09:00:05.500 [INFO] My message with level INFO

  Sleep(4000);
  Logger.UndoLast;
  Logger.Error('My message with level ERROR');

  // Output: 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## Template (Log Format)

The template is the format in which the log will be generated.

default template:

```delphi
${timestamp} [TID ${thread_id}] [PID ${process_id}] [SEQ ${sequence}] [${level}] [${tag}] ${message}
```

### Constants

There are some constants that can be used to facilitate the creation of the template.

*   Common

```delphi
// Exibe o id que foi gerado o log, no formato GUID
TLoggerTemplate.LOG_ID = '${id}';

// Exibe o nome do log. ex: Logger.SetName('SERVICE_REMOTE')
TLoggerTemplate.LOG_NAME = '${name}';

// Exibe a sequencia que o log foi gerado.
TLoggerTemplate.LOG_SEQUENCE = '${sequence}';

// Exibe a data e hora que foi gerado, usado o SetFormatTimestamp
TLoggerTemplate.LOG_TIMESTAMP = '${timestamp}';

// Exibe a data e hora que foi gerado, no formato ISO8601.
TLoggerTemplate.LOG_TIMESTAMP_ISO8601 = '${timestamp_iso8601}';

// Exibe a data e hora que foi gerado, no formato UNIX.
TLoggerTemplate.LOG_TIMESTAMP_UNIX = '${timestamp_unix}';

// Exibe o Id da thread que foi gerado o log.
TLoggerTemplate.LOG_THREADID = '${thread_id}';

// Exibe o id do processo do app.
TLoggerTemplate.LOG_PROCESSID = '${process_id}';

// Exibe o level do log, sendo eles: TRACE / DEBUG / INFO / SUCCESS / WARN / ERROR / FATAL
TLoggerTemplate.LOG_LEVEL = '${level}';

// Exibe o level do log no formato numérico, sendo eles: 1=TRACE / 2=DEBUG / 3=INFO / 4=SUCCESS / 5=WARN / 6=ERROR / 7=FATAL / 8=CUSTOM
TLoggerTemplate.LOG_LEVEL_VALUE = '${level_value}';

// Exibe a tag do log, essa informação é preenchida a após a mensagem; Ex: Logger.Debug('Minha mensagem','Minha Tag');
TLoggerTemplate.LOG_TAG = '${tag}';

// Exibe a mensagem do log, sem essa tag a mensagem não é exibida. Ex: Logger.Debug('Minha mensagem');
TLoggerTemplate.LOG_MESSAGE = '${message}';
```

*   Specials:

```delphi
// Exibe o nome do app.
TLoggerTemplate.LOG_APPNAME = '${app_name}';

// Exibe o diretório do app.
TLoggerTemplate.LOG_APPPATH = '${app_path}';

// Exibe a versão do app.
TLoggerTemplate.LOG_APPVERSION = '${app_version}';

// Exibe o tamanho do app em MB.
TLoggerTemplate.LOG_APPSIZE = '${app_size}';

// Exibe o nome do computador.
TLoggerTemplate.LOG_COMPUTERNAME = '${computer_name}';

// Exibe o nome do usuário do Sistema Operacional.
TLoggerTemplate.LOG_USERNAME = '${username}';

// Exibe as informações do Sistema Operacional.
TLoggerTemplate.LOG_OSVERSION = '${os_version}';

// Exibe o IP Local.
TLoggerTemplate.LOG_IP_LOCAL = '${ip_local}';

// Exibe o MAC Address.
TLoggerTemplate.LOG_MAC_ADDRESS = '${mac_address}';

```

## SetTemplate

Defines the format in which the log will be recorded

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o template com constante
  Logger.SetTemplate(Format('%s [%s] %s', [TLoggerTemplate.LOG_TIMESTAMP, TLoggerTemplate.LOG_LEVEL, LoggerTemplate.LOG_MESSAGE]));

  // Gerando os logs
  Logger.Info('My message with level INFO');
  Logger.Error('My message with level ERROR');

  // Output: 2022-12-01 09:00:05.500 [INFO] My message with level INFO
  // Output: 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

### How to define a template in *providers* specific

*   It is possible to define several specific settings in each *provider* separate.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console,
  DataLogger.Provider.TextFile;

begin
  // Formato do console '${timestamp} [${level}] ${message}'
  Logger.AddProvider(
    TProviderConsole.Create
    .SetTemplate('${timestamp} [${level}] ${message}')
  );

  // Formato do text file '${timestamp} - ${message}'
  Logger.AddProvider(
    TProviderTextFile.Create
    .SetTemplate('${timestamp} - ${message}')
  );

  // Gerando os logs
  Logger.Info('My message with level INFO');
  Logger.Error('My message with level ERROR');

  // Output Console:
  // 2022-12-01 09:00:05.500 [INFO] My message with level INFO
  // 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  // Output TextFile:
  // 2022-12-01 09:00:05.500 - My message with level INFO
  // 2022-12-01 09:00:05.600 - My message with level ERROR

  Readln;
end.
```

## SetFormatTimestamp

Changes the format of the TimeStamp.

*   Default TimeStamp format: `yyyy-mm-dd hh:mm:ss.zzz`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do Timestamp
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss')

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o template com constante
  Logger.SetTemplate(Format('%s [%s] %s', [TLoggerTemplate.LOG_TIMESTAMP, TLoggerTemplate.LOG_LEVEL, LoggerTemplate.LOG_MESSAGE]));

  // Gerando os logs
  Logger.Info('My message with level INFO');
  Logger.Error('My message with level ERROR');

  // Output: 01/12/2022 09:00:05 [INFO] My message with level INFO
  // Output: 01/12/2022 09:00:05 [ERROR] My message with level ERROR

  Readln;
end.
```

## Level

The DataLogger has these levels to generate the *logs*:

```delphi
  Logger.Trace('');
  Logger.Debug('');
  Logger.Info('');
  Logger.Success('');
  Logger.Warn('');
  Logger.Error('');
  Logger.Fatal('');
  Logger.Custom('');

  // Modo simplificado
  Logger.T(''); // TRACE
  Logger.D(''); // DEBUG
  Logger.I(''); // INFO
  Logger.S(''); // SUCCESS
  Logger.W(''); // WARN
  Logger.E(''); // ERROR
  Logger.F(''); // FATAL
  Logger.C(''); // CUSTOM
```

## SetLevelName

It is possible to change the level description to another description.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o Level
  Logger.SetLevelName(TLoggerLevel.Info, 'INFORMATION');
  Logger.SetLevelName(TLoggerLevel.Warn, '  WARNING  ');

  // Gerando os logs
  Logger.Info('My message with level INFO');
  Logger.Warn('My message with level WARN');

  // Output: 2022-12-01 09:00:05.600 [INFORMATION] My message with level INFO
  // Output: 2022-12-01 09:00:05.600 [  WARNING  ] My message with level WARN

  Readln;
end.
```

## SetLevel

It is possible to show only the *logs* from a *level* defined, based on `TLoggerLevel`.

SetLevel default value = `TLoggerLevel.All`

### TLoggerLevel

*   When defining a level, only the chosen option and its superior types will be displayed.
*   Ex: `Logger.SetLevel(TLoggerLevel.Warn);` - Will be registered only the *logs* with the type `Warn / Error / Fatal / Custom`.

```delphi
  TLoggerLevel.All = 'Utilizado para operações internas'
  TLoggerLevel.Trace = 'Level 1'
  TLoggerLevel.Debug = 'Level 2'
  TLoggerLevel.Info = 'Level 3'
  TLoggerLevel.Success = 'Level 4'
  TLoggerLevel.Warn = 'Level 5'
  TLoggerLevel.Error = 'Level 6'
  TLoggerLevel.Fatal = 'Level 7'
  TLoggerLevel.Custom = 'Level 8'
```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o Level
  Logger.SetLevel(TLoggerLevel.Warn);

  // Gerando os logs
  Logger.Info('My message with level INFO');
  Logger.Error('My message with level ERROR');

  // Output: 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## SetDisableLevel

It is possible to disable some *levels* do log, com base no `TLoggerLevel`.

Set Disable Level default value = `[]`

*   When disabled, only options that are not disabled will be displayed.
*   Ex: `Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]);` - Will be registered only the *logs* with the type `Tracer / Debug / Success / Error / Fatal / Custom`.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o DisableLevel
  Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]);

  // Gerando os logs
  Logger.Debug('My message with level DEBUG');
  Logger.Info('My message with level INFO');
  Logger.Warn('My message with level WARN');
  Logger.Error('My message with level ERROR');

  // Output:
  // 2022-12-01 09:00:05.500 [DEBUG] My message with level DEBUG
  // 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## SetOnlyLevel

It is possible to show only some *levels* do log, com base no `TLoggerLevel`.

SetOnlyLevel default value = `[TLoggerLevel.All]`

*   When defined, only registered options will be displayed.
*   Ex: `Logger.SetOnlyLevel([TLoggerLevel.Error]);` - Will be registered only the *logs* with the type `Error`.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o OnlyLevel
  Logger.SetOnlyLevel([TLoggerLevel.Error]);

  // Gerando os logs
  Logger.Debug('My message with level DEBUG');
  Logger.Info('My message with level INFO');
  Logger.Warn('My message with level WARN');
  Logger.Error('My message with level ERROR');

  // Output:
  // 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## SetLogException

It is possible to catch exceptions generated by *providers*

SetLogException default value = `nil`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o LogException
  Logger.SetLogException(
    procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetriesCount: Integer)
    begin
      // Sender - Provider que gerou a exceção, para visualizar - Sender.ClassName

      // LogItem - Contém todas as informações do log

      // E - Contém as informações da exceção

      // RetriesCount - Contém o número da tentativa realizada
      // Caso seja alterado o valor para -1, o processo é interrompido
    end
  );

  // Gerando o log
  Logger.Error('My message with level ERROR');

  // Output:
  // 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## SetMaxRetries

It is possible to set the number of attempts that the *Provider* should try when saving the log.

Set Max Retries default value = `5`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Definindo o máximo de tentativas
  Logger.SetMaxRetries(5);

  // Gerando o log
  Logger.Error('My message with level ERROR');

  // Output:
  // 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## SetIgnoreTemplate

It is possible to ignore the Template and save all data generated by DataLogger;

```delphi
  SetIgnoreTemplate({1}, {2}, {3}, {4});

  Parâmetros:
    {1} = (Boolean) = Defini se deve ignorar o Template.
    {2} = (string) = Defini qual texto vai fazer a separação das informações, semelhante ao CSV.
    {3} = (Boolean) = Defini se deve mostrar as palavras chaves de cada valor.
    {4} = (string) = Defini qual texto deve separar a palavra chave do valor.

  Logger.SetIgnoreTemplate(True, '|', True, ' -> ');

  {palavra_chave}           = "timestamp"
  {palavra_chave_separador} = " -> "
  {valor}                   = "2022-09-15T14:39:38.896-03:00"
  {separator}               = " | "

  // output timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896
```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Ignorando o log format
  Logger.SetIgnoreTemplate(True, '|', True, ' -> ');

  // Gerando o log
  Logger.Error('My message with level ERROR');

  // Output:
  // timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896 | name -> | sequence -> 1 | thread_id -> 3804 | level -> Trace | level_value -> 1 | tag -> | message -> My Trace | app_name -> ProviderTextFile | app_version -> 1.0.0.0 | app_path -> C:\Github\DataLogger\Samples\TextFile\Win32\Debug | app_size -> 13,24 MB | computer_name -> DESKTOP-7RP1H3K | username -> danil | os_version -> Windows 10 (Version 21H2, OS Build 19044.1889, 64-bit Edition) | process_id -> 13608 | ip_local -> 192.168.56.1

  Readln;
end.
```

## SetName

It is possible to define a name for the *DataLogger* which can be displayed in the log record. This name can be used to differentiate the *DataLogger* when more than one instance.

SetName default value = `EmptyStr`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${name} ${timestamp} [${level}] ${message}');

  // Definindo o name
  Logger.SetName('MyLogger');

  // Gerando o log
  Logger.Error('My message with level ERROR');

  // Output:
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## SetLiveMode

It is possible to save the logs in real time, so the next statement of your code will only continue after the log is saved!

Currently, the logs are recorded in memory and then saved without crashing the application.

SetLiveMode default value = `false`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o template
  Logger.SetTemplate('${name} ${timestamp} [${level}] ${message}');

  // Definindo o LiveMode
  Logger.SetLiveMode(True);

  // Gerando o log
  Logger.Error('My message with level ERROR');

  // Output:
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] My message with level ERROR

  Readln;
end.
```

## Transaction

Is it possible to work with *Transaction*, just as it is used in other database-connected components.

The use of this procedure can be applied to the following situation;

Vamos contar uma pequena história:

> we have a *procedure* that is doing an execution, in each step several log information is generated, by habit we always save this information, this makes our text file, for example, too big. <br /> Now imagine being able to save the data only if there was an error during execution or when it was really necessary to save the data.

### how to enable

Enabling the use of *Transaction* deve ser feita por *Provider* with the function `UseTransaction(True)`.

Ex:

```delphi
Logger.AddProvider(
  TProviderConsole.Create
  .UseTransaction(True) // Enable transaction only on this provider
);
```

### StartTransaction

Starts a new transaction.

### CommitTransaction

Commit the record of all logs in the transaction.

### RollbackTransaction

Cancels all logs generated in the transaction.

### Example with Simple Transaction

[Sample com Transaction](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction)

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Iniciando a transaction
  Logger.StartTransaction;

  Writeln('Iniciou a transaction');

  // Gerando o log
  Logger.Info('My message with level INFO 1 ');
  Logger.Info('My message with level INFO 2');
  Logger.Info('My message with level INFO 3');
  Logger.Info('My message with level INFO 4');
  Logger.Info('My message with level INFO 5');
  Logger.Info('My message with level INFO 6');
  Logger.Info('My message with level INFO 7');
  Logger.Info('My message with level INFO 8');
  Logger.Info('My message with level INFO 9');

  Writeln('Terminou os Logs');

  // Fazendo o commit
  Logger.CommitTransaction;
  Writeln('Fez os commits');

  Readln;
end.
```

### example with *TRANSTO* nested

[Sample with Nested Transaction](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction%20Aninhada)

As *transactions* nested data gives the possibility of greater management when saving information.

*   Only one observation must be remembered, the final record will only be saved if there was *commit* the parent transaction, that is, the transaction that started the whole process; <br /> Caso a transação pai, tenha feito rollback, as transações filhas que foram feitas *commit* will be ignored!

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Iniciando a transaction
  Logger.StartTransaction;
  try
    // Gerando o log
    Logger.Info('My message with level INFO 1 ');
    Logger.Info('My message with level INFO 2');
    Logger.Info('My message with level INFO 3');
    Logger.Info('My message with level INFO 4');

    // Iniciando 2 transaction
    Logger.StartTransaction;
    try
      Logger.Info('My message with level INFO 5');
      Logger.Info('My message with level INFO 6');
    finally
      // fazendo Rollback da segunda transaction
      Logger.RollbackTransaction;
    end;

    Logger.Info('My message with level INFO 7');
    Logger.Info('My message with level INFO 8');
    Logger.Info('My message with level INFO 9');
  finally
    // Fazendo o commit
    Logger.CommitTransaction;
  end;

  Readln;
end.

```
