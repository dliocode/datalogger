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

DataLogger was designed to be a simple logging library with support for multiple _providers_.

Para ver este arquivo leia-me em português, por favor clique aqui: [README](https://github.com/dliocode/datalogger/blob/main/README.md)

Support: developer.dlio@gmail.com

## ⚙️ Installation

### To install into your project using [boss](https://github.com/HashLoad/boss):

```sh
$ boss install github.com/dliocode/datalogger
```

### Manual Installation

Add the following folders to your project, under _Project > Options > Delphi Compiler > Search path_

```
../src/core
../src/Providers
```

## 📌 Index

- [How to use](#how-to-use)
- [Providers](#providers)
- [Dependencies](#dependencies)
- [Extra Information](#extra-information)
- [Examples](#examples)
  - [Default Usage](#Default Usage)
  - [Create a new instance of DataLogger](#create-a-new-instance-of-datalogger)
  - [DataLogger Simple](#datalogger-simple)
  - [Custom](#custom)
  - [Special](#special)
    - [SlineBreak](#slinebreak)
    - [UndoLast](#undolast)
  - [Template (Log Format)](#template-log-format)
    - [Template Constants](#template-constants)
  - [SetTemplate](#settemplate)
    - [How to define a template in specific providers](#how-to-define-a-template-in-specific-providers)
  - [SetFormatTimestamp](#setformattimestamp)
  - [Level](#level)
  - [SetLevelName](#setlevelname)
  - [SetLevel](#setlevel)
  - [SetDisableLevel](#setdisablelevel)
  - [SetOnlyLevel](#setonlylevel)
  - [SetLogException](#setlogexception)
  - [SetMaxRetries](#setmaxretries)
  - [SetIgnoreTemplate](#setignoretemplate)
  - [SetName](#setname)
  - [SetLiveMode](#setlivemode)
  - [Transaction](#transaction)

## How to use

There are two different ways to use DataLogger:
Directly through `Logger` or instantiating your own `TDataLogger`.

[Samples](https://github.com/dliocode/datalogger/tree/main/Samples)

The first is just intended to be a convenient shared log to use across your application if you choose.

- **Uses required**:

```
use DataLogger;
```

## providers

A **_provider_** is essentially for storing your logs.
Each instance of a TDataLogger can have several **_providers_** configured.

Here is a list of **60 _providers_** available:

| Name                                                             | Uses                                                                                                               | Samples                                                                                                                                                                                                                                                                                                         |
| ---------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [AWS CloudWatch](https://aws.amazon.com/cloudwatch)              | DataLogger.Provider.AWS.CloudWatch                                                                                 | [AWS CloudWatch](https://github.com/dliocode/datalogger/tree/main/Samples/AWSCloudWatch)                                                                                                                                                                                                                        |
| [Axiom](https://axiom.co/)                                       | DataLogger.Provider.Axiom                                                                                          | [Axiom](https://github.com/dliocode/datalogger/tree/main/Samples/Axiom)                                                                                                                                                                                                                                         |
| [CallMeBot](https://www.callmebot.com/)                          | DataLogger.Provider.CallMeBot.WhatsApp                                                                             | [CallMeBot](https://github.com/dliocode/datalogger/tree/main/Samples/CallMeBot)                                                                                                                                                                                                                                 |
| Console                                                          | DataLogger.Provider.Console                                                                                        | [Console](https://github.com/dliocode/datalogger/tree/main/Samples/Console)<br /> [Console Simple](https://github.com/dliocode/datalogger/tree/main/Samples/Console%20-%20Simple)                                                                                                                               |
| [Coralogix](https://coralogix.com/)                              | DataLogger.Provider.Coralogix                                                                                      | [Coralogix](https://github.com/dliocode/datalogger/tree/main/Samples/Coralogix)                                                                                                                                                                                                                                 |
| CSV                                                              | DataLogger.Provider.CSV                                                                                            | [CSV](https://github.com/dliocode/datalogger/tree/main/Samples/CSV)                                                                                                                                                                                                                                             |
| [Datadog](https://www.datadoghq.com/)                            | DataLogger.Provider.Datadog                                                                                        | [Datadog](https://github.com/dliocode/datalogger/tree/main/Samples/Datadog)                                                                                                                                                                                                                                     |
| [Datalust](https://datalust.co/)                                 | DataLogger.Provider.Datalust                                                                                       | [Datalust](https://github.com/dliocode/datalogger/tree/main/Samples/Datalust)                                                                                                                                                                                                                                   |
| [Discord](https://discord.com/developers/docs/resources/webhook) | DataLogger.Provider.Discord.WebHook                                                                                | [DiscordHook](https://github.com/dliocode/datalogger/tree/main/Samples/DiscordHook)                                                                                                                                                                                                                             |
| [Dynatrace](https://www.dynatrace.com/)                          | DataLogger.Provider.Dyatrace                                                                                       | [Dynatrace](https://github.com/dliocode/datalogger/tree/main/Samples/Dynatrace)                                                                                                                                                                                                                                 |
| [ElasticSearch](https://www.elastic.co/pt/what-is/elasticsearch) | DataLogger.Provider.ElasticSearch                                                                                  | [ElasticSearch](https://github.com/dliocode/datalogger/tree/main/Samples/ElasticSearch)                                                                                                                                                                                                                         |
| [Elmah](https://elmah.io/)                                       | DataLogger.Provider.Elmah                                                                                          | [Elmah](https://github.com/dliocode/datalogger/tree/main/Samples/Elmah)                                                                                                                                                                                                                                         |
| Email                                                            | DataLogger.Provider.Email                                                                                          | [Email](https://github.com/dliocode/datalogger/tree/main/Samples/Email)                                                                                                                                                                                                                                         |
| EventLog                                                         | DataLogger.Provider.EventLog                                                                                       | [EventLog](https://github.com/dliocode/datalogger/tree/main/Samples/EventLog)                                                                                                                                                                                                                                   |
| Events                                                           | DataLogger.Provider.Events                                                                                         | [Events](https://github.com/dliocode/datalogger/tree/main/Samples/Events)<br />[Events - With DataSet](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20DataSet)<br />[Events - With SQLLite](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20SQLLite) |
| [Firebase](https://firebase.google.com/)                         | DataLogger.Provider.Firebase.RealtimeDatabase                                                                      | [Firebase](https://github.com/dliocode/datalogger/tree/main/Samples/Firebase)                                                                                                                                                                                                                                   |
| [Grafana](https://grafana.com/)                                  | DataLogger.Provider.Grafana.Loki<br />DataLogger.Provider.Grafana.OnCall.WebHook                                   | [Grafana](https://github.com/dliocode/datalogger/tree/main/Samples/Grafana)                                                                                                                                                                                                                                     |
| [GraphJSON](https://graphjson.com/)                              | DataLogger.Provider.GraphJSON                                                                                      | [GraphJSON](https://github.com/dliocode/datalogger/tree/main/Samples/GraphJSON)                                                                                                                                                                                                                                 |
| [Graylog](https://www.graylog.org/)                              | DataLogger.Provider.Graylog                                                                                        | [Graylog](https://github.com/dliocode/datalogger/tree/main/Samples/Graylog)                                                                                                                                                                                                                                     |
| HTML                                                             | DataLogger.Provider.HTML                                                                                           | [HTML](https://github.com/dliocode/datalogger/tree/main/Samples/HTML)                                                                                                                                                                                                                                           |
| ListBox                                                          | DataLogger.Provider.ListBox                                                                                        | [ListBox](https://github.com/dliocode/datalogger/tree/main/Samples/ListBox)                                                                                                                                                                                                                                     |
| ListView                                                         | DataLogger.Provider.ListView                                                                                       | [ListView](https://github.com/dliocode/datalogger/tree/main/Samples/ListView)                                                                                                                                                                                                                                   |
| [Logentries](https://logentries.com/)                            | DataLogger.Provider.Logentries                                                                                     | [Logentries](https://github.com/dliocode/datalogger/tree/main/Samples/Logentries)                                                                                                                                                                                                                               |
| [Logflare](https://logflare.app/)                                | DataLogger.Provider.Logflare                                                                                       | [Logflare](https://github.com/dliocode/datalogger/tree/main/Samples/Logflare)                                                                                                                                                                                                                                   |
| [Loggly](https://www.loggly.com/)                                | DataLogger.Provider.Loggly                                                                                         | [Loggly](https://github.com/dliocode/datalogger/tree/main/Samples/Loggly)                                                                                                                                                                                                                                       |
| [Logstash](https://www.elastic.co/pt/logstash/)                  | DataLogger.Provider.Logstash                                                                                       | [Logstash](https://github.com/dliocode/datalogger/tree/main/Samples/Logstash)                                                                                                                                                                                                                                   |
| [Logtail](https://betterstack.com/logtail)                       | DataLogger.Provider.Logtail                                                                                        | [Logtail](https://github.com/dliocode/datalogger/tree/main/Samples/Logtail)                                                                                                                                                                                                                                     |
| [Logz](https://logz.io/)                                         | DataLogger.Provider.Logz                                                                                           | [Logz](https://github.com/dliocode/datalogger/tree/main/Samples/Logz)                                                                                                                                                                                                                                           |
| [Mailgun](https://www.mailgun.com/)                              | DataLogger.Provider.Mailgun.Api                                                                                    | [Mailgun](https://github.com/dliocode/datalogger/tree/main/Samples/Mailgun-Api)                                                                                                                                                                                                                                 |
| [Mailjet](https://www.mailjet.com/)                              | DataLogger.Provider.Mailjet.Api                                                                                    | [Mailjet](https://github.com/dliocode/datalogger/tree/main/Samples/Mailjet-Api)                                                                                                                                                                                                                                 |
| [Mattermost](https://mattermost.com/)                            | DataLogger.Provider.Mattermost<br />DataLogger.Provider.Mattermost.WebHooks                                        | [Mattermost](https://github.com/dliocode/datalogger/tree/main/Samples/Mattermost) <br /> [MattermostHook](https://github.com/dliocode/datalogger/tree/main/Samples/MattermostHook)                                                                                                                              |
| Memo                                                             | DataLogger.Provider.Memo                                                                                           | [Memo](https://github.com/dliocode/datalogger/tree/main/Samples/Memo)<br />[Memo and TexFile](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20and%20TexFile)<br />[Memo - Only Error/VCL](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20-%20Only%20Error/VCL)                 |
| Memory                                                           | DataLogger.Provider.Memory                                                                                         | [Memory](https://github.com/dliocode/datalogger/tree/main/Samples/Memory)                                                                                                                                                                                                                                       |
| [Mezmo](https://www.mezmo.com/)                                  | DataLogger.Provider.Mezmo                                                                                          | [Mezmo](https://github.com/dliocode/datalogger/tree/main/Samples/Mezmo)                                                                                                                                                                                                                                         |
| [MongoDB](https://www.mongodb.com)                               | DataLogger.Provider.MongoDB.Cloud                                                                                  | [MongoDB](https://github.com/dliocode/datalogger/tree/main/Samples/MongoDB)                                                                                                                                                                                                                                     |
| [Ntfy](https://ntfy.sh)                                          | DataLogger.Provider.Ntfy                                                                                           | [Ntfy](https://github.com/dliocode/datalogger/tree/main/Samples/Ntfy)                                                                                                                                                                                                                                           |
| [NewRelic](https://newrelic.com)                                 | DataLogger.Provider.NewRelic                                                                                       | [NewRelic](https://github.com/dliocode/datalogger/tree/main/Samples/NewRelic)                                                                                                                                                                                                                                   |
| Notification                                                     | DataLogger.Provider.Notification                                                                                   | [Notification](https://github.com/dliocode/datalogger/tree/main/Samples/Notification)                                                                                                                                                                                                                           |
| [Novu](https://novu.co/)                                         | DataLogger.Provider.Novu                                                                                           | [Novu](https://github.com/dliocode/datalogger/tree/main/Samples/Novu)                                                                                                                                                                                                                                           |
| OutputDebugString                                                | DataLogger.Provider.OutputDebugString                                                                              | [OutputDebugString](https://github.com/dliocode/datalogger/tree/main/Samples/OutputDebugString)                                                                                                                                                                                                                 |
| [PaperTrail](https://papertrailapp.com)                          | DataLogger.Provider.PaperTrail                                                                                     | [PaperTrail](https://github.com/dliocode/datalogger/tree/main/Samples/PaperTrail)                                                                                                                                                                                                                               |
| [Postmark](https://postmarkapp.com)                              | DataLogger.Provider.Postmark.Api                                                                                   | [Postmark](https://github.com/dliocode/datalogger/tree/main/Samples/Postmark-Api)                                                                                                                                                                                                                               |
| [RabbitMQ](https://github.com/danieleteti/delphistompclient)     | DataLogger.Provider.RabbitMQ                                                                                       | [RabbitMQ](https://github.com/dliocode/datalogger/tree/main/Samples/RabbitMQ)                                                                                                                                                                                                                                   |
| [Redis](https://github.com/danieleteti/delphiredisclient)        | DataLogger.Provider.Redis                                                                                          | [Redis](https://github.com/dliocode/datalogger/tree/main/Samples/Redis)                                                                                                                                                                                                                                         |
| Rest                                                             | DataLogger.Provider.REST.HTTPClient<br />DataLogger.Provider.REST.Indy<br />DataLogger.Provider.REST.NetHTTPClient | [Rest](https://github.com/dliocode/datalogger/tree/main/Samples/REST)                                                                                                                                                                                                                                           |
| RichEdit                                                         | DataLogger.Provider.RichEdit                                                                                       | [RichEdit](https://github.com/dliocode/datalogger/tree/main/Samples/RichEdit)                                                                                                                                                                                                                                   |
| [Sematext](https://sematext.com/)                                | DataLogger.Provider.Sematext.Logs                                                                                  | [Sematext-Logs](https://github.com/dliocode/datalogger/tree/main/Samples/Sematext-Logs)                                                                                                                                                                                                                         |
| [SendChamp](https://www.sendchamp.com/)                          | DataLogger.Provider.SendChamp.SMS<br />DataLogger.Provider.SendChamp.WhatsApp                                      | [SendEmail](https://github.com/dliocode/datalogger/tree/main/Samples/SendChamp)                                                                                                                                                                                                                                 |
| [SendEmail](https://github.com/dliocode/sendemail)               | DataLogger.Provider.SendEmail                                                                                      | [SendEmail](https://github.com/dliocode/datalogger/tree/main/Samples/SendEmail)                                                                                                                                                                                                                                 |
| [SendGrid](https://sendgrid.com/)                                | DataLogger.Provider.SendGrid.WebApi                                                                                | [SendGridWebApi](https://github.com/dliocode/datalogger/tree/main/Samples/SendGrid-WebApi)                                                                                                                                                                                                                      |
| [Slack](https://slack.com/)                                      | DataLogger.Provider.Slack.WebApi<br />DataLogger.Provider.Slack.WebHook                                            | [Slack](https://github.com/dliocode/datalogger/tree/main/Samples/Slack)                                                                                                                                                                                                                                         |
| [Splunk](https://www.splunk.com/)                                | DataLogger.Provider.Splunk                                                                                         | [Splunk](https://github.com/dliocode/datalogger/tree/main/Samples/Splunk)                                                                                                                                                                                                                                       |
| Socket                                                           | DataLogger.Provider.Socket                                                                                         | [Socket](https://github.com/dliocode/datalogger/tree/main/Samples/Socket)                                                                                                                                                                                                                                       |
| [SumoLogic](https://www.sumologic.com/)                          | DataLogger.Provider.SumoLogic                                                                                      | [SumoLogic](https://github.com/dliocode/datalogger/tree/main/Samples/SumoLogic)                                                                                                                                                                                                                                 |
| SysLog                                                           | DataLogger.Provider.SysLog<br />DataLogger.Provider.SysLog.Indy                                                    | [SysLog](https://github.com/dliocode/datalogger/tree/main/Samples/SysLog)<br />[SysLog Indy](https://github.com/dliocode/datalogger/tree/main/Samples/SysLogIndy)                                                                                                                                               |
| [Telegram](https://core.telegram.org/)                           | DataLogger.Provider.Telegram                                                                                       | [Telegram](https://github.com/dliocode/datalogger/tree/main/Samples/Telegram)                                                                                                                                                                                                                                   |
| TextFile                                                         | DataLogger.Provider.TextFile                                                                                       | [TextFile](https://github.com/dliocode/datalogger/tree/main/Samples/TextFile)                                                                                                                                                                                                                                   |
| [UltraMSG](https://ultramsg.com)                                 | DataLogger.Provider.UltraMSG.WhatsApp                                                                              | [UltraMSG](https://github.com/dliocode/datalogger/tree/main/Samples/UltraMSG)                                                                                                                                                                                                                                   |
| [Twilio](https://www.twilio.com/)                                | DataLogger.Provider.Twilio.SMS<br />DataLogger.Provider.Twilio.WhatsApp                                            | [Twilio](https://github.com/dliocode/datalogger/tree/main/Samples/Twilio)                                                                                                                                                                                                                                       |
| [Z-API](https://z-api.io/)                                       | DataLogger.Provider.ZAPI.WhatsApp                                                                                  | [ZAP-API](https://github.com/dliocode/datalogger/tree/main/Samples/ZAPI)                                                                                                                                                                                                                                        |

## Dependencies

These dependencies occur when using some _providers_

| Provider                      | Dependence                                                   |
| ----------------------------- | ------------------------------------------------------------ |
| DataLogger.Provider.RabbitMQ  | [RabbitMQ](https://github.com/danieleteti/delphistompclient) |
| DataLogger.Provider.Redis     | [Redis](https://github.com/danieleteti/delphiredisclient)    |
| DataLogger.Provider.SendEmail | [SendEmail](https://github.com/dliocode/sendemail)           |

## Extra Information

### Android Platform:

**ACCESS_WIFI_STATE** permission is required: Used to capture the MAC Address of the device.

## Examples

### Default Usage

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  logger
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

  logger
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
use
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
use
  DataLogger,
  DataLogger.Simple,
  DataLogger.Provider.Console;

begin
  // Define the provider
  Logger.AddProvider(TProviderConsole.Create);

  //Define in DataloggerSimple the log instance to be used
  TDataLoggerSimple.SetDataLogger(Logger);

  // Only use simple mode;
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

_Custom_ is the way to define a name for your own _level_.

```delphi
use
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

## Special

functions that operate in some providers.

```delphi
  // Skip a line
  Logger.SlineBreak;

  // Undo the last recorded log
  Logger.UndoLast;
```

### SlineBreak

The `SlineBreak` function used to break a line.

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.SlineBreak;
  Logger.Error('My message in the Log type ERROR');

  // Output: 2022-12-01 09:00:05.500 [INFO] My log message of type INFO
  // Output:
  // Output: 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

### UndoLast

The `UndoLast` function used to undo the last record made.

- Some _Providers_ do not have some feature that allows you to undo the last record sent.

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Generating the logs
  Logger.Info('My log message of type INFO');

  // Output: 2022-12-01 09:00:05.500 [INFO] My log message of type INFO

  Sleep(4000);
  Logger.UndoLast;
  Logger.Error('My message in the Log type ERROR');

  // Output: 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## Template (Log Format)

The template is the format in which the log will be generated.

default template:

```delphi
${timestamp} [TID ${thread_id}] [PID ${process_id}] [SEQ ${sequence}] [${level}] [${tag}] ${message}
```

### Template Constants

There are some constants that can be used to facilitate the creation of the template.

- Common

```delphi
// Displays the id that generated the log, in GUID format
TLoggerTemplate.LOG_ID = '${id}';

// Displays the name of the log. ex: Logger.SetName('SERVICE_REMOTE')
TLoggerTemplate.LOG_NAME = '${name}';

// Displays the sequence that the log was generated.
TLoggerTemplate.LOG_SEQUENCE = '${sequence}';

// Displays the generated date and time, using SetFormatTimestamp
TLoggerTemplate.LOG_TIMESTAMP = '${timestamp}';

// Displays the date and time it was generated, in ISO8601 format.
TLoggerTemplate.LOG_TIMESTAMP_ISO8601 = '${timestamp_iso8601}';

// Displays the date and time it was generated, in UNIX format.
TLoggerTemplate.LOG_TIMESTAMP_UNIX = '${timestamp_unix}';

// Displays the ID of the thread that generated the log.
TLoggerTemplate.LOG_THREADID = '${thread_id}';

// Displays the process id of the app.
TLoggerTemplate.LOG_PROCESSID = '${process_id}';

// Displays the log level, namely: TRACE / DEBUG / INFO / SUCCESS / WARN / ERROR / FATAL
TLoggerTemplate.LOG_LEVEL = '${level}';

// Displays the log level in numerical format, as follows: 1=TRACE / 2=DEBUG / 3=INFO / 4=SUCCESS / 5=WARN / 6=ERROR / 7=FATAL / 8=CUSTOM
TLoggerTemplate.LOG_LEVEL_VALUE = '${level_value}';

// Displays the log tag, this information is filled in after the message; Ex: Logger.Debug('My Message','My Tag');
TLoggerTemplate.LOG_TAG = '${tag}';

// Displays the log message, without this tag the message is not displayed. Ex: Logger.Debug('My message');
TLoggerTemplate.LOG_MESSAGE = '${message}';
```

- Specials:

```delphi
// Displays the name of the app.
TLoggerTemplate.LOG_APPNAME = '${app_name}';

// Displays the app's directory.
TLoggerTemplate.LOG_APPPATH = '${app_path}';

// Displays the version of the app.
TLoggerTemplate.LOG_APPVERSION = '${app_version}';

// Displays app size in MB.
TLoggerTemplate.LOG_APPSIZE = '${app_size}';

// Displays the computer name.
TLoggerTemplate.LOG_COMPUTERNAME = '${computer_name}';

// Displays the Operating System username.
TLoggerTemplate.LOG_USERNAME = '${username}';

// Displays Operating System information.
TLoggerTemplate.LOG_OSVERSION = '${os_version}';

// Display the Local IP.
TLoggerTemplate.LOG_IP_LOCAL = '${ip_local}';

// Displays the MAC Address.
TLoggerTemplate.LOG_MAC_ADDRESS = '${mac_address}';

```

## SetTemplate

Defines the format in which the log will be recorded

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Defining the template with constant
  Logger.SetTemplate(Format('%s [%s] %s', [TLoggerTemplate.LOG_TIMESTAMP, TLoggerTemplate.LOG_LEVEL, LoggerTemplate.LOG_MESSAGE]));

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My message in the Log type ERROR');

  // Output: 2022-12-01 09:00:05.500 [INFO] My log message of type INFO
  // Output: 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

### How to define a template on specific _providers_

- It is possible to define several specific settings in each separate _provider_.

```delphi
use
  DataLogger,
  DataLogger.Provider.Console,
  DataLogger.Provider.TextFile;

begin
  // Console format '${timestamp} [${level}] ${message}'
  Logger.AddProvider(
    TProviderConsole.Create
    .SetTemplate('${timestamp} [${level}] ${message}')
  );

  // Format of text file '${timestamp} - ${message}'
  Logger.AddProvider(
    TProviderTextFile.Create
    .SetTemplate('${timestamp} - ${message}')
  );

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My message in the Log type ERROR');

  // Output Console:
  // 2022-12-01 09:00:05.500 [INFO] My log message of type INFO
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  // Output TextFile:
  // 2022-12-01 09:00:05.500 - My message in the Log of type INFO
  // 2022-12-01 09:00:05.600 - My message in the Log of type ERROR

  Readln;
end.
```

## SetFormatTimestamp

Changes the format of the TimeStamp.

- Default TimeStamp format: `yyyy-mm-dd hh:mm:ss.zzz`

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the timestamp format
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss')

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Defining the template with constant
  Logger.SetTemplate(Format('%s [%s] %s', [TLoggerTemplate.LOG_TIMESTAMP, TLoggerTemplate.LOG_LEVEL, LoggerTemplate.LOG_MESSAGE]));

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My message in the Log type ERROR');

  // Output: 12/01/2022 09:00:05 [INFO] My log message of type INFO
  // Output: 12/01/2022 09:00:05 [ERROR] My log message of type ERROR

  Readln;
end.
```

## Level

DataLogger has these levels to generate _logs_:

```delphi
  Logger.Trace('');
  Logger.Debug('');
  Logger.Info('');
  Logger.Success('');
  Logger.Warn('');
  Logger.Error('');
  Logger.Fatal('');
  Logger.Custom('');

  // simplified mode
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
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Defining the Level
  Logger.SetLevelName(TLoggerLevel.Info, 'INFORMATION');
  Logger.SetLevelName(TLoggerLevel.Warn, ' WARNING ');

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.Warn('My log message type WARN');

  // Output: 2022-12-01 09:00:05.600 [INFORMATION] My log message of type INFO
  // Output: 2022-12-01 09:00:05.600 [ WARNING ] My log message of type WARN

  Readln;
end.
```

## SetLevel

It is possible to show only _logs_ from a defined _level_, based on `TLoggerLevel`.

SetLevel default value = `TLoggerLevel.All`

### TLoggerLevel

- When defining a level, only the chosen option and its superior types will be displayed.
- Ex: `Logger.SetLevel(TLoggerLevel.Warn);` - Only _logs_ with type `Warn / Error / Fatal / Custom` will be registered.

```delphi
  TLoggerLevel.All = 'Used for internal operations'
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
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Defining the Level
  Logger.SetLevel(TLoggerLevel.Warn);

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My message in the Log type ERROR');

  // Output: 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## SetDisableLevel

It is possible to disable some _levels_ of logging, based on `TLoggerLevel`.

SetDisableLevel default value = `[]`

- When disabled, only the options that are not disabled will be displayed.
- Ex: `Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]);` - Only _logs_ with type `Tracer / Debug / Success / Error / Fatal / Custom` will be registered.

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Setting the DisableLevel
  Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]);

  // Generating the logs
  Logger.Debug('My log message of type DEBUG');
  Logger.Info('My log message of type INFO');
  Logger.Warn('My log message type WARN');
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // 2022-12-01 09:00:05.500 [DEBUG] My log message of type DEBUG
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## SetOnlyLevel

It is possible to show only certain _levels_ of the log, based on `TLoggerLevel`.

SetOnlyLevel default value = `[TLoggerLevel.All]`

- When defined, only registered options will be displayed.
- Ex: `Logger.SetOnlyLevel([TLoggerLevel.Error]);` - Only _logs_ with type `Error` will be registered.

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Setting the OnlyLevel
  Logger.SetOnlyLevel([TLoggerLevel.Error]);

  // Generating the logs
  Logger.Debug('My log message of type DEBUG');
  Logger.Info('My log message of type INFO');
  Logger.Warn('My log message type WARN');
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## SetLogException

Is it possible to catch exceptions thrown by _providers_

SetLogException default value = `nil`

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Defining the LogException
  Logger.SetLogException(
    procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetriesCount: Integer)
    begin
      // Sender - Provider that threw the exception, to view - Sender.ClassName

      // LogItem - Contains all log information

      // E - Contains the exception information

      // RetriesCount - Contains the number of attempts made
      // If the value is changed to -1, the process is interrupted
    end
  );

  // Generating the log
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## SetMaxRetries

It is possible to set the number of attempts that _Provider_ should try when saving the log.

SetMaxRetries default value = `5`

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Setting the maximum attempts
  Logger.SetMaxRetries(5);

  // Generating the log
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## SetIgnoreTemplate

It is possible to ignore the Template and save all data generated by DataLogger;

```delphi
  SetIgnoreTemplate({1}, {2}, {3}, {4});

  Parameters:
    {1} = (Boolean) = Define whether to ignore the Template.
    {2} = (string) = Define which text will separate the information, similar to CSV.
    {3} = (Boolean) = Define whether to show the keywords of each value.
    {4} = (string) = Define which text should separate the keyword from the value.

  Logger.SetIgnoreTemplate(True, '|', True, ' -> ');

  {keyword} = "timestamp"
  {separator_keyword} = " -> "
  {value} = "2022-09-15T14:39:38.896-03:00"
  {separator} = " | "

  // output timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896
```

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Ignoring log format
  Logger.SetIgnoreTemplate(True, '|', True, ' -> ');

  // Generating the log
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896 | name -> | sequence -> 1 | thread_id -> 3804 | level -> Trace | level_value -> 1 | tag -> | message -> My Trace | app_name -> ProviderTextFile | app_version -> 1.0.0.0 | app_path -> C:\Github\DataLogger\Samples\TextFile\Win32\Debug | app_size -> 13.24 MB | computer_name -> DESKTOP-7RP1H3K | username -> daniel | os_version -> Windows 10 (Version 21H2, OS Build 19044.1889, 64-bit Edition) | process_id -> 13608 | ip_local -> 192.168.56.1

  Readln;
end.
```

## SetName

You can define a name for the _DataLogger_ which can be displayed in the log record. This name can be used to differentiate _DataLogger_ when there are more than one instance.

SetName default value = `EmptyStr`

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${name} ${timestamp} [${level}] ${message}');

  // Defining the name
  Logger.SetName('MyLogger');

  // Generating the log
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## SetLiveMode

It is possible to save the logs in real time, so the next statement of your code will only continue after the log is saved!

Currently, the logs are recorded in memory and then saved without crashing the application.

SetLiveMode default value = `false`

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Defining the template
  Logger.SetTemplate('${name} ${timestamp} [${level}] ${message}');

  // Defining the LiveMode
  Logger.SetLiveMode(True);

  // Generating the log
  Logger.Error('My message in the Log type ERROR');

  // Output:
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

## Transaction

It is possible to work with _Transaction_, in the same way it is used in other components with database connection.

The use of this procedure can be applied to the following situation;

Let's tell a little story:

> We have a _procedure_ that is executing, in each step a lot of log information is generated, by custom we always save this information, this makes our text file, for example, too big. <br /> Now imagine being able to save the data only if there was an error during execution or when it was really necessary to save the data.

### How to enable

Activating the use of _Transaction_ must be done by _Provider_ with the `UseTransaction(True)` function.

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

Commits the recording of all logs in the transaction.

### RollbackTransaction

Cancels all logs generated in the transaction.

### Example with Simple Transaction

[Sample with Transaction](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction)

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Starting the transaction
  Logger.StartTransaction;

  Writeln('Started the transaction');

  // Generating the log
  Logger.Info('My log message type INFO 1 ');
  Logger.Info('My log message type INFO 2');
  Logger.Info('My log message type INFO 3');
  Logger.Info('My log message type INFO 4');
  Logger.Info('My log message of type INFO 5');
  Logger.Info('My log message type INFO 6');
  Logger.Info('My log message type INFO 7');
  Logger.Info('My log message type INFO 8');
  Logger.Info('My log message type INFO 9');

  Writeln('Finished Logging');

  // Making the commit
  Logger.CommitTransaction;
  Writeln('You made the commits');

  Readln;
end.
```

### Example with _Transation_ Nested

[Sample with Nested Transaction](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction%20Nested)

Nested _transactions_ give the possibility of greater management when saving information.

- Only one observation should be remembered, the final record will only be saved if there was a _commit_ of the parent transaction, that is, the transaction that started the whole process; <br /> If the parent transaction has been rolled back, the child transactions that were _committed_ will be ignored!

```delphi
use
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Defining the template
  Logger.SetTemplate('${timestamp} [${level}] ${message}');

  // Starting the transaction
  Logger.StartTransaction;
  try
    // Generating the log
    Logger.Info('My log message type INFO 1 ');
    Logger.Info('My log message type INFO 2');
    Logger.Info('My log message type INFO 3');
    Logger.Info('My log message type INFO 4');

    // Starting 2 transaction
    Logger.StartTransaction;
    try
      Logger.Info('My log message of type INFO 5');
      Logger.Info('My log message type INFO 6');
    finally
      // Rolling back the second transaction
      Logger.RollbackTransaction;
    end;

    Logger.Info('My log message type INFO 7');
    Logger.Info('My log message type INFO 8');
    Logger.Info('My log message type INFO 9');
  finally
    // Making the commit
    Logger.CommitTransaction;
  end;

  Readln;
end.

```