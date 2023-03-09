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

### To install in your project using [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/dliocode/datalogger
```

### Manual Installation

Add the following folders to your project, under *Project > Options > Delphi Compiler > Search path*

```
../src/Core
../src/Providers
```

## 📌 Índice

* [How to use](#how-to-use)
* [Providers](#providers)
* [Dependencies](#dependencies)
* [Examples](#examples)
  * [Standard Use](#standard-use)
  * [Create a new DataLogger instance](#create-a-new-datalogger-instance)
  * [Basic DataLogger](#basic-datalogger)
  * [Generate a log for a specific provider](#generate-a-log-for-a-specific-provider)
  * [Custom](#Custom)
  * [Log Format](#log-format)
    * [LogFormat](#logformat)
    * [How to set a format](#how-to-set-a-format)
    * [How to set a format for specific providers](#how-to-set-a-format-for-specific-providers)
    * [How to change the timestamp format](#how-to-change-the-timestamp-format)
    * [Tag types for the log format](#tag-types-for-the-log-format)
  * [Level](#level)
  * [Disable Level](#disable-Level)
  * [Only Level](#only-Level)
  * [Log Exception](#log-exception)
  * [Max Retries](#max-retries)
  * [IgnoreLogFormat](#ignorelogformat)
  * [Name](#name)
  * [LiveMode](#livemode)
  * [Transaction](#transaction)

## How to use

There are different ways to use DataLogger.

Directly from the ``` Logger ``` or by instantiating your own ``` TDataLogger ```.

[Samples](https://github.com/dliocode/datalogger/tree/main/Samples)

The first example is just intended to be a convenient shared log to use across your application.

* **Add the following to your Uses clause**: 
```
uses DataLogger;
``` 

## Providers


A **_provider_** is essentially for storing your logs.
Each instance of a TDataLogger can have several **_providers_** configured.

Here is a list of **56 _providers_** available:

 | Nome | Uses | Samples | 
 | ------ | ------ | --------- | 
 | [AWS CloudWatch](https://aws.amazon.com/cloudwatch) | DataLogger.Provider.AWS.CloudWatch | [AWS CloudWatch](https://github.com/dliocode/datalogger/tree/main/Samples/AWSCloudWatch) | 
 | [Axiom](https://axiom.co/) | DataLogger.Provider.Axiom | [Axiom](https://github.com/dliocode/datalogger/tree/main/Samples/Axiom) | 
 | [CallMeBot](https://www.callmebot.com/) | DataLogger.Provider.CallMeBot.WhatsApp | [CallMeBot](https://github.com/dliocode/datalogger/tree/main/Samples/CallMeBot) |  
 | Console | DataLogger.Provider.Console | [Console](https://github.com/dliocode/datalogger/tree/main/Samples/Console)<br /> [Console Simple](https://github.com/dliocode/datalogger/tree/main/Samples/Console%20-%20Simple)| 
 | [Coralogix](https://coralogix.com/) | DataLogger.Provider.Coralogix | [Coralogix](https://github.com/dliocode/datalogger/tree/main/Samples/Coralogix) |  
 | CSV | DataLogger.Provider.CSV | [CSV](https://github.com/dliocode/datalogger/tree/main/Samples/CSV) |  
 | [Datadog](https://www.datadoghq.com/) | DataLogger.Provider.Datadog | [Datadog](https://github.com/dliocode/datalogger/tree/main/Samples/Datadog) |  
 | [Datalust](https://datalust.co/) | DataLogger.Provider.Datalust | [Datalust](https://github.com/dliocode/datalogger/tree/main/Samples/Datalust) |  
 | [Discord](https://discord.com/developers/docs/resources/webhook) | DataLogger.Provider.Discord.Hook | [DiscordHook](https://github.com/dliocode/datalogger/tree/main/Samples/DiscordHook) | 
 | [Dynatrace](https://www.dynatrace.com/) | DataLogger.Provider.Dyatrace | [Dynatrace](https://github.com/dliocode/datalogger/tree/main/Samples/Dynatrace) |  
 | [ElasticSearch](https://www.elastic.co/pt/what-is/elasticsearch) | DataLogger.Provider.ElasticSearch | [ElasticSearch](https://github.com/dliocode/datalogger/tree/main/Samples/ElasticSearch) | 
 | [Elmah](https://elmah.io/) | DataLogger.Provider.Elmah | [Elmah](https://github.com/dliocode/datalogger/tree/main/Samples/Elmah) | 
 | Email | DataLogger.Provider.Email | [Email](https://github.com/dliocode/datalogger/tree/main/Samples/Email) | 
 | EventLog | DataLogger.Provider.EventLog | [EventLog](https://github.com/dliocode/datalogger/tree/main/Samples/EventLog) | 
 | Events | DataLogger.Provider.Events | [Events](https://github.com/dliocode/datalogger/tree/main/Samples/Events)<br />[Events - With DataSet](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20DataSet)<br />[Events - With SQLLite](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20SQLLite) | 
 | [Firebase](https://firebase.google.com/) | DataLogger.Provider.Firebase.RealtimeDatabase | [Firebase](https://github.com/dliocode/datalogger/tree/main/Samples/Firebase) | 
 | [Grafana](https://grafana.com/) | DataLogger.Provider.Grafana.Loki<br />DataLogger.Provider.Grafana.OnCall.WebHook | [Grafana](https://github.com/dliocode/datalogger/tree/main/Samples/Grafana) |  
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
 | [Mattermost](https://mattermost.com/) | DataLogger.Provider.Mattermost<br />DataLogger.Provider.Mattermost.Hooks | [Mattermost](https://github.com/dliocode/datalogger/tree/main/Samples/Mattermost) <br /> [MattermostHook](https://github.com/dliocode/datalogger/tree/main/Samples/MattermostHook) | 
 | Memo | DataLogger.Provider.Memo | [Memo](https://github.com/dliocode/datalogger/tree/main/Samples/Memo)<br />[Memo and TexFile](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20and%20TexFile)<br />[Memo - Only Error/VCL](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20-%20Only%20Error/VCL) | 
 | Memory | DataLogger.Provider.Memory | [Memory](https://github.com/dliocode/datalogger/tree/main/Samples/Memory) | 
 | [Mezmo](https://www.mezmo.com/) | DataLogger.Provider.Mezmo | [Mezmo](https://github.com/dliocode/datalogger/tree/main/Samples/Mezmo) | 
 | [MongoDB](https://www.mongodb.com) | DataLogger.Provider.MongoDB.Cloud | [MongoDB](https://github.com/dliocode/datalogger/tree/main/Samples/MongoDB) |  
 | [Ntfy](https://ntfy.sh) | DataLogger.Provider.Ntfy | [Ntfy](https://github.com/dliocode/datalogger/tree/main/Samples/Ntfy) |   
 | [NewRelic](https://newrelic.com) | DataLogger.Provider.NewRelic | [NewRelic](https://github.com/dliocode/datalogger/tree/main/Samples/NewRelic) |  
 | Notification | DataLogger.Provider.Notification | [Notification](https://github.com/dliocode/datalogger/tree/main/Samples/Notification) | 
 | [Novu](https://novu.co/) | DataLogger.Provider.Novu | [Novu](https://github.com/dliocode/datalogger/tree/main/Samples/Novu) |   
 | OutputDebugString | DataLogger.Provider.OutputDebugString | [OutputDebugString](https://github.com/dliocode/datalogger/tree/main/Samples/OutputDebugString) | 
 | [PaperTrail](https://papertrailapp.com) | DataLogger.Provider.PaperTrail | [PaperTrail](https://github.com/dliocode/datalogger/tree/main/Samples/PaperTrail) | 
 | [Postmark](https://postmarkapp.com) | DataLogger.Provider.Postmark.Api | [Postmark](https://github.com/dliocode/datalogger/tree/main/Samples/Postmark-Api) | 
 | [RabbitMQ](https://github.com/danieleteti/delphistompclient) | DataLogger.Provider.RabbitMQ | [RabbitMQ](https://github.com/dliocode/datalogger/tree/main/Samples/RabbitMQ) | 
 | [Redis](https://github.com/danieleteti/delphiredisclient) | DataLogger.Provider.Redis | [Redis](https://github.com/dliocode/datalogger/tree/main/Samples/Redis) | 
 | Rest | DataLogger.Provider.REST.HTTPClient<br />DataLogger.Provider.REST.Indy<br />DataLogger.Provider.REST.NetHTTPClient | [Rest](https://github.com/dliocode/datalogger/tree/main/Samples/REST) | 
 | RichEdit | DataLogger.Provider.RichEdit | [RichEdit](https://github.com/dliocode/datalogger/tree/main/Samples/RichEdit) | 
 | [Sematext](https://sematext.com/) | DataLogger.Provider.Sematext.Logs | [Sematext-Logs](https://github.com/dliocode/datalogger/tree/main/Samples/Sematext-Logs) | 
 | [SendEmail](https://github.com/dliocode/sendemail) | DataLogger.Provider.SendEmail | [SendEmail](https://github.com/dliocode/datalogger/tree/main/Samples/SendEmail) | 
 | [SendGrid](https://sendgrid.com/) | DataLogger.Provider.SendGrid.WebApi | [SendGridWebApi](https://github.com/dliocode/datalogger/tree/main/Samples/SendGrid-WebApi) | 
 | [Slack](https://slack.com/) | DataLogger.Provider.Slack | [Slack](https://github.com/dliocode/datalogger/tree/main/Samples/Slack) | 
 | [Splunk](https://www.splunk.com/) | DataLogger.Provider.Splunk | [Splunk](https://github.com/dliocode/datalogger/tree/main/Samples/Splunk) | 
 | Socket | DataLogger.Provider.Socket | [Socket](https://github.com/dliocode/datalogger/tree/main/Samples/Socket) | 
 | [SumoLogic](https://www.sumologic.com/) | DataLogger.Provider.SumoLogic | [SumoLogic](https://github.com/dliocode/datalogger/tree/main/Samples/SumoLogic) | 
 | SysLog | DataLogger.Provider.SysLog<br />DataLogger.Provider.SysLog.Indy | [SysLog](https://github.com/dliocode/datalogger/tree/main/Samples/SysLog)<br />[SysLog Indy](https://github.com/dliocode/datalogger/tree/main/Samples/SysLogIndy) | 
 | [Telegram](https://core.telegram.org/) | DataLogger.Provider.Telegram | [Telegram](https://github.com/dliocode/datalogger/tree/main/Samples/Telegram) | 
 | TextFile | DataLogger.Provider.TextFile | [TextFile](https://github.com/dliocode/datalogger/tree/main/Samples/TextFile) | 
 | [Twilio](https://www.twilio.com/) | DataLogger.Provider.Twilio.SMS<br />DataLogger.Provider.Twilio.WhatsApp | [Twilio](https://github.com/dliocode/datalogger/tree/main/Samples/Twilio) |  
 | [Z-API](https://z-api.io/) | DataLogger.Provider.ZAPI.WhatsApp | [ZAP-API](https://github.com/dliocode/datalogger/tree/main/Samples/ZAPI) |  


## Dependencies


These dependencies occur when using some _providers_

 | Provider | Dependence | 
 | ---------- | ------------ | 
 | DataLogger.Provider.RabbitMQ | [RabbitMQ](https://github.com/danieleteti/delphistompclient) | 
 | DataLogger.Provider.Redis | [Redis](https://github.com/danieleteti/delphiredisclient) | 
 | DataLogger.Provider.SendEmail | [SendEmail](https://github.com/dliocode/sendemail) | 

## Extra Information

### Android Platform: 

Permission required __ACCESS_WIFI_STATE__ - used to capture the MAC Address of the device.

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

### Basic DataLogger

```delphi
uses
  DataLogger,
  DataLogger.Simple,
  DataLogger.Provider.Console;

begin
  // Defini o provider
  Logger.AddProvider(TProviderConsole.Create);

  //Define in DataloggerSimple the log instance to be used
  TDataLoggerSimple.SetDataLogger(Logger);

  // Just use basic mode..
  Trace('My trace message');
  Debug('My debug message');
  Info('My info message');
  Success('My success message');
  Warn('My warning message');
  Error('My error message');
  Fatal('My fatal message');
  Custom('My Type', 'My custom message');

  Readln;
end.

```

## Custom

The _Custom_ method is the way to define naming for your own _level_.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  Logger.Custom('Custom Level', 'My message with custom level!');

  // Output: 2022-12-01 09:00:05.500 [Custom Level] My message with custom level!	

  Readln;
end.
```

## Log Format

Default log format:

```
${timestamp} [TID ${thread_id}] [PID ${process_id}] [SEQ ${sequence}] [${level}] [${tag}] ${message}
```

### LogFormat 


There are some constants that can be used:

```delphi
TLoggerFormat.LOG_NAME = '${name}';
TLoggerFormat.LOG_SEQUENCE = '${sequence}';
TLoggerFormat.LOG_TIMESTAMP = '${timestamp}';
TLoggerFormat.LOG_TIMESTAMP_ISO8601 = '${timestamp_iso8601}';
TLoggerFormat.LOG_TIMESTAMP_UNIX = '${timestamp_unix}';
TLoggerFormat.LOG_THREADID = '${thread_id}';
TLoggerFormat.LOG_PROCESSID = '${process_id}';
TLoggerFormat.LOG_LEVEL = '${level}';
TLoggerFormat.LOG_LEVEL_VALUE = '${level_value}';
TLoggerFormat.LOG_TAG = '${tag}';
TLoggerFormat.LOG_MESSAGE = '${message}';

TLoggerFormat.LOG_APPNAME = '${app_name}';
TLoggerFormat.LOG_APPPATH = '${app_path}';
TLoggerFormat.LOG_APPVERSION = '${app_version}';
TLoggerFormat.LOG_APPSIZE = '${app_size}';

TLoggerFormat.LOG_COMPUTERNAME = '${computer_name}';
TLoggerFormat.LOG_USERNAME = '${username}';
TLoggerFormat.LOG_OSVERSION = '${os_version}';
TLoggerFormat.LOG_IP_LOCAL = '${ip_local}';
TLoggerFormat.LOG_MAC_ADDRESS = '${mac_address}';
```

### How to set a format


Always set the log format after adding all _providers_, that way it will apply to all of them.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Deine the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Usando constant
  // Logger.SetLogFormat(Format('%s [%s] %s', 
  //   [TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_LEVEL,  TLoggerFormat.LOG_MESSAGE])
  // );

  // Generating the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My message of type ERROR');
  
  // Output: 2022-12-01 09:00:05.500 [INFO] My log message of type INFO
  // Output: 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR

  Readln;
end.
```

### How to set a format for specific _providers_

* It is possible to define several specific settings for each separate _provider_.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console,
  DataLogger.Provider.TextFile;

begin
  // Format for console '${timestamp} [${level}] ${message}'
  Logger.AddProvider(TProviderConsole.Create.SetLogFormat('${timestamp} [${level}] ${message}'));

  // Format for text file '${timestamp} - ${message}'  
  Logger.AddProvider(TProviderTextFile.Create.SetLogFormat('${timestamp} - ${message}'));  

  // Generating the logs
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output Console: 
  // 2022-12-01 09:00:05.500 [INFO] My log message of type INFO
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  // Output TextFile: 
  // 2022-12-01 09:00:05.500 - My log message of type INFO
  // 2022-12-01 09:00:05.600 - My log message of type ERROR 

  Readln;
end.
```

### How to change the TimeStamp format
* Default TimeStamp format: `yyyy-mm-dd hh:mm:ss.zzz`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Define the Timestamp format
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss')

  // Define the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Using a constant
  // Logger.SetLogFormat(Format('%s [%s] %s', 
  //   [TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_LEVEL,  TLoggerFormat.LOG_MESSAGE])
  // );

  // Generate the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My log message of type ERROR');
  
  // Output: 01/12/2022 09:00:05 [INFO] My log message of type INFO
  // Output: 01/12/2022 09:00:05 [ERROR] My log message of type ERROR  

  Readln;
end.
```

### Tag types to create log format
* Basic

```delphi
// Displays the name of the log. ex: Logger.SetName('SERVICE_REMOTE')
TLoggerFormat.LOG_NAME;

// Displays the sequence in which the log was generated
TLoggerFormat.LOG_SEQUENCE;

// Displays the date and time it was generated, using SetFormatTimestamp
TLoggerFormat.LOG_TIMESTAMP;

// Displays the date and time it was generated, using ISO8601 format.
TLoggerFormat.LOG_TIMESTAMP_ISO8601;

// Displays the date and time it was generated, using UNIX format.
TLoggerFormat.LOG_TIMESTAMP_UNIX;

// Show the ID of the thread which generated the log.
TLoggerFormat.LOG_THREADID;

// Displays the process ID of the app which created the log
TLoggerFormat.LOG_PROCESSID;

// Displays the log level, these are one of the following: TRACE / DEBUG / INFO / SUCCESS / WARN / ERROR / FATAL
TLoggerFormat.LOG_LEVEL;

// Displays the numeric log level, these are one of the following:: 1=TRACE / 2=DEBUG / 3=INFO / 4=SUCCESS / 5=WARN / 6=ERROR / 7=FATAL / 8=CUSTOM
TLoggerFormat.LOG_LEVEL_VALUE;

// Exibe a tag do log, essa informação é preenchida a após a mensagem; Ex: Logger.Debug('Minha mensagem','Minha Tag');
TLoggerFormat.LOG_TAG;

Displays the log tag, this information is filled in after the message; Ex: Logger.Debug('My Message','My Tag');
TLoggerFormat.LOG_MESSAGE;
```

* Specials:

```delphi
// Display the name of the app
TLoggerFormat.LOG_APPNAME;

// Display the version of the app
TLoggerFormat.LOG_APPVERSION;

// Display the path to the app's executable
TLoggerFormat.LOG_APPPATH

// Display the file size of the app
TLoggerFormat.LOG_APPSIZE

// Display the name of the computer on which the app is running
TLoggerFormat.LOG_COMPUTERNAME;

// Display the user's login name
TLoggerFormat.LOG_USERNAME

// Display the version of the operating system
TLoggerFormat.LOG_OSVERSION

// Display the IP address of the computer
TLoggerFormat.LOG_IP_LOCAL

```

## SetLevel

It is possible to show only _logs_ from a defined _level_, based on ```TLoggerLevel```.

SetLevel default value = ```TLoggerLevel.All```

### TLoggerLevel

When a level is defined, only the chosen option and its superior/greater types will be displayed.
* Ex: ``` Logger.SetLevel(TLoggerLevel.Warn); ``` - Only _logs_ with type ``` Warn / Error / Fatal / Custom ``` will be logged.

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
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // define the log format
    Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Define the log level
  Logger.SetLevel(TLoggerLevel.Warn);

  // Generate the logs
  Logger.Info('My log message of type INFO');
  Logger.Error('My log message of type ERROR');
  
  // Output: 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## Disable Level

It is possible to disable some _levels_ of logging, based on ```TLoggerLevel```.

SetDisableLevel default value = ```[]```

### SetDisableLevel

* When disabled, only the options that are not disabled will be displayed.
* Ex: ``` Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]); ``` - Only _logs_ with type ``` Tracer / Debug / Success / Error / Fatal / Custom ``` will be logged.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Define the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Set the DisableLevel
  Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]);

  // Generate the logs
  Logger.Debug('My log message of type DEBUG');
  Logger.Info('My log message of type INFO');
  Logger.Warn('My log message of type WARN');  
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.500 [DEBUG] My log message of type DEBUG  
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## Only Level

It is possible to show only certain _levels_ of the log, based on ```TLoggerLevel```.

SetOnlyLevel default value = ```[TLoggerLevel.All]```

### SetOnlyLevel

* When set, only registered options will be displayed.
* Ex: ``` Logger.SetOnlyLevel([TLoggerLevel.Error]); ``` - only logs with type ``` Error ``` will be displayed.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Define the format of the log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Set the OnlyLevel value
  Logger.SetOnlyLevel([TLoggerLevel.Error]);

  // Generate the logs
  Logger.Debug('My log message of type DEBUG');
  Logger.Info('My log message of type INFO');
  Logger.Warn('My log message of type WARN');  
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## Log Exception

Is it possible to catch exceptions thrown by specific _providers_

SetLogException default value = ```nil```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Set the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Define the LogException type
  Logger.SetLogException(
    procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetriesCount: Integer)
    begin 
      // Sender - Provider that threw the exception, to view specifics use - Sender.ClassName

      // LogItem - Contains all log information

      // E - Contains the exception information

      // RetriesCount - Contains the number of the attempt made
      // If the value is changed to -1, the process is interrupted
    end
  );

  // Generate the log
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## Max Retries

It is possible to set the number of attempts that _Provider_ should try when saving the log.

SetMaxRetries default value = ```5```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Define the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Set the maximum number of retries
  Logger.SetMaxRetries(5);

  // Create the log
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## IgnoreLogFormat

It is possible to ignore the LogFormat format and save all the data generated by the DataLogger;

```delphi  
SetIgnoreLogFormat({1}, {2}, {3}, {4});
```delphi  

```delphi  
  Parameters:
    {1} = (Boolean) = Define whether to ignore LogFormat.
    {2} = (string) = Define which text will separate the information, similar to CSV.
    {3} = (Boolean) = Define whether to show the keywords of each value.
    {4} = (string) = Define which text should separate the keyword from the value.

  Logger.SetIgnoreLogFormat(True, '|', True, ' -> '); 

  {keyword}           = "timestamp"
  {keyword_separador} = " -> "
  {value}                   = "2022-09-15T14:39:38.896-03:00"
  {separator}               = " | " 
  
  // output timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896                 
```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Set the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Ignore log format
  Logger.SetIgnoreLogFormat(True, '|', True, ' -> ');

  // Generate the log
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896 | name ->  | sequence -> 1 | thread_id -> 3804 | level -> Trace | level_value -> 1 | tag ->  | message -> My Trace | app_name -> ProviderTextFile | app_version -> 1.0.0.0 | app_path -> C:\Github\DataLogger\Samples\TextFile\Win32\Debug | app_size -> 13,24 MB | computer_name -> DESKTOP-7RP1H3K | username -> danil | os_version -> Windows 10 (Version 21H2, OS Build 19044.1889, 64-bit Edition) | process_id -> 13608 | ip_local -> 192.168.56.1

  Readln;
end.
```

## Name

You can define a name for the _DataLogger_ which can be displayed in the log record. This name can be used to differentiate _DataLogger_ when there is more than one instance.

SetName default value = ```''```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Set the log format
  Logger.SetLogFormat('${name} ${timestamp} [${level}] ${message}');

  // Set a name for the log
  Logger.SetName('MyLogger');

  // Create the log
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## LiveMode


It is possible to save the logs in real time, so the next statement of your code will only continue after the log is saved!

Currently, the logs are recorded in memory and then saved without crashing the application.

SetLiveMode default value = ```false```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Define the log format
  Logger.SetLogFormat('${name} ${timestamp} [${level}] ${message}');

  // Turn on LiveMode
  Logger.SetLiveMode(True);

  // Create the log
  Logger.Error('My log message of type ERROR');
  
  // Output: 
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] My log message of type ERROR  

  Readln;
end.
```

## Transaction

It is possible to work with _Transaction_, in the same way it is used in other components with database connection.

The use of this procedure can be applied to the following situation;

Let's tell a little story:

> We have a _procedure_ that is executing, in each step several log information is generated, by custom we always save this information, this makes our text file, for example, too big. <br /> Now imagine being able to save the data only if there was an error during execution or when it was really necessary to save the data.

### How to enable

Activating the use of _Transaction_ must be done by _Provider_ with the ```UseTransaction(True)``` function.

Ex: ``` Logger.AddProvider(TProviderConsole.Create.UseTransaction(True)); ```

### StartTransaction

Starts a new transaction.

### CommitTransaction

Commits the recording of all logs in the transaction.

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

  // Define the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Initiate a transaction
  Logger.StartTransaction;

  Writeln('Initiated a transaction');

  // Create some log messages
  Logger.Info('My log message of type INFO 1 ');
  Logger.Info('My log message of type INFO 2');
  Logger.Info('My log message of type INFO 3');
  Logger.Info('My log message of type INFO 4');
  Logger.Info('My log message of type INFO 5');
  Logger.Info('My log message of type INFO 6');
  Logger.Info('My log message of type INFO 7');
  Logger.Info('My log message of type INFO 8');
  Logger.Info('My log message of type INFO 9');

  Writeln('End of log messages');

  // Fazendo o commit
  Logger.CommitTransaction;
  Writeln('The transaction has been committed');

  Readln;
end.
```
### Exemplo with a nested _Transation_

[Sample with nested Transaction](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction%20Aninhada)


Nested _transactions_ give the possibility of greater management when saving information.
* Just one thing should be remembered, the final record will only be saved if there was a _commit_ of the parent transaction, that is, the transaction that started the whole process; <br /> If the parent transaction has been rolled back, the child transactions that were _committed_ will be ignored!

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Set the log format
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Start a transaction
  Logger.StartTransaction;
  try
    // Create some log messages
    Logger.Info('My log message of type INFO 1 ');
    Logger.Info('My log message of type INFO 2');
    Logger.Info('My log message of type INFO 3');
    Logger.Info('My log message of type INFO 4');

    // Start a second transaction
    Logger.StartTransaction;
    try
      Logger.Info('My log message of type INFO 5');
      Logger.Info('My log message of type INFO 6');
    finally
      // Rollback the contents of the second transaction
      Logger.RollbackTransaction;
    end;

    Logger.Info('My log message of type INFO 7');
    Logger.Info('My log message of type INFO 8');
    Logger.Info('My log message of type INFO 9');
  finally
    // Commit the transaction details
    Logger.CommitTransaction;
  end;

  Readln;
end.

```
