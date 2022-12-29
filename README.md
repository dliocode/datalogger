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

## ⚙️ Instalação

### Para instalar em seu projeto usando [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/dliocode/datalogger
```

### Instalação Manual

Adicione as seguintes pastas ao seu projeto, em *Project > Options > Delphi Compiler > Search path*

```
../src/Core
../src/Providers
```

## 📌 Índice

* [Como usar](#como-usar)
* [Providers](#providers)
* [Dependências](#dependencias)
* [Exemplos](#exemplos)
  * [Uso Padrão](#uso-padrao)
  * [Criar uma nova instância do DataLogger](#criar-uma-nova-instancia-do-datalogger)
  * [DataLogger Simple](#datalogger-simple)
  * [Gerar log para um Provider Específico](#gerar-log-para-um-provider-específico)
  * [Custom](#Custom)
  * [Formato do Log](#formato-do-log)
    * [LogFormat](#logformat)
    * [Como definir um formato](#como-definir-um-formato)
    * [Como definir um formato em providers específicos](#como-definir-um-formato-em-providers-especificos)
    * [Como mudar o formato do TimeStamp](#como-mudar-o-formato-do-timestamp)
    * [Tipos de Tag para criar o formato do log](#tipos-de-tag-para-criar-o-formato-do-log)
  * [Level](#level)
  * [Disable Level](#disable-Level)
  * [Only Level](#only-Level)
  * [Log Exception](#log-exception)
  * [Max Retries](#max-retries)
  * [InitialMessage](#initialmessage)
  * [FinalMessage](#finalmessage)
  * [IgnoreLogFormat](#ignorelogformat)
  * [Name](#name)
  * [LiveMode](#livemode)
  * [Transaction](#transaction)

## Como usar

Existem duas maneiras diferentes de usar o DataLogger: 
Diretamente pelo ``` Logger ``` ou instanciando seu próprio ``` TDataLogger ```.

[Samples](https://github.com/dliocode/datalogger/tree/main/Samples)

O primeiro destina-se apenas a ser um log compartilhado conveniente para ser usado em todo o seu aplicativo, se você escolher.

* **Uses necessária**: 
```
uses DataLogger;
``` 

## Providers

Um **_provider_** serve essencialmente para armazenar seus logs. 
Cada instância de um TDataLogger pode ter vários **_providers_** configurados.

Aqui temos uma lista com **56 _providers_** disponíveis:

 | Nome | Uses | Samples | 
 | ------ | ------ | --------- | 
 | [AWS CloudWatch](https://aws.amazon.com/cloudwatch) | DataLogger.Provider.AWS.CloudWatch | [AWS CloudWatch](https://github.com/dliocode/datalogger/tree/main/Samples/AWSCloudWatch) | 
 | [Axiom](https://axiom.co/) | DataLogger.Provider.Axiom | [Axiom](https://github.com/dliocode/datalogger/tree/main/Samples/Axiom) | 
 | Console | DataLogger.Provider.Console | [Console](https://github.com/dliocode/datalogger/tree/main/Samples/Console)<br /> [Console Simple](https://github.com/dliocode/datalogger/tree/main/Samples/Console%20-%20Simple)| 
 | [Coralogix](https://coralogix.com/) | DataLogger.Provider.Coralogix | [Coralogix](https://github.com/dliocode/datalogger/tree/main/Samples/Coralogix) |  
 | CSV | DataLogger.Provider.CSV | [CSV](https://github.com/dliocode/datalogger/tree/main/Samples/CSV) |  
 | [Datadog](https://www.datadoghq.com/) | DataLogger.Provider.Datadog | [Datadog](https://github.com/dliocode/datalogger/tree/main/Samples/Datadog) |  
 | [Datalust](https://datalust.co/) | DataLogger.Provider.Datalust | [Datalust](https://github.com/dliocode/datalogger/tree/main/Samples/Datalust) |  
 | [Discord](https://discord.com/developers/docs/resources/webhook) | DataLogger.Provider.Discord.Hook | [DiscordHook](https://github.com/dliocode/datalogger/tree/main/Samples/DiscordHook) | 
 | [Dynatrace](https://www.dynatrace.com/) | DataLogger.Provider.Dyatrace | [Dynatrace](https://github.com/dliocode/datalogger/tree/main/Samples/Dynatrace) |  
 | [ElasticSearch](https://www.elastic.co/pt/what-is/elasticsearch) | DataLogger.Provider.ElasticSearch | [ElasticSearch](https://github.com/dliocode/datalogger/tree/main/Samples/ElasticSearch) | 
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


## Dependências

Essas dependências se dá quando utilizado alguns _providers_

 | Provider | Dependence | 
 | ---------- | ------------ | 
 | DataLogger.Provider.RabbitMQ | [RabbitMQ](https://github.com/danieleteti/delphistompclient) | 
 | DataLogger.Provider.Redis | [Redis](https://github.com/danieleteti/delphiredisclient) | 
 | DataLogger.Provider.SendEmail | [SendEmail](https://github.com/dliocode/sendemail) | 

## Exemplos

### Uso Padrão

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

O _Custom_ é a forma de definir dar o nome para o seu próprio _level_.

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

## Formato do Log

Formato de log padrão: 

```
${timestamp} [TID ${thread_id}] [PID ${process_id}] [SEQ ${sequence}] [${level}] [${tag}] ${message}
```

### LogFormat 

Existe algumas constantes que podem ser utilizadas:

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

### Como definir um formato

Sempre defina o formato de log após adicionar todos os _providers_, dessa forma ele será aplicado em todos.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Usando constant
  // Logger.SetLogFormat(Format('%s [%s] %s', 
  //   [TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_LEVEL,  TLoggerFormat.LOG_MESSAGE])
  // );

  // Gerando os logs
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 2022-12-01 09:00:05.500 [INFO] Minha mensagem no Log do tipo INFO
  // Output: 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

### Como definir um formato em _providers_ específicos

* É possível definir várias configurações específicas em cada _provider_ separado. 

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console,
  DataLogger.Provider.TextFile;

begin
  // Formato do console '${timestamp} [${level}] ${message}'
  Logger.AddProvider(TProviderConsole.Create.SetLogFormat('${timestamp} [${level}] ${message}'));

  // Formato do text file '${timestamp} - ${message}'  
  Logger.AddProvider(TProviderTextFile.Create.SetLogFormat('${timestamp} - ${message}'));  

  // Gerando os logs
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output Console: 
  // 2022-12-01 09:00:05.500 [INFO] Minha mensagem no Log do tipo INFO
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  // Output TextFile: 
  // 2022-12-01 09:00:05.500 - Minha mensagem no Log do tipo INFO
  // 2022-12-01 09:00:05.600 - Minha mensagem no Log do tipo ERROR 

  Readln;
end.
```

### Como mudar o formato do TimeStamp
* Formato de TimeStamp padrão: `yyyy-mm-dd hh:mm:ss.zzz`

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do Timestamp
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss')

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Usando constant
  // Logger.SetLogFormat(Format('%s [%s] %s', 
  //   [TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_LEVEL,  TLoggerFormat.LOG_MESSAGE])
  // );

  // Gerando os logs
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 01/12/2022 09:00:05 [INFO] Minha mensagem no Log do tipo INFO
  // Output: 01/12/2022 09:00:05 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

### Tipos de Tag para criar o formato do log
* Comum 

```delphi
// Exibe o nome do log. ex: Logger.SetName('SERVICE_REMOTE')
TLoggerFormat.LOG_NAME;

// Exibe a sequencia que o log foi gerado.
TLoggerFormat.LOG_SEQUENCE;

// Exibe a data e hora que foi gerado, usado o SetFormatTimestamp
TLoggerFormat.LOG_TIMESTAMP;

// Exibe a data e hora que foi gerado, no formato ISO8601.
TLoggerFormat.LOG_TIMESTAMP_ISO8601;

// Exibe a data e hora que foi gerado, no formato UNIX.
TLoggerFormat.LOG_TIMESTAMP_UNIX;

// Exibe o Id da thread que foi gerado o log.
TLoggerFormat.LOG_THREADID;

// Exibe o id do processo do app.
TLoggerFormat.LOG_PROCESSID;

// Exibe o level do log, sendo eles: TRACE / DEBUG / INFO / SUCCESS / WARN / ERROR / FATAL
TLoggerFormat.LOG_LEVEL;

// Exibe o level do log no formato numérico, sendo eles: 1=TRACE / 2=DEBUG / 3=INFO / 4=SUCCESS / 5=WARN / 6=ERROR / 7=FATAL / 8=CUSTOM
TLoggerFormat.LOG_LEVEL_VALUE;

// Exibe a tag do log, essa informação é preenchida a após a mensagem; Ex: Logger.Debug('Minha mensagem','Minha Tag');
TLoggerFormat.LOG_TAG;

// Exibe a mensagem do log, sem essa tag a mensagem não é exibida. Ex: Logger.Debug('Minha mensagem');
TLoggerFormat.LOG_MESSAGE;
```

* Especiais:

```delphi
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

## SetLevel

É possível mostrar somente os _logs_ a partir de um _level_ definido, com base no ```TLoggerLevel```.

SetLevel valor padrão = ```TLoggerLevel.All```

### TLoggerLevel

* Quando definido um level, será exibido somente a opção escolhida e seus tipos superiores.
* Ex: ``` Logger.SetLevel(TLoggerLevel.Warn); ``` - Será registrado somente os _logs_ com o tipo ``` Warn / Error / Fatal / Custom ```.

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

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Definindo o Level
  Logger.SetLevel(TLoggerLevel.Warn);

  // Gerando os logs
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## Disable Level

É possível desabilitar alguns _levels_ do log, com base no ```TLoggerLevel```.

SetDisableLevel valor padrão = ```[]```

### SetDisableLevel

* Quando desabilitado será exibido somente as opções que não estão desabilitadas.
* Ex: ``` Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]); ``` - Será registrado somente os _logs_ com o tipo ``` Tracer / Debug / Success / Error / Fatal / Custom ```.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Definindo o DisableLevel
  Logger.SetDisableLevel([TLoggerLevel.Info, TLoggerLevel.Warn]);

  // Gerando os logs
  Logger.Debug('Minha mensagem no Log do tipo DEBUG');
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Warn('Minha mensagem no Log do tipo WARN');  
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.500 [DEBUG] Minha mensagem no Log do tipo DEBUG  
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## Only Level

É possível mostrar somente alguns _levels_ do log, com base no ```TLoggerLevel```.

SetOnlyLevel valor padrão = ```[TLoggerLevel.All]```

### SetOnlyLevel

* Quando definido será exibido somente as opções registradas.
* Ex: ``` Logger.SetOnlyLevel([TLoggerLevel.Error]); ``` - Será registrado somente os _logs_ com o tipo ``` Error ```.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Definindo o OnlyLevel
  Logger.SetOnlyLevel([TLoggerLevel.Error]);

  // Gerando os logs
  Logger.Debug('Minha mensagem no Log do tipo DEBUG');
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Warn('Minha mensagem no Log do tipo WARN');  
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## Log Exception

É possível capturar exceções geradas pelos _providers_

SetLogException valor padrão = ```nil```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

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
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## Max Retries

É possível definir o número de tentativas que o _Provider_ deve tentar ao salvar o log.

SetMaxRetries valor padrão = ```5```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Definindo o máximo de tentativas
  Logger.SetMaxRetries(5);

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## InitialMessage

É possível definir uma mensagem fixa no inicio de uma mensagem.

SetInitialMessage valor padrão = ```''```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Definindo a mensagem inicial
  Logger.SetInitialMessage('DLIOCODE ');

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // DLIOCODE 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## FinalMessage

É possível definir uma mensagem fixa no final de uma mensagem.

SetFinalMessage valor padrão = ```''```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Definindo a mensagem final
  Logger.SetFinalMessage(' DLIOCODE');

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR DLIOCODE 

  Readln;
end.
```

## IgnoreLogFormat

É possível ignorar o formato do LogFormat e salvar todos os dados gerados pelo DataLogger;

```delphi  
SetIgnoreLogFormat({1}, {2}, {3}, {4});
```delphi  

```delphi  
  Parâmetros:
    {1} = (Boolean) = Defini se deve ignorar o LogFormat.
    {2} = (string) = Defini qual texto vai fazer a separação das informações, semelhante ao CSV.
    {3} = (Boolean) = Defini se deve mostrar as palavras chaves de cada valor.
    {4} = (string) = Defini qual texto deve separar a palavra chave do valor.

  Logger.SetIgnoreLogFormat(True, '|', True, ' -> '); 

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

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Ignorando o log format
  Logger.SetIgnoreLogFormat(True, '|', True, ' -> ');

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // timestamp -> 2022-09-15T14:39:38.896-03:00 | timestamp_format -> 2022-09-15 14:39:38.896 | name ->  | sequence -> 1 | thread_id -> 3804 | level -> Trace | level_value -> 1 | tag ->  | message -> My Trace | app_name -> ProviderTextFile | app_version -> 1.0.0.0 | app_path -> C:\Github\DataLogger\Samples\TextFile\Win32\Debug | app_size -> 13,24 MB | computer_name -> DESKTOP-7RP1H3K | username -> danil | os_version -> Windows 10 (Version 21H2, OS Build 19044.1889, 64-bit Edition) | process_id -> 13608 | ip_local -> 192.168.56.1

  Readln;
end.
```

## Name

É possível definir um nome para o _DataLogger_ que pode ser exibido no registro do log. Esse nome pode ser utilizado para diferenciar o _DataLogger_ quando a mais de uma instância.

SetName valor padrão = ```''```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${name} ${timestamp} [${level}] ${message}');

  // Definindo o name
  Logger.SetName('MyLogger');

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## LiveMode

É possível salvar os logs em tempo real, de forma que a próxima instrução do seu código só vai continuar depois que o log for salvo!

Atualmente os logs são registrados em memória e depois ele é salvo sem travar a aplicação.

SetLiveMode valor padrão = ```false```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${name} ${timestamp} [${level}] ${message}');

  // Definindo o LiveMode
  Logger.SetLiveMode(True);

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // MyLogger 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## Transaction

É possível trabalhar com _Transaction_, da mesma forma que é usado em outros componentes com conexão de banco de dados.

A utilização deste procedimento pode ser aplicado a seguinte situação;

Vamos contar uma pequena história:

> Temos uma _procedure_ que está fazendo uma execução, em cada etapa é gerado várias informações de log, por costume sempre salvamos essas informações, isso faz com que o nosso arquivo de texto por exemplo fique muito grande demais. <br /> Agora imagina poder salvar os dados somente se houve algum erro durante a execução ou quando necessário realmente salvar os dados.

### Como habilitar

A ativação do uso da _Transaction_ deve ser feita por _Provider_ com a função ```UseTransaction(True)```.

Ex: ``` Logger.AddProvider(TProviderConsole.Create.UseTransaction(True)); ```

### StartTransaction

Inicia uma nova transação.

### CommitTransaction

Confirma o registra de todos os logs na transação.

### RollbackTransaction

Cancela todos os logs gerados na transação.

### Exemplo com Transação Simples

[Sample com Transaction](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction)

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Iniciando a transaction
  Logger.StartTransaction;

  Writeln('Iniciou a transaction');

  // Gerando o log
  Logger.Info('Minha mensagem no Log do tipo INFO 1 ');
  Logger.Info('Minha mensagem no Log do tipo INFO 2');
  Logger.Info('Minha mensagem no Log do tipo INFO 3');
  Logger.Info('Minha mensagem no Log do tipo INFO 4');
  Logger.Info('Minha mensagem no Log do tipo INFO 5');
  Logger.Info('Minha mensagem no Log do tipo INFO 6');
  Logger.Info('Minha mensagem no Log do tipo INFO 7');
  Logger.Info('Minha mensagem no Log do tipo INFO 8');
  Logger.Info('Minha mensagem no Log do tipo INFO 9');

  Writeln('Terminou os Logs');

  // Fazendo o commit
  Logger.CommitTransaction;
  Writeln('Fez os commits');

  Readln;
end.
```
### Exemplo com _Transation_ Aninhada

[Sample com Transaction Aninhada](https://github.com/dliocode/datalogger/tree/main/Samples/Transaction%20Aninhada)

As _transactions_ aninhadas dá a possibilidade de um gerenciamento maior na hora de salvar as informações.
* Apenas uma observação deve ser lembrada, só será salvo o registro final se houve _commit_ da transação pai, ou seja, da transação que iniciou todo processo; <br /> Caso a transação pai, tenha feito rollback, as transações filhas que foram feitas _commit_ serão ignoradas!

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Iniciando a transaction
  Logger.StartTransaction;
  try
    // Gerando o log
    Logger.Info('Minha mensagem no Log do tipo INFO 1 ');
    Logger.Info('Minha mensagem no Log do tipo INFO 2');
    Logger.Info('Minha mensagem no Log do tipo INFO 3');
    Logger.Info('Minha mensagem no Log do tipo INFO 4');

    // Iniciando 2 transaction
    Logger.StartTransaction;
    try
      Logger.Info('Minha mensagem no Log do tipo INFO 5');
      Logger.Info('Minha mensagem no Log do tipo INFO 6');
    finally
      // fazendo Rollback da segunda transaction
      Logger.RollbackTransaction;
    end;

    Logger.Info('Minha mensagem no Log do tipo INFO 7');
    Logger.Info('Minha mensagem no Log do tipo INFO 8');
    Logger.Info('Minha mensagem no Log do tipo INFO 9');
  finally
    // Fazendo o commit
    Logger.CommitTransaction;
  end;

  Readln;
end.

```
