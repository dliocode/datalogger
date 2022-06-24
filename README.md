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

## Dependências

Essas dependências se dá quando utilizado alguns _providers_

| Provider | Dependence |
|----------|------------|
| DataLogger.Provider.SendEmail| [SendEmail](https://github.com/dliocode/sendemail) |
| DataLogger.Provider.Redis| [Redis](https://github.com/danieleteti/delphiredisclient) |

## Como usar

Existem duas maneiras diferentes de usar o DataLogger: 
Diretamente pelo ``` Logger ``` ou instanciando seu próprio ``` TDataLogger ```.

[Samples](https://github.com/dliocode/datalogger/tree/main/Samples)

O primeiro destina-se apenas a ser um log compartilhado conveniente para ser usado em todo o seu aplicativo, se você escolher.

* **Uses necessária**: 
```
uses DataLogger;
``` 

### Provider

Um **_provider_** serve essencialmente para armazenar seus logs. 
Cada instância de um TDataLogger pode ter vários **_providers_** configurados.

Aqui temos uma lista de todos os _providers_ disponíveis:

| Nome | Uses | Samples |
|------|------|---------|
| Console| DataLogger.Provider.Console| [Console](https://github.com/dliocode/datalogger/tree/main/Samples/Console) |
| [ElasticSearch](https://www.elastic.co/pt/what-is/elasticsearch) | DataLogger.Provider.ElasticSearch| [ElasticSearch](https://github.com/dliocode/datalogger/tree/main/Samples/ElasticSearch) |
| Email| DataLogger.Provider.Email| [Email](https://github.com/dliocode/datalogger/tree/main/Samples/Email) |
| EventLog | DataLogger.Provider.EventLog | [EventLog](https://github.com/dliocode/datalogger/tree/main/Samples/EventLog)|
| Events | DataLogger.Provider.Events | [Events](https://github.com/dliocode/datalogger/tree/main/Samples/Events)<br />[Events - With DataSet](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20DataSet)<br />[Events - With SQLLite](https://github.com/dliocode/datalogger/tree/main/Samples/Events%20-%20With%20SQLLite)|
| ListBox| DataLogger.Provider.ListBox| [ListBox](https://github.com/dliocode/datalogger/tree/main/Samples/ListBox) |
| ListView | DataLogger.Provider.ListView | [ListView](https://github.com/dliocode/datalogger/tree/main/Samples/ListView)|
| [Logstach](https://www.elastic.co/pt/logstash/)| DataLogger.Provider.Logstach | [Logstach](https://github.com/dliocode/datalogger/tree/main/Samples/Logstach)|
| [Mattermost](https://mattermost.com/)| DataLogger.Provider.Mattermost<br />DataLogger.Provider.Mattermost.Hooks | [Mattermost](https://github.com/dliocode/datalogger/tree/main/Samples/Mattermost) <br /> [MattermostHook](https://github.com/dliocode/datalogger/tree/main/Samples/MattermostHook)|
| Memo | DataLogger.Provider.Memo | [Memo](https://github.com/dliocode/datalogger/tree/main/Samples/Memo)<br />[Memo and TexFile](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20and%20TexFile)<br />[Memo - Only Error/VCL](https://github.com/dliocode/datalogger/tree/main/Samples/Memo%20-%20Only%20Error/VCL)|
| Memory | DataLogger.Provider.Memory | [Memory](https://github.com/dliocode/datalogger/tree/main/Samples/Memory)|
| Notification | DataLogger.Provider.Notification | [Notification](https://github.com/dliocode/datalogger/tree/main/Samples/Notification)|
| OutputDebugString| DataLogger.Provider.OutputDebugString| [OutputDebugString](https://github.com/dliocode/datalogger/tree/main/Samples/OutputDebugString) |
| [Redis](https://github.com/danieleteti/delphiredisclient)| DataLogger.Provider.Redis| [Redis](https://github.com/dliocode/datalogger/tree/main/Samples/Redis) |
| Rest | DataLogger.Provider.REST.HTTPClient<br />DataLogger.Provider.REST.Indy<br />DataLogger.Provider.REST.NetHTTPClient | [Rest](https://github.com/dliocode/datalogger/tree/main/Samples/REST)|
| RichEdit| DataLogger.Provider.RichEdit| [RichEdit](https://github.com/dliocode/datalogger/tree/main/Samples/RichEdit) |
| [SendEmail](https://github.com/dliocode/sendemail) | DataLogger.Provider.SendEmail| [SendEmail](https://github.com/dliocode/datalogger/tree/main/Samples/SendEmail) |
| [Slack](https://slack.com/)| DataLogger.Provider.Slack| [Slack](https://github.com/dliocode/datalogger/tree/main/Samples/Slack) |
| Socket | DataLogger.Provider.Socket | [Socket](https://github.com/dliocode/datalogger/tree/main/Samples/Socket)|
| SysLog | DataLogger.Provider.SysLog | [SysLog](https://github.com/dliocode/datalogger/tree/main/Samples/SysLog)|
| [Telegram](https://core.telegram.org/) | DataLogger.Provider.Telegram | [Telegram](https://github.com/dliocode/datalogger/tree/main/Samples/Telegram)|
| TextFile | DataLogger.Provider.TextFile | [TextFile](https://github.com/dliocode/datalogger/tree/main/Samples/TextFile) |

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
    .Fatal('My Fatal')
    .CustomType('My Custom Type', 'My message with custom type');
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
    .Fatal('My Fatal')
    .CustomType('My Custom Type', 'My message with custom type');
  Readln;
end.
```

## CustomType

O _CustomType_ é uma forma de criar seu próprio _type_.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  Logger.CustomType('My Custom Type', 'My Message with custom type!');

  // Output: 2022-12-01 09:00:05.500 [My Custom Type] My Message with custom type!	

  Readln;
end.
```

## Formato do Log

Formato de log padrão: 
```
${timestamp} [TID ${thread_id}] [PID ${process_id}] [SEQ ${sequence}] [${type}] [${tag}] ${message}
```
### FormatLog Constant

Existe algumas constantes que podem ser utilizadas:
```
TLoggerFormat.LOG_NAME = '${name}';
TLoggerFormat.LOG_SEQUENCE = '${sequence}';
TLoggerFormat.LOG_TIMESTAMP = '${timestamp}';
TLoggerFormat.LOG_THREADID = '${thread_id}';
TLoggerFormat.LOG_PROCESSID = '${process_id}';
TLoggerFormat.LOG_TYPE = '${type}';
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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Usando constant
  // Logger.SetLogFormat(Format('%s [%s] %s', 
  //   [TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_LOG_TYPE,  TLoggerFormat.LOG_MESSAGE])
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
  // Formato do console '${timestamp} [${type}] ${message}'
  Logger.AddProvider(TProviderConsole.Create.SetLogFormat('${timestamp} [${type}] ${message}'));

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Usando constant
  // Logger.SetLogFormat(Format('%s [%s] %s', 
  //   [TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_LOG_TYPE,  TLoggerFormat.LOG_MESSAGE])
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

```
// Exibe o nome do log. ex: Logger.SetName('SERVICE_REMOTE')
TLoggerFormat.LOG_NAME;

// Exibe a sequencia que o log foi gerado.
TLoggerFormat.LOG_SEQUENCE;

// Exibe a data e hora que foi gerado.
TLoggerFormat.LOG_TIMESTAMP;

// Exibe o Id da thread que foi gerado o log.
TLoggerFormat.LOG_THREADID;

// Exibe o id do processo do app.
TLoggerFormat.LOG_PROCESSID;

// Exibe o tipo do log, sendo eles: TRACE/DEBUG/INFO/SUCCESS/WARN/ERROR/FATAL
TLoggerFormat.LOG_TYPE;

// Exibe a tag do log, essa informação é preenchida a após a mensagem; Ex: Logger.Debug('Minha mensagem','Minha Tag');
TLoggerFormat.LOG_TAG;

// Exibe a mensagem do log, sem essa tag a mensagem não é exibida. Ex: Logger.Debug('Minha mensagem');
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

## LogLevel

É possível definir o nível do registro do log com base no ```TLoggerType```.

SetLogLevel valor padrão = ```TLoggerType.All```

### LoggerType / Level

* Quando definido um level, será exibido somente a opção escolhida e seus tipos superiores.
* Ex: ``` Logger.SetLogLevel(TLoggerType.Warn); ``` - Será registrado somente os _logs_ com o tipo ``` Warn / Error / Fatal / Custom ```.

```
  TLoggerType.All = Utilizado para operações internas 
  TLoggerType.Trace = Level 1
  TLoggerType.Debug = Level 2
  TLoggerType.Info = Level 3
  TLoggerType.Success = Level 4
  TLoggerType.Warn = Level 5
  TLoggerType.Error = Level 6
  TLoggerType.Fatal = Level 7
  TLoggerType.Custom = Level 8
```

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Definindo o LogLevel
  Logger.SetLogLevel(TLoggerType.Warn);

  // Gerando os logs
  Logger.Info('Minha mensagem no Log do tipo INFO');
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR  

  Readln;
end.
```

## Disable LogType

É possível desabilitar vários tipos de log, com base no ```TLoggerType```.

SetDisableLogType valor padrão = ```[]```

### SetDisableLogType

* Quando desabilitado será exibido somente as opções que não estão desabilitadas.
* Ex: ``` Logger.SetDisableLogType([TLoggerType.Info, TLoggerType.Warn]); ``` - Será registrado somente os _logs_ com o tipo ``` Tracer / Debug / Success / Error / Fatal / Custom ```.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Definindo o DisableLogType
  Logger.SetDisableLogType([TLoggerType.Info, TLoggerType.Warn]);

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

## Only LogType

É possível habilitar vários tipos de log, com base no ```TLoggerType```.

SetOnlyLogType valor padrão = ```[TLoggerType.All]```

### SetOnlyLogType

* Quando definido será exibido somente as opções registradas.
* Ex: ``` Logger.SetOnlyLogType([TLoggerType.Error]); ``` - Será registrado somente os _logs_ com o tipo ``` Error ```.

```delphi
uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Definindo o OnlyLogType
  Logger.SetOnlyLogType([TLoggerType.Error]);

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Definindo a mensagem final
  Logger.SetFinalMessage(' DLIOCODE');

  // Gerando o log
  Logger.Error('Minha mensagem no Log do tipo ERROR');
  
  // Output: 
  // 2022-12-01 09:00:05.600 [ERROR] Minha mensagem no Log do tipo ERROR DLIOCODE 

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
  Logger.SetLogFormat('${name} ${timestamp} [${type}] ${message}');

  // Definindo o name
  Logger.SetName('MyLogger');

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

Vamos contar uma pesquena história:

> Temos uma _procedure_ que está fazendo uma execução, em cada etapa é gerado várias informações log, por costume sempre salvamos essas informações, isso faz que o nosso arquivo texto por exemplo, porém esse arquivo pode ficar grande demais. <br /> Agora imagina poder salvar os dados somente se houve algum erro durante a execução.

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

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
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

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
