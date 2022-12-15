module FsSimpleSql.ConnectionStringValidation

open Microsoft.Data.SqlClient

open FsCombinators.Core

type ConnectionStringParameter =
    | Server
    | Database
    | ApplicationName
    | Authentication
    | CommandTimeout
    | ConnectRetryCount
    | ConnectRetryInterval
    | ConnectTimeout
    | FailoverPartner
    | FailoverPartnerSpn
    | HostNameInCertificate
    | TrustedConnection
    | IpAddressPreference
    | LoadBalanceTimeout
    | MaxPoolSize
    | MinPoolSize
    | MultipleActiveResultSets
    | MultiSubnetFailover
    | PacketSize
    | Password
    | PersistSecurityInfo
    | PoolBlockingPeriod
    | Pooling
    | Replication
    | ServerSpn
    | TransactionBinding
    | TrustServerCertificate
    | User
    | WorkstationId

let validationBuilder connectionString =
    SqlConnectionStringBuilder connectionString

let seqAsMaybeString xs =
    if Seq.isEmpty xs then
        None
    else
        List.ofSeq xs
        |> string
        |> Some

let validateConnectionString builder listMissingParameters connectionString =
    let algorithm =
        builder
        >> listMissingParameters
        >> seqAsMaybeString 
        >> Option.map (sprintf "The following connection string parameters are required, but were missing: %s")
        >> Option.iter (invalidArg "connectionString")

    S K algorithm connectionString

let connectionStringKeyNames: Map<ConnectionStringParameter, string> =
    Map
        [ (Server, "Data Source")
          (Database, "Initial Catalog")
          (ApplicationName, "Application Name")
          (Authentication, "Authentication")
          (CommandTimeout, "Command Timeout")
          (ConnectRetryCount, "Connect Retry Count")
          (ConnectRetryInterval, "Connect Retry Interval")
          (ConnectTimeout, "Connect Timeout")
          (FailoverPartner, "Failover Partner")
          (FailoverPartnerSpn, "Failover Partner SPN")
          (HostNameInCertificate, "Host Name In Certificate")
          (TrustedConnection, "Integrated Security")
          (IpAddressPreference, "IP Address Preference")
          (LoadBalanceTimeout, "Load Balance Timeout")
          (MaxPoolSize, "Max Pool Size")
          (MinPoolSize, "Min Pool Size")
          (MultipleActiveResultSets, "Multiple Active Result Sets")
          (MultiSubnetFailover, "Multi Subnet Failover")
          (PacketSize, "Packet Size")
          (Password, "Password")
          (PersistSecurityInfo, "Persist Security Info")
          (PoolBlockingPeriod, "Pool Blocking Period")
          (Pooling, "Pooling")
          (Replication, "Replication")
          (ServerSpn, "Server SPN")
          (TransactionBinding, "Transaction Binding")
          (TrustServerCertificate, "Trust Server Certificate")
          (User, "User ID")
          (WorkstationId, "Workstation ID") ]

let keyIsDefined key (builder: SqlConnectionStringBuilder) =
    connectionStringKeyNames.TryFind key
    |> Option.map builder.ShouldSerialize
    |> Option.defaultValue false

let listMissingRequired reqParams builder =
    reqParams
    |> Seq.filter (not << C keyIsDefined builder)

type RequiredParameters = ConnectionStringParameter seq

let simpleValidate (builder: string -> SqlConnectionStringBuilder) =
    listMissingRequired >> (validateConnectionString builder)

module BasicNetwork =
    let requiredParameters =
        [ Server; Database ]

module TrustedNetwork =
    let requiredParameters =
        [ Server
          Database
          TrustServerCertificate ]

module WithUserCredentials =
    let requiredParameters =
        [ User; Password; TrustedConnection ]
