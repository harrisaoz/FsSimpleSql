module FsSimpleSql.Connection

open System.Data.Common

open Microsoft.Data.SqlClient

module RString = Combinators.String

type DataSource =
    | DataSource of
        getConnection: (unit -> DbConnection) *
        getCqConnectionPair: (unit -> DbConnection * DbConnection)

// let interact text prepare execute =
//     use connection = new Microsoft.Data.SqlClient.SqlConnection(connectionString)
//     let interaction: DbCommand = new Microsoft.Data.SqlClient.SqlCommand(text, connection)
//
//     match commandTimeout with
//     | CommandTimeout seconds -> interaction.CommandTimeout <- seconds
//     | DefaultCommandTimeout -> interaction.CommandTimeout <- 30
//
//     prepare interaction
//     connection.Open()
//     execute interaction

type AuthenticationData =
    | TrustedConnection
    | Server of userId: string * password: string

let connectionStringAuthenticationSection =
    function
    | TrustedConnection -> "Trusted_Connection=true"
    | Server (userId, password) -> $"Trusted_Connection=false;User={userId};Password={password}"

let defaultCsParamTo name defaultValue xs =
    let matches needle candidate =
        candidate
        |> RString.split [|'='|]
        |> List.ofArray
        |> function
            | [] -> false
            | key :: _ -> RString.iequal key needle
    match Array.exists (matches name) xs with
    | false ->
        Array.insertAt 0 $"{name}={defaultValue}" xs
    | true ->
        xs

let requiredCsParameters = [
    "Server"
    "Database"
]

let applyConnectionStringDefaults connectionString =
    connectionString
    |> RString.split [|';'|]
    |> defaultCsParamTo "Application Name" "FsSimpleSql"
    |> defaultCsParamTo "MultipleActiveResultSets" "True"
    |> defaultCsParamTo "Application Intent" "ReadWrite"
    |> defaultCsParamTo "TrustServerCertificate" "false"
    |> defaultCsParamTo "Min Pool Size" "1"
    |> defaultCsParamTo "Max Pool Size" "1"
    |> RString.join ";"

let newConnection connectionString =
    new SqlConnection(connectionString)

let openConnection (connection: DbConnection) =
    connection.Open ()

let usingCqConnectionPair (fn: DbConnection * DbConnection -> 'a) (DataSource(_, cqConnectionPair)) =
    let pair = cqConnectionPair ()
    use queryConnection = fst pair
    use commandConnection = snd pair
    
    openConnection queryConnection
    openConnection commandConnection
    fn (queryConnection, commandConnection)

let setApplicationIntent intent connectionString =
    let builder = SqlConnectionStringBuilder connectionString
    builder.ApplicationIntent <- intent
    builder.ConnectionString

module MapPair =
    let map f (a,b) = (f a, f b)
    let apply a (f,g) = (f a, g a)

let newDataSource connectionString =
    let asDbConnection (c: #DbConnection): DbConnection =
        c :> DbConnection

    let getConnection () =
        connectionString
        |> applyConnectionStringDefaults
        |> newConnection
        |> asDbConnection

    let getCqConnectionPair () =
        (ApplicationIntent.ReadWrite, ApplicationIntent.ReadOnly)
        |> MapPair.map setApplicationIntent
        |> MapPair.apply connectionString
        |> MapPair.map newConnection
        |> MapPair.map asDbConnection

    DataSource (getConnection, getCqConnectionPair)
