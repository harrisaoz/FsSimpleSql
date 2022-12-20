module FsSimpleSql.Connection

open System.Data.Common

open Microsoft.Data.SqlClient

module StringExt = FsCombinators.StringExtensions

open FsSimpleSql.ConnectionString

type DataSource =
    | DataSource of getConnection: (unit -> DbConnection) * getCqConnectionPair: (unit -> DbConnection * DbConnection)

let newConnection connectionString = new SqlConnection(connectionString)

let openConnection (connection: DbConnection) =
    connection.Open()
    connection

let usingCqConnectionPair (fn: DbConnection * DbConnection -> 'a) (DataSource(_, cqConnectionPair)) =
    let pair = cqConnectionPair ()
    use queryConnection = fst pair
    use commandConnection = snd pair

    fn (openConnection queryConnection, openConnection commandConnection)

module MapPair =
    let map f (a, b) = (f a, f b)
    let apply a (f, g) = (f a, g a)

let newDataSource validate connectionString =
    let asDbConnection (c: #DbConnection) : DbConnection = c :> DbConnection

    let getConnection () =
        connectionString
        |> applyDefaults StandardDefaults.secureMars
        |> validate
        // |> simpleValidate (List.append TrustedNetwork.requiredParameters WithUserCredentials.requiredParameters)
        |> newConnection
        |> asDbConnection

    let getCqConnectionPair () =
        (ApplicationIntent.ReadWrite, ApplicationIntent.ReadOnly)
        |> MapPair.map setApplicationIntent
        |> MapPair.apply connectionString
        |> MapPair.map newConnection
        |> MapPair.map asDbConnection

    DataSource(getConnection, getCqConnectionPair)

let newDataSourceUnvalidated =
    newDataSource id
