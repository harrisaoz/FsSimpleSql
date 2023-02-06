module FsSimpleSql.Facades.SyncSeqApply

open System.Data.Common
open FsCombinators.Core
open FsSimpleSql

type ExternalAction<'a, 'b> =
    | Noop of result: Result<'b, string>
    | ExternalFunction of fn: ('a -> Result<'b, string>)

let noop = Noop(Result.Ok None)

let map
    (initQuery: DbCommand -> DbCommand)
    (resultSetAdapter: DbDataReader -> 'a)
    (externalAction: ExternalAction<'a, 'e>)
    (initDbCommand: DbCommand -> DbCommand)
    (rebindCommand: 'a -> DbCommand -> DbCommand)
    (commandConnection, queryConnection)
    =
    let dmlStatement: DbCommand =
        Statement.newStatement commandConnection
        |> initDbCommand
        |> Statement.prepareStatement

    let each (record: 'a) : Result<int, string> =
        match externalAction with
        | ExternalFunction externalCommand -> externalCommand record
        | Noop result -> result
        |> Result.bind (fun _ ->
            dmlStatement
            |> rebindCommand record
            |> C (Tx.inTransaction commandConnection) Exec.executeDml)

    Statement.newStatement queryConnection
    |> initQuery
    |> Statement.prepareStatement
    |> Exec.executeQuery
    |> Records.enumerateResultSet resultSetAdapter
    |> Seq.map (fun r -> (r, each r))

let summarize onOk onError =
    Seq.fold
        (fun (applyCount, total) (a, result) ->
            match result with
            | Result.Ok numberOfRowsAffected ->
                K(applyCount + numberOfRowsAffected, total + 1)
                <| onOk (a, numberOfRowsAffected)
            | Result.Error msg -> K(applyCount, total + 1) <| onError (a, msg))
        (0, 0)
