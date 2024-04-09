module FsSimpleSql.Tx

open System.Data.Common

let startIsolated level (connection: DbConnection) = connection.BeginTransaction(level)

let start = startIsolated System.Data.IsolationLevel.Snapshot

let associate (command: DbCommand) transaction =
    command.Transaction <- transaction
    transaction

let commit (transaction: DbTransaction) = transaction.Commit()

let rollback (transaction: DbTransaction) = transaction.Rollback()

let inTransaction (connection: DbConnection) (command: DbCommand) (work: DbCommand -> 'a) =
    let tx = start connection |> associate command
    try
        let workOutput = work command
        commit tx
        Ok workOutput
    with
        | ex ->
            rollback tx
            Error ex.Message
