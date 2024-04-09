module FsSimpleSql.Tx

open System.Data.Common

let startIsolated level (connection: DbConnection) = connection.BeginTransaction(level)

let start = startIsolated System.Data.IsolationLevel.Snapshot

let associate (command: DbCommand) transaction =
    command.Transaction <- transaction
    transaction

let commit (transaction: DbTransaction) = transaction.Commit()

let rollback (transaction: DbTransaction) = transaction.Rollback()

let inTransaction (execute: DbCommand -> 'a) (connection: DbConnection) (statement: DbCommand) =
    let tx = connection |> start |> associate statement

    try
        let executionResult = execute statement
        commit tx
        Ok executionResult
    with ex ->
        rollback tx
        Error ex.Message
