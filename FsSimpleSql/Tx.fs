module FsSimpleSql.Tx

open System.Data.Common

let startIsolated level (connection: DbConnection) = connection.BeginTransaction(level)

let start connection =
    startIsolated System.Data.IsolationLevel.Snapshot connection

let tryStart connection =
    ErrorHandling.tryMaybeFatalAsResult start connection

let associate (command: DbCommand) transaction =
    command.Transaction <- transaction
    transaction

let commit (transaction: DbTransaction) = transaction.Commit()

let tryCommit transaction =
    ErrorHandling.tryMaybeFatalAsResult commit transaction

let rollback (transaction: DbTransaction) = transaction.Rollback()

let tryRollback transaction =
    ErrorHandling.tryMaybeFatalAsResult rollback transaction

let rec withRetry retryCount (execute: DbCommand -> 'a) connection statement =
    let onTxException tx (ex: exn) =
        ErrorHandling.tryMaybeFatalAsUnit rollback tx

        if retryCount > 0 then
            withRetry (retryCount - 1) execute connection statement
        else
            Error ex.Message

    tryStart connection
    |> Result.map (associate statement)
    |> Result.bind (fun tx ->
        ErrorHandling.tryMaybeFatal
            (onTxException tx)
            Ok
            (fun stmt ->
                let executionResult = execute stmt
                commit tx
                executionResult)
            statement)

let mutable defaultRetryCount = 2

let inTransaction execute connection statement =
    withRetry defaultRetryCount execute connection statement
