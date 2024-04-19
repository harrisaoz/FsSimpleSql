module FsSimpleSql.Exec

open System.Data.Common

let executeQuery (command: DbCommand) = command.ExecuteReader()

let executeQueryWithTimeout timeout =
    Statement.withStatementTimeout timeout >> executeQuery

/// Returns a single record.
let executeScalar (command: DbCommand) = command.ExecuteScalar()

/// Returns the number of records affected.
let executeDml (command: DbCommand) = command.ExecuteNonQuery()

let tryExecute tryExecute (command: DbCommand) =
    ErrorHandling.tryMaybeFatalAsResult tryExecute command

let tryExecuteQuery command = tryExecute executeQuery command

let tryExecuteScalar command = tryExecute executeScalar command

let tryExecuteDml command = tryExecute executeDml command
