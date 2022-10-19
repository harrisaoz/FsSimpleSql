module FsSimpleSql.Exec

open System.Data.Common

let executeQuery (command: DbCommand) =
    command.ExecuteReader ()

let executeQueryWithTimeout timeout =
    Statement.setStatementTimeout timeout >> executeQuery

/// Returns a single record.
let executeScalar (command: DbCommand) =
    command.ExecuteScalar ()

/// Returns the number of records affected.
let executeDml (command: DbCommand) =
    command.ExecuteNonQuery ()
