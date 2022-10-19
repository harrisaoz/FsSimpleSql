module FsSimpleSql.Statement

open System.Data.Common

type StatementTimeout =
    | TimeoutSeconds of int
    | Default

let [<Literal>] DefaultStatementTimeoutSeconds = 180

let setStatementTimeout timeout (command: DbCommand) =
    match timeout with
    | TimeoutSeconds seconds ->
        command.CommandTimeout <- seconds
        command
    | Default ->
        command.CommandTimeout <- DefaultStatementTimeoutSeconds
        command

let newStatement (connection: DbConnection) text =
    let statement = connection.CreateCommand ()
    statement.CommandText <- text
    statement

let prepareStatement (cmd: DbCommand) =
    cmd.Prepare ()
    cmd

let addParameters parameters (command: DbCommand) =
    if not (Array.isEmpty parameters) then
        command.Parameters.AddRange parameters
    command

let setParamValue (paramIndex: int) value (command: DbCommand) =
    command.Parameters.Item(paramIndex).Value <- value
