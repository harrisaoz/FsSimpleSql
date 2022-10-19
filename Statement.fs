module FsSimpleSql.Statement

open System.Data.Common

type CommandTimeout =
    | CommandTimeout of int
    | DefaultCommandTimeout

let [<Literal>] DefaultCommandTimeoutSeconds = 180

let setStatementTimeout timeout (command: DbCommand) =
    match timeout with
    | CommandTimeout seconds ->
        command.CommandTimeout <- seconds
        command
    | DefaultCommandTimeout ->
        command.CommandTimeout <- DefaultCommandTimeoutSeconds
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
