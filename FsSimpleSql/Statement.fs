module FsSimpleSql.Statement

open System.Data
open System.Data.Common

type StatementTimeout =
    | TimeoutSeconds of int
    | Default

[<Literal>]
let DefaultStatementTimeoutSeconds = 180

let newStatement (connection: DbConnection) = connection.CreateCommand()

let withStatementTimeout timeout (command: DbCommand) =
    match timeout with
    | TimeoutSeconds seconds ->
        command.CommandTimeout <- seconds
        command
    | Default ->
        command.CommandTimeout <- DefaultStatementTimeoutSeconds
        command

let withText text (cmd: DbCommand) =
    cmd.CommandText <- text
    cmd

let withName = withText

let asStoredProcedure (cmd: DbCommand) =
    cmd.CommandType <- CommandType.StoredProcedure
    cmd

let associatedConnection (cmd: DbCommand) =
    cmd.Connection

let newStoredProcStatement name =
    newStatement >> withText name >> asStoredProcedure

let newTextStatement text = newStatement >> withText text

let prepareStatement (cmd: DbCommand) =
    cmd.Prepare()
    cmd

let withParameters (parameters: DbParameter array) (command: DbCommand) =
    if not (Array.isEmpty parameters) then
        command.Parameters.AddRange parameters

    command

let rebind (parameters: DbParameter array) (command: DbCommand) =
    parameters
    |> Array.iter (fun (p: DbParameter) -> command.Parameters.Item(p.ParameterName).Value <- p.Value)

    command

let withParameterValue (paramIndex: int) value (command: DbCommand) =
    command.Parameters.Item(paramIndex).Value <- value
    command
