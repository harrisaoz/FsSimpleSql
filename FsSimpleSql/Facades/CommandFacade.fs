module FsSimpleSql.Facades.CommandFacade

type CommandName = CommandName of string

type PrepareCommandStatementDependencies<'Connection, 'Statement> =
    | PrepareCommandStatementDependencies of
        newStatement: ('Connection -> 'Statement) *
        withCommandName: (string -> 'Statement -> 'Statement) *
        prepare: ('Statement -> 'Statement)

type PrepareCommandStatementContext =
    | PrepareCommandStatementContext of commandName: CommandName

type PrepareCommandStatementData<'Connection> =
    | PrepareCommandStatementData of connection: 'Connection

let prepareStatement
    (PrepareCommandStatementDependencies(newStatement, withCommandName, prepare))
    (PrepareCommandStatementContext(CommandName commandName))
    (PrepareCommandStatementData(connection))
    =
    newStatement connection
    |> withCommandName commandName
    |> prepare

type ExecuteStatementDependencies<'Statement, 'ExecutionResult> =
    | ExecuteStatementDependencies of
        executeCommand: ('Statement -> 'ExecutionResult)

type ExecuteStatementContext<'Record, 'Statement> =
    | ExecuteStatementContext of bind: ('Record -> 'Statement -> 'Statement)

type ExecuteStatementData<'Statement> =
    | ExecuteStatementData of statement: 'Statement

let executeStatement
    (ExecuteStatementDependencies executeCommand)
    (ExecuteStatementContext bind)
    statement
    record
    =
    statement |> bind record |> executeCommand

let executeCommand prepDeps execDeps prepCtx execCtx =
    prepareStatement prepDeps prepCtx
    >> executeStatement execDeps execCtx

open FsSimpleSql

module DefaultDependencies =
    open System.Data.Common

    // The type cannot be inferred, so an explicit type signature is needed here.
    let prepareStatement: PrepareCommandStatementDependencies<DbConnection, DbCommand> =
        PrepareCommandStatementDependencies(
            (Statement.newStatement
             >> Statement.asStoredProcedure),
            Statement.withName,
            Statement.prepareStatement
        )

    let executeStatementWithNumRows =
        ExecuteStatementDependencies(Exec.executeDml)

    let executeStatementScalar =
        ExecuteStatementDependencies(Exec.executeScalar)

let newCommandStatementUsingDefaultDependencies () =
    prepareStatement DefaultDependencies.prepareStatement

let executeStatementUsingDefaultDependencies () =
    executeStatement DefaultDependencies.executeStatementWithNumRows

let executeCommandUsingDefaultDependencies () =
    executeCommand
        DefaultDependencies.prepareStatement
        DefaultDependencies.executeStatementWithNumRows
