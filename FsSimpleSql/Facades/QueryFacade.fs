module FsSimpleSql.Facades.QueryFacade

type QueryText = QueryText of string

type PrepareQueryStatementDependencies<'Connection, 'Statement, 'Parameter> =
    | PrepareQueryStatementDependencies of
        newStatement: ('Connection -> 'Statement) *
        withText: (string -> 'Statement -> 'Statement) *
        prepare: ('Statement -> 'Statement)

type PrepareQueryStatementContext =
    | PrepareQueryStatementContext of queryText: QueryText

let prepareStatement
    (PrepareQueryStatementDependencies(newStatement, withText, prepare))
    (PrepareQueryStatementContext(QueryText queryText))
    connection
    =
    newStatement connection
    |> withText queryText
    |> prepare

type ExecuteStatementDependencies<'Statement, 'Reader, 'Record, 'Parameter> =
    | ExecuteStatementDependencies of
        bind: ('Parameter array -> 'Statement -> 'Statement) *
        executeQuery: ('Statement -> 'Reader) *
        enumerateResultSet: (('Reader -> 'Record) -> 'Reader -> 'Record seq)

type ExecuteStatementContext<'Reader, 'Record, 'Parameter> =
    | ExecuteStatementContext of
        parameters: 'Parameter array *
        deserialize: ('Reader -> 'Record)

let executeStatement
    (ExecuteStatementDependencies(bind, exec, enumerateResultSet))
    (ExecuteStatementContext(parameters, deserialize))
    =
    bind parameters
    >> exec
    >> (enumerateResultSet deserialize)

let executeQuery prepDeps execDeps prepCtx execCtx =
    prepareStatement prepDeps prepCtx
    >> executeStatement execDeps execCtx

open FsSimpleSql

module DefaultDependencies =
    let prepareStatement =
        PrepareQueryStatementDependencies(
            Statement.newStatement,
            Statement.withText,
            Statement.prepareStatement
        )

    let executeStatement =
        ExecuteStatementDependencies(
            Statement.withParameters,
            Exec.executeQuery,
            Records.enumerateResultSet
        )

let prepareQueryStatementUsingDefaultDependencies () =
    prepareStatement DefaultDependencies.prepareStatement

let executeStatementUsingDefaultDependencies () =
    executeStatement DefaultDependencies.executeStatement

let executeQueryUsingDefaultDependencies () =
    executeQuery
        DefaultDependencies.prepareStatement
        DefaultDependencies.executeStatement
