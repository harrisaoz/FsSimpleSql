module FsSimpleSql.Facades.ExportAndAcknowledge

open FsCombinators.Core
open FsSimpleSql

type Document<'Id, 'Name, 'Content> =
    | Document of identity: 'Id * name: 'Name * content: 'Content

type Export<'DocId, 'DocName, 'DocContent> =
    Document<'DocId, 'DocName, 'DocContent> -> Result<'DocId, string>

/// It is assumed that the ack function returns how many items (rows)
/// were updated.  It is expected that this should be either 0 or 1 for
/// the envisaged scenarios.
type Ack<'DocId> = 'DocId -> int

type ExportDependencies<'DocId, 'DocName, 'DocContent> =
    | ExportDependencies of
        export: Export<'DocId, 'DocName, 'DocContent> *
        trace: ('DocId -> unit) *
        onErr: (string -> unit)

module Query = QueryFacade
module Command = CommandFacade

let executeInTransaction inTx stmtConn exec =
    (B (B C) (B (B W) B)) inTx stmtConn exec

/// qryPrepDeps: how to create a prepared statement for a query
/// qryExecDeps: how to:
/// - bind parameters to a prepared statement
/// - execute a prepared statement that is to return a result set (query)
/// - transform a result set into a typed sequence, given a deserialization
///   function
/// ackPrepDeps: how to create a prepared statement for a command
/// ackExecDeps: how to execute a statement in a transaction
/// Example:
/// executeInTransaction
///     Tx.inTransaction
///     Statement.associatedConnection
///     Exec.executeDml
/// |> CommandFacade.ExecuteStatementDependencies
///
/// qryPrepCtx = query text
/// qryExecCtx = query parameters + how to deserialize
/// ackPrepCtx = ack command name
/// ackExecCtx = bind function
///
/// Requires the query connection as input, wrapped in a
/// QueryFacade.PrepareQueryStatementData
let exportDocumentsAndAcknowledge
    (qryPrepDeps: Query.PrepareQueryStatementDependencies<'C, 'Sq, 'P>)
    (qryExecDeps:
        Query.ExecuteStatementDependencies<'Sq, 'Rdr, Document<'Id, 'Name, 'Content>, 'P>)
    (ackPrepDeps: Command.PrepareCommandStatementDependencies<'C, 'Sc>)
    (ackExecDeps: Command.ExecuteStatementDependencies<'Sc, 'XR>)
    (ExportDependencies(export, trace, onErr):
        ExportDependencies<'Id, 'Name, 'Content>)
    (qryPrepCtx: Query.PrepareQueryStatementContext)
    (qryExecCtx:
        Query.ExecuteStatementContext<'Rdr, Document<'Id, 'Name, 'Content>, 'P>)
    (ackPrepCtx: Command.PrepareCommandStatementContext)
    (ackExecCtx: Command.ExecuteStatementContext<'Id, 'Sc>)
    (ackPrepData: Command.PrepareCommandStatementData<'C>)
    =
    let ackStatement =
        Command.prepareStatement ackPrepDeps ackPrepCtx ackPrepData

    let ackExec =
        CommandFacade.executeStatement ackExecDeps ackExecCtx

    let exportAndAckAsync doc =
        async {
            return
                export doc
                |> Result.map (ackExec ackStatement >> (C K trace))
                |> Result.mapError (C K onErr)
        }

    Query.executeQuery qryPrepDeps qryExecDeps qryPrepCtx qryExecCtx
    >> Seq.map exportAndAckAsync
    >> Async.Sequential
    >> Async.RunSynchronously
    >> List.ofSeq

module DefaultDependencies =
    let ackPrep =
        let newStoredProcStatement connection =
            connection
            |> (Statement.newStatement
                >> Statement.asStoredProcedure)

        CommandFacade.PrepareCommandStatementDependencies(
            newStatement = newStoredProcStatement,
            withCommandName = Statement.withName,
            prepare = Statement.prepareStatement
        )

    let ackExec =
        let execCmd cmd =
            executeInTransaction
                Tx.inTransaction
                Statement.associatedConnection
                Exec.executeDml
                cmd

        CommandFacade.ExecuteStatementDependencies(executeCommand = execCmd)
        
    // let export = 
    //     ExportDependencies(
    //         export = ...,
    //         trace = ,
    //         onErr)

let standardAckPrep () =
    DefaultDependencies.ackPrep
    |> CommandFacade.prepareStatement

let standardAckExec () =
    DefaultDependencies.ackExec
    |> CommandFacade.executeStatement

let standardAckStatement ackPrepCtx ackPrepData =
    Command.ExecuteStatementData(
        statement =
            (Command.prepareStatement
                DefaultDependencies.ackPrep
                ackPrepCtx
                ackPrepData)
    )

let exportDocsAndAcknowledgeWithDefaultDependencies () =
    exportDocumentsAndAcknowledge
        QueryFacade.DefaultDependencies.prepareStatement
        QueryFacade.DefaultDependencies.executeStatement
        DefaultDependencies.ackPrep
        DefaultDependencies.ackExec

/// qryPrepCtx = query text
/// qryExecCtx = query parameters + how to deserialize
/// ackPrepCtx = ack command name
/// ackExecCtx = bind function
let eda qryPrepCtx qryExecCtx ackPrepCtx ackExecCtx export onErr cRO cRW =
    let ackPrep =
        let deps =
            CommandFacade.DefaultDependencies.prepareStatement

        CommandFacade.prepareStatement deps ackPrepCtx

    let ackExec =
        let execCmd =
            executeInTransaction
                Tx.inTransaction
                Statement.associatedConnection
                Exec.executeDml

        let deps =
            CommandFacade.ExecuteStatementDependencies(executeCommand = execCmd)

        CommandFacade.executeStatement deps ackExecCtx

    let ackStatement =
        CommandFacade.PrepareCommandStatementData(connection = cRW)
        |> ackPrep

    let exportAndAckAsync doc =
        async {
            return
                export doc
                |> Result.map (ackExec ackStatement)
                |> Result.mapError onErr
        }

    cRO
    |> ((Query.executeQuery
             Query.DefaultDependencies.prepareStatement
             Query.DefaultDependencies.executeStatement
             qryPrepCtx
             qryExecCtx)
        >> Seq.map exportAndAckAsync
        >> Async.Sequential
        >> Async.RunSynchronously
        >> List.ofSeq)
