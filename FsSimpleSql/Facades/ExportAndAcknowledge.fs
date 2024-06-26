﻿module FsSimpleSql.Facades.ExportAndAcknowledge

open FsCombinators.Core
open FsSimpleSql
open Microsoft.Data.SqlClient

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

let executeInTransaction inTx stmtConn exec stmt =
    inTx stmtConn exec stmt

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
    (ackExecDeps: Command.ExecuteStatementDependencies<'Sc, Result<'Id, string>>)
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

    let ackExec = CommandFacade.executeStatement ackExecDeps ackExecCtx

    let main: Document<'Id, 'Name, 'Content> -> Result<'Id, string> =
        export
        >> Result.bind (ackExec ackStatement)
        >> Result.map (C K trace)
        >> Result.mapError (C K onErr)

    // Eliminate Async.Sequential/RunSync by doing the following:
    // - use a command/query connection pair
    // - execute query in the query connection, without an explicit transaction
    // - execute the ack command in the command connection, using an explicit
    //   transaction that is set to Snapshot isolation.
    Query.executeQuery qryPrepDeps qryExecDeps qryPrepCtx qryExecCtx
    >> Seq.map main
    >> List.ofSeq

module DefaultDependencies =
    let ackPrep idParameterName =
        let newStoredProcStatement connection =
            connection
            |> (Statement.newStatement
                >> Statement.asStoredProcedure
                >> Statement.withParameters
                    [| SqlParameter(
                           $"@{idParameterName}",
                           System.Data.SqlDbType.Int
                       ) |])

        CommandFacade.PrepareCommandStatementDependencies(
            newStatement = newStoredProcStatement,
            withCommandName = Statement.withName,
            prepare = Statement.prepareStatement
        )

    let ackExec: Command.ExecuteStatementDependencies<'a, Result<int, string>> =
        let execCmd cmd =
            executeInTransaction
                Tx.inTransaction
                Exec.executeDml
                (Statement.associatedConnection cmd)
                cmd

        CommandFacade.ExecuteStatementDependencies(executeCommand = execCmd)

let standardAckPrep idParameterName =
    DefaultDependencies.ackPrep idParameterName
    |> CommandFacade.prepareStatement

let standardAckExec () =
    DefaultDependencies.ackExec |> CommandFacade.executeStatement

let standardAckStatement idParameterName ackPrepCtx ackPrepData =
    Command.ExecuteStatementData(
        statement =
            (Command.prepareStatement
                (DefaultDependencies.ackPrep idParameterName)
                ackPrepCtx
                ackPrepData)
    )

let exportDocsAndAcknowledgeWithDefaultDependencies idParameterName =
    exportDocumentsAndAcknowledge
        QueryFacade.DefaultDependencies.prepareStatement
        QueryFacade.DefaultDependencies.executeStatement
        (DefaultDependencies.ackPrep idParameterName)
        DefaultDependencies.ackExec
