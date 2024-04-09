﻿module FsSimpleSql.Facades.SyncSeqApply

open System.Data.Common
open FsCombinators.Core
open FsSimpleSql

type ExternalAction<'a, 'b> =
    | Noop of result: Result<'b, string>
    | ExternalFunction of fn: ('a -> Result<'b, string>)

let noop = Noop(Result.Ok None)

let executeCmdInTx connection cmd =
    Tx.inTransaction connection Exec.executeDml cmd

type GenericFunctions<'Conn, 'Stmt, 'Reader, 'a> =
    { NewStatement: 'Conn -> 'Stmt
      PrepareStatement: 'Stmt -> 'Stmt
      ExecInTx: 'Conn -> 'Stmt -> Result<int, string>
      ExecuteQuery: 'Stmt -> 'Reader
      EnumerateResultSet: ('Reader -> 'a) -> 'Reader -> 'a seq }

let dbFunctions: GenericFunctions<DbConnection, DbCommand, DbDataReader, 'a> =
    { NewStatement = Statement.newStatement
      PrepareStatement = Statement.prepareStatement
      ExecInTx = (fun connection -> Tx.inTransaction connection Exec.executeDml)
      ExecuteQuery = Exec.executeQuery
      EnumerateResultSet = Records.enumerateResultSet }

let mapGeneric
    ({ NewStatement = newStatement
       PrepareStatement = prepareStatement
       ExecInTx = execInTx
       ExecuteQuery = executeQuery
       EnumerateResultSet = enumerateResultSet }: GenericFunctions<'c, 's, 'r, 'a>)
    =
    fun query rsReader externalAction proc procRebind (commandConn, queryConn) ->
        let procStmt = newStatement commandConn |> proc |> prepareStatement

        let each (record: 'a) : Result<int, string> =
            match externalAction with
            | ExternalFunction f -> f record
            | Noop result -> result
            |> Result.bind (fun _ -> procRebind record procStmt |> execInTx commandConn)

        queryConn
        |> newStatement
        |> query
        |> prepareStatement
        |> executeQuery
        |> enumerateResultSet rsReader
        |> Seq.map (fun record -> (record, each record))

let map query = mapGeneric dbFunctions query

let summarize onOk onError =
    Seq.fold
        (fun (totalNumberOfRowsAffected, totalItemCount) (a, result) ->
            match result with
            | Result.Ok numberOfRowsAffectedByCall ->
                K
                <| (totalNumberOfRowsAffected + numberOfRowsAffectedByCall, totalItemCount + 1)
                <| onOk (a, numberOfRowsAffectedByCall)
            | Result.Error msg ->
                K <| (totalNumberOfRowsAffected, totalItemCount + 1) <| onError (a, msg))
        (0, 0)
