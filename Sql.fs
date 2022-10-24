module FsSimpleSql.Sql

open System.Data.Common
open Serilog

type InputParameters = DbParameter array

type SqlQuery<'a> =
    | SqlQuery of
        queryText: string *
        parameters: InputParameters *
        extractor: (DbDataReader -> 'a)

type SqlCommand<'a> =
    | SqlCommand of
        sqlText: string *
        parameters: InputParameters *
        binder: ('a -> DbCommand -> DbCommand)

let pipeToExternalThenAuditWithLogging (logger: ILogger) qryTimeout id producer dbConsumer extConsumer (qryConn, cmdConn) =
    let (SqlCommand(consumerSqlText, dbConsumerParams, dbConsumerBinder)) = dbConsumer
    let consumerStatement =
        Statement.newStatement cmdConn consumerSqlText
        |> Statement.addParameters dbConsumerParams

    let dbConsume (a: 'a) =
        Tx.inTransaction (cmdConn, consumerStatement) (dbConsumerBinder a >> Exec.executeDml)

    let forEach (a: 'a) =
        extConsumer a
        |> Result.bind (fun _ -> dbConsume a)
        |> Result.map (fun n -> 1 - n)
        |> function
            | Ok n ->
                logger.Debug("pipeToExternalThenAuditWithTracing.forEach {a} -> {n}", a, n)
                Ok n
            | Error msg ->
                logger.Debug("pipeToExternalThenAuditWithTracing.forEach {a}", a)
                logger.Warning("pipeToExternalThenAuditWithTracing.forEach {Id} {Msg}", id a, msg)
                Error msg

    let (SqlQuery (producerText, producerParameters, producerDao)) = producer
    Statement.newStatement qryConn producerText
    |> Statement.addParameters producerParameters
    |> Exec.executeQueryWithTimeout qryTimeout
    |> Records.enumerateResultSet producerDao
    |> Seq.map forEach
    |> List.ofSeq

let pipeToExternalThenAudit qryTimeout =
    pipeToExternalThenAuditWithLogging Log.Logger qryTimeout
