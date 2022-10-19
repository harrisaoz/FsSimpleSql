module FsSimpleSql.Sql

open System.Data.Common

type InputParameters = DbParameter array

type SqlQuery<'a> =
    | SqlQuery of
        queryText: string *
        parameters: InputParameters *
        extractor: (Records.RecordExtractor -> 'a)

type SqlCommand<'a> =
    | SqlCommand of
        sqlText: string *
        parameters: InputParameters *
        binder: ('a -> DbCommand -> unit)

let pipeToExternalThenAudit qryTimeout producer dbConsumer extConsumer (qryConn, cmdConn) =
    let (SqlCommand(consumerSqlText, dbConsumerParams, dbConsumerBinder)) = dbConsumer
    let consumerStatement =
        Statement.newStatement cmdConn consumerSqlText
        |> Statement.addParameters dbConsumerParams

    let dbConsume (a: 'a) =
        Tx.inTransaction (cmdConn, consumerStatement) (
            fun cmd ->
                dbConsumerBinder a cmd
                Exec.executeDml cmd)

    let forEach (a: 'a) =
        extConsumer a
        |> Result.bind (fun _ -> dbConsume a)
        |> Result.map (fun n -> 1 - n)

    let (SqlQuery (producerText, producerParameters, producerDao)) = producer
    Statement.newStatement qryConn producerText
    |> Statement.addParameters producerParameters
    |> Exec.executeQueryWithTimeout qryTimeout
    |> Records.enumerateResultSet producerDao
    |> Seq.map forEach
    |> List.ofSeq
