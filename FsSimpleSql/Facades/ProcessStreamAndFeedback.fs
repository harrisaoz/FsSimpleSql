module FsSimpleSql.Facades.ProcessStreamAndFeedback

open FsCombinators.Core

[<Literal>]
let moduleName =
    "FsSimpleSql.Facades.ProcessStreamAndFeedback"

type SqlQuery<'StatementText, 'SourceRecord, 'Parameters, 'Reader> =
    | SqlQuery of
        queryText: 'StatementText *
        parameters: 'Parameters *
        extractor: ('Reader -> 'SourceRecord)

type SqlCommand<'SourceRecord, 'Connection, 'Command, 'Parameters> =
    | SqlCommand of
        newStatement: ('Connection -> 'Command) *
        parameters: 'Parameters *
        binder: ('SourceRecord -> 'Command -> 'Command)

let runUnbound
    (onOk: 'Srec -> int -> unit, onError: 'Srec -> string -> unit)
    (cStmt: 'StatementText -> 'Connection -> 'Command,
     bind: 'Parameters -> 'Command -> 'Command,
     execStreamQry: 'Command -> 'Reader,
     enumStream: ('Reader -> 'Srec) -> 'Reader -> #seq<'Srec>,
     inTx: 'Connection -> 'Command -> ('Command -> int) -> Result<int, string>,
     execFeedbackQry: 'Command -> int)
    (SqlQuery(producerText, producerParams, producerDao: 'Reader -> 'Srec))
    (SqlCommand(newStatement, dbConsumerParams, dbConsumerBinder: 'Srec -> 'Command -> 'Command))
    (proc: 'Srec -> Result<'Id, string>)
    (streamConn: 'Connection, feedbackConn: 'Connection)
    =

    let consumerStatement =
        newStatement feedbackConn |> bind dbConsumerParams

    let dbConsume (a: 'Srec) =
        inTx feedbackConn consumerStatement (dbConsumerBinder a >> execFeedbackQry)

    let forEach (a: 'Srec) =
        async {
            return
                proc a
                |> Result.bind (fun _ -> dbConsume a)
                |> Result.map (fun n -> 1 - n)
                |> Result.map (C K (onOk a))
                |> Result.mapError (C K (onError a))
        }

    streamConn
    |> cStmt producerText
    |> bind producerParams
    |> execStreamQry
    |> enumStream producerDao
    |> Seq.map forEach
    |> Async.Sequential
    |> Async.RunSynchronously
    |> List.ofSeq

let onOk debug a n =
    sprintf "%s.forEach \"%s\" -> %d" moduleName (string a) n
    |> debug

let onError (debug, warning) ident a msg =
    sprintf "%s.forEach \"%s\"" moduleName (string a)
    |> debug

    sprintf "%s.forEach \"%s\" %s" moduleName (ident a) msg
    |> warning

open Serilog
open FsSimpleSql

let runDefaultWithSerilog (logger: ILogger) streamQueryTimeout ident =
    let debug =
        fun (s: string) -> logger.Debug(s)

    let warning =
        fun (s: string) -> logger.Warning(s)

    runUnbound
        (onOk debug, onError (debug, warning) ident)
        (Statement.newTextStatement,
         Statement.withParameters,
         Exec.executeQueryWithTimeout streamQueryTimeout,
         Records.enumerateResultSet,
         Tx.inTransaction,
         Exec.executeDml)

let runWithDefaults ident =
    runDefaultWithSerilog Log.Logger (Statement.TimeoutSeconds 60) ident
