module FsSimpleSql.Spec.ProcessStreamAndFeedbackSpec

open FsCheck.Xunit

open FsCombinators.Core
open FsSimpleSql.Facades.ProcessStreamAndFeedback

let ignoreStreamEvents: ('a -> int -> unit) * ('a -> string -> unit) =
    let noop2 = fun _ _ -> ()
    noop2, noop2

let stubCstmt = fun _ _ -> "fake statement"

[<Property>]
let ``runUnbound over an empty input stream should return an empty list``
    (qsVal: int)
    (numRows: int)
    =
    let onStreamEvent = ignoreStreamEvents

    let cStmt = fun _ _ -> qsVal
    let bind = C K
    let execStreamQry = fun _ -> "reader"

    let enumStream =
        fun _ _ -> Seq.empty<string>

    let inTransaction _ _ _ : Result<int, string> = failwith "invalid state"
    let execFeedbackQry = fun _ -> numRows

    let proc _ : Result<int, string> = failwith "invalid state"

    runUnbound
        onStreamEvent
        (cStmt, bind, execStreamQry, enumStream, inTransaction, execFeedbackQry)
        (SqlQuery(
            queryText = "give me nothing",
            parameters = "p",
            extractor = fun _ -> ""
        ))
        (SqlCommand(
            newStatement = (fun _ -> 0),
            parameters = "p",
            binder = fun _ c -> c
        ))
        proc
        (1, 2) = List.empty

[<Property>]
let ``runUnbound property 2`` () =
    let execStreamQry _ = 1 // todo: extractor and enumerator...
    true
