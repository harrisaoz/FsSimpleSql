module FsSimpleSql.Spec.Facades.SyncSeqApplySpec

open FsCheck
open FsCheck.Xunit
open FsSimpleSql.Facades.SyncSeqApply

[<Property>]
let ``map over empty result set is empty sequence`` () =
    let emptyResultSet: int seq = Seq.empty

    let fakeFunctions: GenericFunctions<int, int, int, int> =
        { NewStatement = (fun _ -> 0)
          PrepareStatement = id
          ExecInTx = (fun _ _ -> Result.Ok 1)
          ExecuteQuery = (fun _ -> Ok 0)
          EnumerateResultSet = (fun _ _ -> emptyResultSet) }

    let query = fun _ -> 0
    let extractRecord = fun _ -> 0
    let externalAction = ExternalFunction(fun _ -> Result.Error "should not be reached")
    let proc = fun _ -> 0
    let procRebind = fun _ _ -> 0
    let connections = 2, 1

    let actual = mapGeneric fakeFunctions query extractRecord externalAction proc procRebind connections

    match actual with
    | Ok xs -> Seq.length xs = 0 |> Prop.label "Ok"
    | _ -> false |> Prop.label "Unexpected result of type Error"

[<Property>]
let ``map over query returning one row should have one element`` (data: int, epr: int, efv: int) =
    let expectedProcResult = epr
    let expectedFValue = efv

    let fakeFunctions: GenericFunctions<int, int, int, int> =
        { NewStatement = (fun _ -> 0)
          PrepareStatement = id
          ExecInTx = (fun _ _ -> Result.Ok expectedProcResult)
          ExecuteQuery = (fun _ -> Ok 1)
          EnumerateResultSet = (fun _ _ -> [ data ]) }

    let query = fun _ -> 0
    let extractRecord = id
    let externalAction = ExternalFunction(fun _ -> Result.Ok expectedFValue)
    let proc = fun _ -> 0
    let procRebind = fun _ _ -> 0
    let connections = 2, 1

    let actual =
        mapGeneric fakeFunctions query extractRecord externalAction proc procRebind connections |> Result.map List.ofSeq

    match actual with
    | Error msg -> false |> Prop.label $"Unexpected Result.Error for query result ${msg}"
    | Ok xs ->
        match xs with
        | (d, Result.Ok actualProcValue) :: _ ->
            (d = data && actualProcValue = expectedProcResult)
            |> Prop.label $"d = {d} [expected {data}]"
            |> Prop.label $"proc value = {actualProcValue} [expected {expectedProcResult}]"
        | _ -> false |> Prop.label "Unexpected result structure"

[<Property>]
let ``map over query returning rows should have matching number of elements``
    (
        data: NonEmptyArray<int>,
        eprBasis: int,
        efv: int
    ) =
    let expectedProcResult = if eprBasis >= 0 then 1 else 0
    let expectedFValue = efv
    let dataArray: int array = Array.ofSeq <| data.Get

    let fakeFunctions: GenericFunctions<int, int, int, int> =
        { NewStatement = (fun _ -> 0)
          PrepareStatement = id
          ExecInTx = (fun _ _ -> Result.Ok expectedProcResult)
          ExecuteQuery = (fun _ -> Ok 1)
          EnumerateResultSet = (fun _ _ -> dataArray) }

    let query = fun _ -> 0
    let extractRecord = id
    let externalAction = ExternalFunction(fun _ -> Result.Ok expectedFValue)
    let proc = fun _ -> 0
    let procRebind = fun _ _ -> 0
    let connections = 2, 1

    let actual =
        mapGeneric fakeFunctions query extractRecord externalAction proc procRebind connections
        |> Result.map Array.ofSeq

    let folder (isOk: bool) (output, input) =
        match output with
        | d, Result.Ok actualProcValue -> (isOk && d = input && actualProcValue = expectedProcResult)
        | _ -> false

    match actual with
    | Error msg -> false |> Prop.label $"Unexpected Result.Error for query result ${msg}"
    | Ok xs -> Array.zip xs dataArray |> Array.fold folder true |> Prop.label "Ok"
