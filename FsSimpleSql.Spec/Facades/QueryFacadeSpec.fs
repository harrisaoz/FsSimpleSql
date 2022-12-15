module FsSimpleSql.Spec.Facades.QueryFacadeSpec

open FsCheck
open FsCheck.Xunit
open FsSimpleSql.Facades

module Qf = QueryFacade

type S = S of int * string

[<Property>]
let ``newQueryStatement example``
    (init0: int)
    (incOnNewStmt: int)
    (incOnPrepare: int)
    (NonEmptyString queryText)
    (NonEmptyString prepSignal)
    =
    let newStatementTransform = fun n -> n + incOnNewStmt
    let prepareStatementTransform = fun n -> n + incOnPrepare
    let newStatement = fun n -> S(newStatementTransform n, "")
    let withText = fun qt (S(n, _)) -> S(n, qt)

    let prepare =
        fun (S(n, s)) -> S(prepareStatementTransform n, $"{prepSignal}{s}")

    let dependencies =
        Qf.PrepareQueryStatementDependencies(newStatement, withText, prepare)

    let ctx = Qf.PrepareQueryStatementContext(Qf.QueryText queryText)

    let data = init0

    Qf.prepareStatement dependencies ctx data = S(
        init0 |> (newStatementTransform >> prepareStatementTransform),
        $"{prepSignal}{queryText}"
    )

type R = R of int
type Rec = Rec of int

/// Generate the seq { R n, R (n-1), ..., R 0 }
let seqFromR f (R n) =
    Seq.unfold
        (fun (R k) -> if k > 0 then Some(f (R k), R(k - 1)) else None)
        (R n)

open FsCombinators.Core

[<Property>]
let ``executeQuery example`` (PositiveInt stmtValue) =
    let bind = C K
    let exec = R

    let enumerateRs = seqFromR

    let dependencies = Qf.ExecuteStatementDependencies(bind, exec, enumerateRs)

    let context = Qf.ExecuteStatementContext([||], (fun (R n) -> Rec n))

    let data = stmtValue

    let exp n = [ n .. -1 .. 1 ] |> List.map Rec
    let expected = exp stmtValue

    let actual = Qf.executeStatement dependencies context data |> List.ofSeq

    actual = expected
    |> Prop.label (string actual)
    |> Prop.label (string expected)
