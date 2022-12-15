module FsSimpleSql.Spec.ConnectionStringSpec

open FsCheck
open FsCheck.Xunit

open FsCombinators.Core

open FsSimpleSql.ConnectionString
open FsSimpleSql.Spec.ArbConnectionString

[<Property>]
let ``B C (B C) rotates the last three arguments`` (f: int -> int -> int -> int) (x: int) (y: int) (z: int) =
    B C (B C) f x y z = f y z x

[<Properties(Arbitrary = [| typeof<ServerName>
                            typeof<DatabaseName>
                            typeof<ArbConnectionString> |])>]
module WithCustomTypes =
    [<Property>]
    let ``secureMarsDefaults sets MARS to true and intent to read/write`` (ArbConnectionString cs) =
        secureMarsDefaults cs
        |> (fun s ->
            s.Contains "Multiple Active Result Sets=True" &&
            s.Contains "Application Intent=ReadWrite")
        |> Prop.label (secureMarsDefaults cs)
