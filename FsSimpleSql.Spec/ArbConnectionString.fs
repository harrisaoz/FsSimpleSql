module FsSimpleSql.Spec.ArbConnectionString

open FsCheck

open Microsoft.Data.SqlClient

let extendNonEmptyString p f =
    Arb.Default.NonEmptyString().Generator
    |> Gen.filter (fun (NonEmptyString s) -> s |> Seq.forall p)
    |> Gen.map (fun (NonEmptyString s) -> f s)
    |> Arb.fromGen

type ServerName =
    | ServerName of string
    static member String() =
        extendNonEmptyString System.Char.IsLetter ServerName

type DatabaseName =
    | DatabaseName of string
    static member String() =
        extendNonEmptyString System.Char.IsLetter DatabaseName

type ArbConnectionString =
    | ArbConnectionString of string
    static member String() =
        let builder = SqlConnectionStringBuilder ""

        let cs (builder: SqlConnectionStringBuilder) = builder.ConnectionString

        let buildFromRequiredParameters (ServerName server) (DatabaseName database) =
            builder.DataSource <- server
            builder.InitialCatalog <- database
            builder

        gen {
            let! server = Arb.generate<ServerName>
            let! database = Arb.generate<DatabaseName>
            return (buildFromRequiredParameters server database |> cs)
        }
        |> Gen.resize 10
        |> Arb.fromGen
