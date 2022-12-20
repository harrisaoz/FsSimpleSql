module FsSimpleSql.Spec.ConnectionStringValidationSpec

open FsCheck
open FsCheck.Xunit

open FsSimpleSql.ConnectionStringValidation
open FsSimpleSql.ConnectionString

open FsSimpleSql.Spec.ArbConnectionString

[<Property>]
let ``Arbitrary text does not generally satisfy connection string constraints`` (arbText: string) =
    lazy (simpleValidate validationBuilder BasicNetwork.requiredParameters arbText)
    |> Prop.throws<System.ArgumentException, _>

[<Property>]
let ``An otherwise empty connection string with only default secure MARS scenario values should not be considered valid for a basic remote connection``
    ()
    =
    lazy
        (applyDefaults StandardDefaults.secureMars ""
         |> simpleValidate validationBuilder BasicNetwork.requiredParameters)
    |> Prop.throws<System.ArgumentException, _>

[<Properties(Arbitrary = [| typeof<ServerName>
                            typeof<DatabaseName>
                            typeof<ArbConnectionString> |])>]
module WithCustomTypes =
    [<Property>]
    let ``A connection string missing a required parameter should be considered invalid`` (ArbConnectionString s) =
        let required: ConnectionStringParameter list =
            [ IpAddressPreference ]

        lazy (simpleValidate validationBuilder required s)
        |> Prop.throws<System.ArgumentException, _>

    [<Property>]
    let ``A connection string not missing any required parameters should be considered valid``
        (ArbConnectionString cs)
        =
        let required: ConnectionStringParameter list =
            [ Database ]

        simpleValidate validationBuilder required cs = cs
        |> Prop.label cs

    [<Property>]
    let ``Generated database names should always be non-empty`` (DatabaseName s) = String.length s > 0

    [<Property(MaxTest = 50, Verbose = true)>]
    let ``Generated connection strings should be non-empty`` (ArbConnectionString s) = String.length s > 0

    [<Property(MaxTest = 50)>]
    let ``Generated database names should only contain characters`` (DatabaseName s) =
        String.forall System.Char.IsLetter s
