module FsSimpleSql.ConnectionString

open Microsoft.Data.SqlClient

open FsCombinators.Core

module StringExt = FsCombinators.StringExtensions

// type AuthenticationData =
//     | TrustedConnection
//     | Server of userId: string * password: string

// let connectionStringAuthenticationSection =
//     function
//     | TrustedConnection -> "Trusted_Connection=true"
//     | Server (userId, password) -> $"Trusted_Connection=false;User={userId};Password={password}"

let defaultCsParamTo parameterName defaultValue xs =
    let matches needle candidate =
        candidate
        |> StringExt.split [| '=' |]
        |> List.ofArray
        |> function
            | [] -> false
            | key :: _ -> StringExt.iequal key needle

    match Array.exists (matches parameterName) xs with
    | false -> Array.insertAt 0 $"{parameterName}={defaultValue}" xs
    | true -> xs

let applyDefaults defaultMap connectionString =
    defaultMap
    |> Map.fold (B C (B C) defaultCsParamTo) (connectionString |> StringExt.split [| ';' |])
    |> String.concat ";"

let secureMarsDefaults =
    applyDefaults (
        Map
            [ ("Application Name", "FsSimpleSql")
              ("Multiple Active Result Sets", "True")
              ("Min Pool Size", "1")
              ("Max Pool Size", "1")
              ("Application Intent", "ReadWrite") ]
    )

let unsecureCqrsDefaults =
    applyDefaults (
        Map
            [ ("Application Name", "FsSimpleSql")
              ("Multiple Active Result Sets", "False")
              ("Min Pool Size", "1")
              ("Max Pool Size", "1")
              ("Trust Server Certificate", "true") ]
    )

let setApplicationIntent intent connectionString =
    let builder =
        SqlConnectionStringBuilder connectionString

    builder.ApplicationIntent <- intent
    builder.ConnectionString
