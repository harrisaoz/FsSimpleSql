module FsSimpleSql.Parameters

open System.Data.Common
open Microsoft.Data.SqlClient

let newParameter name dbType : DbParameter = SqlParameter(name, dbType)

let inline withValue value (parameter: DbParameter) =
    parameter.Value <- value
    parameter
