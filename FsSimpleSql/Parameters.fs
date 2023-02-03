module FsSimpleSql.Parameters

open System.Data.Common
open Microsoft.Data.SqlClient

let fixParameterName (name: string) =
    if name.StartsWith("@") then name else $"@{name}"

/// <summary>
/// Create a new DbParameter of the given type and '@' name.
/// </summary>
///
/// <remarks>
/// Type-specific utility function via partial application:
/// <code>
/// let newIdParameter = newParameter SqlDbType.Int
/// let personIdParam = newIdParameter "person_id"
/// let hatIdParam = newIdParameter "hat_id"
/// </code>
/// Chaining:
/// <code>
/// newParameter SqlDbType.Int "maxLengthMm" | withValue 10
/// </code>
/// </remarks>
///
/// <param name="name">The name of the parameter.  The name may or may not
/// include the initial '@' character - if not present, it is added before
/// creating the parameter.</param>
/// <param name="dbType">The column type of the parameter. See
/// <see cref="System.Data.Common.SqlDbType">SqlDbType</see></param>
let newParameter dbType (name: string) : DbParameter =
    SqlParameter(fixParameterName name, dbType)

let havingSize size (parameter: DbParameter) =
    parameter.Size <- size
    parameter

/// <summary>
/// Equivalent to <c>newParameter type name |> havingSize size</c>.
/// </summary>
let newSizedParameter dbType size name : DbParameter =
    SqlParameter(fixParameterName name, dbType, size)

let havingPrecision precision (parameter: DbParameter) =
    parameter.Precision <- precision
    parameter

let havingScale scale (parameter: DbParameter) =
    parameter.Scale <- scale
    parameter

let inline withValue value (parameter: DbParameter) =
    parameter.Value <- value
    parameter
