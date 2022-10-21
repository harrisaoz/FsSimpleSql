module FsSimpleSql.Records

open System.Collections.Generic
open System.Data
open System.Data.Common
open Microsoft.Data.SqlClient

type RecordEnumerator<'a> (reader: DbDataReader, deserialize: DbDataReader -> 'a) =
    interface IEnumerator<'a> with
        member this.Current: 'a = 
            deserialize reader

        member this.Current: obj = 
            (deserialize reader) :> obj

        member this.Dispose(): unit =
            reader.Dispose()

        member this.MoveNext(): bool =
            reader.HasRows && reader.Read()

        member this.Reset(): unit = 
            raise (System.NotImplementedException())

    interface IEnumerable<'a> with
        member this.GetEnumerator(): System.Collections.IEnumerator =
            this :> System.Collections.IEnumerator

        member this.GetEnumerator(): IEnumerator<'a> =
            this :> IEnumerator<'a>

let enumerateResultSet (extract: DbDataReader -> 'a) reader =
    new RecordEnumerator<'a>(reader, extract)

let getNotNull (reader: DbDataReader) (columnValueByIndex: int -> 'a) (columnName: string) =
    columnValueByIndex(reader.GetOrdinal columnName)

let getNullable (reader: DbDataReader) (columnValueByIndex: int -> 'a) (columnName: string) =
    let ordinal = reader.GetOrdinal columnName
    if reader.IsDBNull(ordinal) then
        Option.None
    else
        Option.Some (columnValueByIndex ordinal)

let getColumnValue (_: 'a) (reader: DbDataReader) (name: string) =
    reader.GetFieldValue<'a>(name)

let bindColumnValue (dbType: SqlDbType) (name: string) (value: 'a) =
    let commandParameter = SqlParameter($"@{name}", dbType)
    commandParameter.Value <- value
    commandParameter

let bindSizedColumnValue (dbType: SqlDbType) (length: int) (name: string) (value: 'a) =
    let commandParameter = SqlParameter($"@{name}", dbType, length)
    commandParameter.Value <- value
    commandParameter
