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

let extractWith: (DbDataReader -> 'a) -> DbDataReader -> IEnumerable<'a> =
    fun extractor reader ->
        new RecordEnumerator<'a>(reader, extractor)

let getNotNull (reader: DbDataReader) (columnValueByIndex: int -> 'a) (columnName: string) =
    columnValueByIndex(reader.GetOrdinal columnName)

let getNullable (reader: DbDataReader) (columnValueByIndex: int -> 'a) (columnName: string) =
    let ordinal = reader.GetOrdinal columnName
    if reader.IsDBNull(ordinal) then
        Option.None
    else
        Option.Some (columnValueByIndex ordinal)

let bindColumnValue (dbType: SqlDbType) (name: string) (value: 'a) =
    let commandParameter = SqlParameter($"@{name}", dbType)
    commandParameter.Value <- value
    commandParameter

type RecordExtractor (reader: DbDataReader) =
    member this.GetNotNull(columnValueByIndex: int -> 'a, columnName: string): 'a =
        getNotNull reader columnValueByIndex columnName

    member this.GetNullable(columnValueByIndex: int -> 'a, columnName: string): 'a option =
        getNullable reader columnValueByIndex columnName

    member this.GetInt32(columnName: string): int32 option =
        getNullable reader reader.GetInt32 columnName

    member this.GetString(columnName: string): string option =
        getNullable reader reader.GetString columnName

let readRecord' reader (shaper: RecordExtractor -> 'a) =
    shaper (RecordExtractor(reader))

let enumerateResultSet (extract: RecordExtractor -> 'a) reader =
    new RecordEnumerator<'a>(reader, RecordExtractor >> extract)
