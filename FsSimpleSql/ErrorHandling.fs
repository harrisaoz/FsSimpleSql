module FsSimpleSql.ErrorHandling

let isFatalException (maybeFatal: System.Exception) =
    match maybeFatal with
    | :? System.OutOfMemoryException as f1 -> true
    | :? System.StackOverflowException as f2 -> true
    | :? System.AccessViolationException as f3 -> true
    | :? System.AppDomainUnloadedException as f4 -> true
    | :? System.BadImageFormatException as f5 -> true
    | :? System.DivideByZeroException as f6 -> true
    | :? System.Threading.ThreadAbortException as f7 -> true
    | _ -> false

let tryMaybeFatal onError onOk f x =
    try
        f x |> onOk
    with ex ->
        if isFatalException ex then
            System.Environment.FailFast(ex.Message)
            // unreachable, since FailFast always exits immediately
            ex |> onError
        else
            ex |> onError

let tryMaybeFatalAsUnit f x =
    let ignore _ = ()
    tryMaybeFatal ignore ignore f x
    
let tryMaybeFatalAsResult f x =
    tryMaybeFatal (fun ex -> ex.Message |> Error) Ok f x
