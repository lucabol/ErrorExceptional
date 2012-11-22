module Critical

open System

type Result<'a, 'b> =
| Success of 'a
| Failure of 'b

type Critical() =
       // a -> m a
        member o.Return x       = Success x
        // m a -> (a -> m b) -> m b
        member o.Bind (m, f)    = match m with
                                    | Failure e -> Failure e
                                    | Success x -> f x
        // m a -> m a
        member o.ReturnFrom m   = m

let critical = Critical()

let fault f = f

let contingentGen stopF errF f =
    try
        Success(f ())
    with
        | ex when stopF ex -> Failure(errF ex)
        | _                -> reraise ()


let exceptionMapToFuncs exMap =
    let tryFind ex = exMap |> List.tryFind (fun (k, _) -> k.GetType() = ex.GetType())
    (fun ex ->
        let found = tryFind ex
        match found with Some(_) -> true | None -> false),
    (fun ex ->
        let found = tryFind ex
        match found with
        | Some(k, v)    -> v ex
        | None          -> raise ex)

let contingent0 exMap f =
    let stopF, errF = exceptionMapToFuncs exMap
    contingentGen stopF errF f

let contingent1 exMap f x =
    let stopF, errF = exceptionMapToFuncs exMap
    contingentGen stopF errF (fun _ -> f x)

let contingent2 exMap f x y =
    let stopF, errF = exceptionMapToFuncs exMap
    contingentGen stopF errF (fun _ -> f x y)

let contingent3 exMap f x y z =
    let stopF, errF = exceptionMapToFuncs exMap
    contingentGen stopF errF (fun _ -> f x y z)

let neverThrow0 exc f       = contingentGen (fun _ -> true) (fun ex -> exc ex) f
let neverThrow1 exc f x     = contingentGen (fun _ -> true) (fun ex -> exc ex) (fun _ -> f x)
let neverThrow2 exc f x y   = contingentGen (fun _ -> true) (fun ex -> exc ex) (fun _ -> f x y)
let neverThrow3 exc f x y z = contingentGen (fun _ -> true) (fun ex -> exc ex) (fun _ -> f x y z)

let alwaysThrow exc f x =
    match f x with
    | Success(ret)              -> ret
    | Failure(e)                -> raise (exc e)
        

 
