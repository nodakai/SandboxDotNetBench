namespace ABP.Bench

module FSharp =
    type Bench0001() =
        interface IBench with
            member this.Run n =
                let rec f n k =
                    if k = 0 || k = n then
                        1
                    else
                        f (n-1) (k-1) + f (n-1) k
                f n (n / 2) |> box

module Program =
    open System

    let benchlist = Map.ofList [
        1, fun () -> {| fs = FSharp.Bench0001() :> IBench; cs = CSharp.Bench0001() :> IBench |}
    ]

    let removeOutliers arr =
        Array.Sort(arr)
        ArraySegment(arr, offset=1, count=arr.Length - 2)

    let meanVar arr =
        printfn "%A" arr
        let a = Seq.average arr
        let ss = Seq.sumBy (fun e -> (e - a) ** 2.) arr  // n * sample var
        let n = Seq.length arr |> double
        let v = ss / (n - 1.5)  // 1.5: prepare for an unbiased estimator of pop.stddev
        let sd = sqrt v
        {| mean = a; sd = sd |}

    [<EntryPoint>]
    let main argv =
        let mutable n = 30
        if argv.Length < 1 then
            failwith "choose which bench to run"
        let ok, which = Int32.TryParse(argv.[0])
        if not ok then
            failwith <| sprintf "invalid bench #: '%s'" argv.[0]
        if 1 < argv.Length then
            let ok, n0 = Int32.TryParse(argv.[1])
            if not ok then
                failwith <| sprintf "invalid n: '%s'" argv.[1]
            n <- n0
        let factory = benchlist.[which]
        let benchmarkers = factory()
        let rep = 5
        let run name (b: IBench) =
            printf "running %s %d ... " name which
            b.Run(5) |> ignore  // warmup
            let run _ =
                let sw = Diagnostics.Stopwatch()
                sw.Start()
                let res = b.Run(n)
                let elapsed = double sw.ElapsedMilliseconds
                printfn "%A" res
                elapsed
            let raw = Array.init rep run |> removeOutliers |> meanVar
            {| raw with name = name |}
        let f = run "F#" benchmarkers.fs
        let c = run "C#" benchmarkers.cs
        let winner, loser = if f.mean > c.mean then c, f else f, c
        let pct = loser.mean / winner.mean - 1. |> ((*) 100.)
        printfn "%d: %s %.0f +/- %.2f ms vs %s %.0f +/- %.2f ms (%s is %.2f %% faster)" which f.name f.mean f.sd c.name c.mean c.sd winner.name pct
        0 // return an integer exit code
