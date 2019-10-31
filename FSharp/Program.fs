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

    type FsCsPair = { fs: IBench; cs: IBench }
    type PairFactory = unit -> FsCsPair
    let benchlist: Map<int, PairFactory> = Map.ofList [
        1, fun () -> { fs = FSharp.Bench0001() :> IBench; cs = CSharp.Bench0001() :> IBench}
    ]

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
        let { fs = fs; cs = cs } = factory()
        let run (name: string) (b: IBench) =
            printf "running %s%d ... " name which
            b.Run(5) |> ignore  // warmup
            let sw = Diagnostics.Stopwatch()
            sw.Start()
            b.Run(n) |> printfn "%A"
            sw.ElapsedMilliseconds
        let tfs = run "F#" fs
        let tcs = run "C#" cs
        let winner = if tfs > tcs then "C#" else "F#"
        let pct = (double tfs) / (double tcs) - 1. |> ((*) 100.)
        printfn "%d: %d ms vs %d ms (%s is %.2f %% faster)" which tfs tcs winner pct
        0 // return an integer exit code
