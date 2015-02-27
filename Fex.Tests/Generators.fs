module Rex.Tests.Generators

open FsCheck

open Rex.Regex

type public Instances =

    static member Regular() =
        { new Arbitrary<Regular<char>>() with
            override x.Generator = 
                let curry f a b = f (a,b)
                let ops = [Op.Alt; Op.And; Op.Seq] |> Seq.map Gen.constant |> Gen.oneof 
                let rec regex' s = 
                    match s with
                    | n when n < 0 -> invalidArg "s" "Only positive arguments are allowed"
                    | 0 -> // leaves
                        Gen.oneof [
                            Gen.map Regular.Done Arb.generate<_>
                            Gen.map Regular.Single Arb.generate<_>
                            Gen.constant Regular.EOF
                        ]
                    | n -> // internal nodes
                        let sub = regex' (n/2)
                        Gen.oneof [
                            Gen.map Not sub
                            Gen.map (curry Regular.App) ops <*> Gen.arrayOf sub
                        ]
                Gen.sized regex'
            override x.Shrinker t = Seq.empty }

