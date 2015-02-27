namespace Rex

open System
open System.Runtime.CompilerServices
open System.Text

module Regex =

    type internal Op =
        | And = 0
        | Alt = 1
        | Seq = 2

    [<StructuralEquality; NoComparison>]
    type Regular<'t> when 't : equality =
        internal
        | Done of bool
        | Single of 't
        | App of Op * 't Regular[]
        | Not of 't Regular
        | EOF
        with
            override this.ToString() =
                let rec toString (sb : StringBuilder) =
                    function
                    | Done r -> sb.Append("Done(").Append(r).Append(')')
                    | Single t -> sb.Append(''').Append(t.ToString()).Append(''')
                    | App (op, subs) -> 
                        sb.Append(op).Append('(') |> ignore
                        Array.tryHead subs |> Option.iter (toString sb >> ignore) 
                        if subs.Length > 1 then // Seq.skip throws if inufficient :(
                            Seq.skip 1 subs |> Seq.iter (fun x -> toString (sb.Append(", ")) x |> ignore)
                        sb.Append(')')
                    | Not r -> (toString (sb.Append("Not(")) r).Append(')')
                    | EOF -> sb.Append("EOF")

                (toString (StringBuilder()) this).ToString()

            member this.Diff x =
                match this with
                | Done r -> Done r
                | Single y -> Done (x = y)
                | App (op, list) -> Done false // todo
                | Not r -> Not (this.Diff x)
                | EOF -> Done false


    // Smart constructors:

    let Success = Regular.Done true
    let Failure = Regular.Done false

    [<Extension>]
    let Not = function
        | Done r -> Done (not r)
        | Not r -> r
        | other -> Regular.Not other