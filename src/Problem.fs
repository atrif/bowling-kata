module Bowling

open System;

type Frame = Strike | Spare of int | Score of int * int

let score (rolls:seq<char>) =
    let frameSeq = 
        let rollsAsStr = String.Concat rolls
        let framesSplitted = rollsAsStr.Split([|' '|])
        let getScoreFromFrame (frameRolls:seq<char>): Frame=
            match frameRolls |> Array.ofSeq with
            | [| 'X' |] -> Strike
            | [| '-';'/' |] -> Spare 0
            | [| i; '/' |] when i >= '0' && i <= '9' -> Spare (int i - int '0')
            | [| '-'; '-' |] -> Score (0, 0)
            | [| '-'; i2 |] when i2 >= '0' && i2 <= '9'-> Score (0, (int i2 - int '0'))
            | [| i1; '-' |] when i1 >= '0' && i1 <= '9'-> Score ((int i1 - int '0'), 0)
            | [| i1; i2 |] when i1 >= '0' && i1 <= '9' && i2 >= '0' && i2 <= '9'-> Score ((int i1 - int '0'), (int i2 - int '0'))

        framesSplitted
        |> Seq.filter (fun s -> s <> "")
        |> Seq.map getScoreFromFrame

    let getScoreFromFrameTuple (f1, f2, f3) =
        match (f1,f2, f3) with
        | (Strike, Strike, Strike) -> 30
        | (Strike, Strike, Spare i) -> 20 + i
        | (Strike, Strike, Score (i,_)) -> 20 + i
        | (Strike, Spare i, _) -> 20
        | (Strike, Score (i1, i2), _) -> 10 + i1 + i2
        | (Spare _, Strike, _) -> 10 + 10
        | (Spare _, Spare i, _) -> 10 + i
        | (Spare _, Score (i1, _), _) -> 10 + i1
        | (Score (i1, i2), _, _) -> i1 + i2

    let framesFromSecond =
        if (Seq.length frameSeq) <= 1 
        then Seq.replicate 1 (Score (0, 0))
        else
            [| Score (0, 0) |] 
            |> Seq.append (frameSeq |> Seq.skip 1)
        
    let framesFromThird =
        if (Seq.length frameSeq) <= 2 
        then Seq.replicate 2 (Score (0, 0))
        else
            [| Score (0, 0); Score (0, 0) |] 
            |> Seq.append (frameSeq |> Seq.skip 2)

    Seq.zip3 frameSeq framesFromSecond framesFromThird
    |> Seq.map getScoreFromFrameTuple
    |> Seq.sum
