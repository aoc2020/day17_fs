module day17.IO

open System
open System.IO
open day17.BaseTypes

let readFile (filePath:String) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine().ToCharArray()
} 

let toCube (x:int) (y:int) (stateChar:char) : Cube =
    let pos = Position(x |> int64, y |> int64, 0L, 0L)
    let state = if stateChar = '#' then ON else OFF
    Cube (state,pos) 

let readCubes (filePath:String) =
    readFile filePath 
    |> Seq.mapi (fun (y:int) (cubeLine:char[]) ->
        cubeLine |> Seq.mapi (fun (x:int) (stateChar:char) -> toCube x y stateChar))
    |> Seq.concat 
    |> Seq.toArray 
