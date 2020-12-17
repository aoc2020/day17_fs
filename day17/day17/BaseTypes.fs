module day17.BaseTypes

type RawPosition = int64*int64*int64*int64

type Position (x:int64,y:int64,z:int64,h:int64) as self =
    override this.ToString () = sprintf "(%d,%d,%d,%d)" x y z h
    member this.raw : RawPosition = (x,y,z,h)
    member this.neighbours () =
        let adj = [|-1L;0L;1L|]
        in 
            adj |> Seq.map (fun (x1:int64) ->
                adj |> Seq.map (fun (y1:int64) ->
                    adj |> Seq.map (fun (z1:int64) ->
                        adj |> Seq.map (fun (h1:int64) ->
                            if (x1 = 0L |> not) || (y1 = 0L |> not) || (z1 = 0L |> not) || (h1 = 0L |> not) then
                                [|Position(x+x1,y+y1,z+z1,h+h1)|]
                            else
                                [||]
                        ) |> Seq.concat |> Seq.toArray 
                    ) |> Seq.concat |> Seq.toArray 
                ) |> Seq.concat |> Seq.toArray 
            ) |> Seq.concat |> Seq.toArray 

type CubeState =
    | ON
    | OFF

type Cube (state:CubeState,position:Position) as self =
    override this.ToString () = sprintf "Cube(%A @%A" state position
    member this.State = state
    member this.Position = position
    member this.neighbors () = position.neighbours ()
