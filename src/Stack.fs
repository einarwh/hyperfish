module Stack 

open Shade

exception StackUnderflowException

type Stack = StackValue list
and StackValue = 
| PictureValue of Picture 
| FunctionValue of Function 
| NumberValue of int 
and Function = {
  Name : string 
  Code : Stack -> Stack
}

let create() : Stack = []

let top stack : StackValue = 
  match stack with
  | [] -> raise StackUnderflowException 
  | h :: t -> h

let pop stack : Stack = 
  match stack with
  | [] -> raise StackUnderflowException 
  | _ :: t -> t

let evaluate (stack : Stack) (fn : Function) : Stack = 
  match fn with 
  | { Name = _
      Code = code } ->
    code stack

let push (stack : Stack) (v : StackValue) : Stack = 
  printfn "pushing value %A" v
  match v with 
  | NumberValue _ -> v :: stack 
  | PictureValue _ -> v :: stack 
  | FunctionValue f -> evaluate stack f

let rec runProgram (stack : Stack) (values : StackValue list) : Stack = 
  match values with 
  | [] -> stack
  | v :: vs -> 
    runProgram (push stack v) vs
