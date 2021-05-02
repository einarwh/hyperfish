module Stack 

  exception StackUnderflowException

  type Stack = StackValue list
  and StackValue = 
  | PictureValue of Picture 
  | FunctionValue of Function 
  | NumberValue of int 
  and Picture = Picture 
  and Function = {
    Name : string 
    Code : Stack -> Stack
  }

  let create() : Stack = []

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
    match v with 
    | NumberValue _ -> v :: stack 
    | PictureValue _ -> v :: stack 
    | FunctionValue f -> evaluate stack f



