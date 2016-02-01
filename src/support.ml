open Format
module Error = struct 
 exception Exit of int
 type info = FI of string * int * int | UNKNOWN
 type 'a withinfo = {i: info; v: 'a}
end
