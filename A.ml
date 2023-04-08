(*Contadores do numero de iterações*)

let  a = ref 0
let  b = ref 0

(*Primeiro Método*)

let rec arth n = 
  a := !a + 1;
  if n < 0 then -10
  else if n = 0 then 1 
  else if n = 1 then 2 
  else 
  let sum = ref 0 in
  for k = 1 to n - 2 do
  sum :=  !sum + (arth(k))  * (arth(n - k - 1));
  done; 
  3*arth(n - 1) + !sum

(*Segundo Método*)

let rec s n = 
  b := !b + 1;
  if float_of_int n < 0. then -10.
  else if float_of_int n = 0. then 1.
  else if float_of_int n = 1. then 2.
  else (6. *. (float_of_int n) -. 3. ) /. ( (float_of_int n) +. 1. ) *. s (int_of_float((float_of_int n) -. 1.)) -. ( (float_of_int n) -. 2. ) /. ( (float_of_int n) +. 1. ) *. s (int_of_float((float_of_int n) -. 2.))

(*Otimização do segundo Método usando zarith*)

let zero   = Z.zero
let one    = Z.one
let two    = Z.succ one
let three  = Z.succ two
let four   = Z.succ three
let five   = Z.succ four
let six    = Z.succ five
let ( +> ) = Z.add (* a soma nos inteiros Z *)
let ( *> ) = Z.mul (* a multiplicação nos inteiros Z *)
let ( /> ) = Z.div (* a divisão nos inteiros Z *)
let ( >- ) = Z.sub (* a subtraçao nos inteiros Z*)

module Posicao =
  struct
    type t = (Z.t)
    let compare n1 n2 = compare n1 n2
  end 
  
module Zarass = Map.Make (Posicao) 

let z =
  let table = ref Zarass.empty in
  let rec otimiz m =
  try Zarass.find m !table
  with Not_found ->
  let rec ret =
      if Z.lt m  zero then invalid_arg "Error"
      else if Z.equal m  zero then one
      else if Z.equal m  one then two
      else ((six *> m >- three ) *> otimiz(m >- one) >- (m >- two) *> otimiz(m >- two)) /> (m +> one)
    in table := Zarass.add m ret !table; ret in otimiz;;


(*Visualização do Programa*)

let (n, m) = Scanf.scanf " %d %d" (fun a b -> (a, b))
let numt = arth(n)
let nums = s(n)
let () = if numt <> -10 then Printf.printf "%d %d\n" numt !a else Printf.printf "Fatal error: exception Invalid_argument\n"
let () = if nums <> -10. then Printf.printf "%d %d\n" (int_of_float (nums)) !b else Printf.printf "Fatal error: exception Invalid_argument\n"
let () = Printf.printf "%s\n" (Z.to_string((z(Z.of_int(m)))))

(*
Exemplo de execução:

 O utilizador deve colocar um número, clicar no enter e depois colocar outro número;
 A primeira linha do output será o resultado da equação de Schröder com o n utilizado sendo o primeiro número colocado pelo 
utilizador, seguido do número de vezes que a função recursiva foi chamada
 A segunda linha será igual à primeira mas o número de vezes que a função foi chamada será agora correspondente ao número de
chamadas feitas pela segunda fórmula;
 A terceira linha será o resultado da equação utilizando o segundo número.


 Exemplo:
 Input:7
       123
 Output:8558 169
        8558 41
        8557572425033666215686176761173379981842706730578965474568974827521364024130801056762994342
*)