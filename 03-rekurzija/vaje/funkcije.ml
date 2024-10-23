(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse list = 
  match list with
  | [] -> []
  | x :: xs -> reverse xs @ [x]

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat_napacno crka list =
  match list with
  | [] -> 0
  | x :: xs when x = crka -> 1 + repeat_napacno crka xs
  | x :: xs -> repeat_napacno crka xs


let rec repeat crka stevilo = 
  match stevilo with
  | x when x <= 0 -> []
  | x -> crka :: repeat crka (stevilo - 1)


(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
Pri tem ne smete uporabbiti vgrajene funkcije [List.init].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let range n =
  let rec range' n acc =
    match n with
    | -1 -> acc
    | x -> range' (x - 1) (x :: acc)
  in
  range' n []
  
(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 Pri tem ne smete uporabiti vgrajene funkcije [List.map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f  =
  function
  | [] -> []
  | x :: xs -> f x :: (map f) xs

(*----------------------------------------------------------------------------*]
 Časovna zahtevnost operatorja [@] je linearna v prvem argumentu, poskušajte 
 napisati reverse tako, da bo bolj učinkovit in hkrati repno rekurziven.
 Pri tem ne smete uporabiti vgrajene funkcije [List.rev] ali [List.rev_append].
[*----------------------------------------------------------------------------*)

let reverse_tlr list =
  let rec reverse_tlr' list stack =
    match list with
    | [] -> stack
    | x :: xs -> reverse_tlr' xs (x :: stack)
  in
  reverse_tlr' list []


(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map_tlrec f seznam =
  let rec map_tlrec' f seznam acc_seznam =
      match seznam with
      | [] -> acc_seznam
      | x :: xs -> map_tlrec' f xs (f x :: acc_seznam)
  in
  map_tlrec' f seznam [] |> reverse_tlr

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 Pri tem ne smete uporabiti vgrajene funkcije [List.mapi].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let mapi f list =
  let rec mapi' f list acc_list acc_stevilo = 
    match list with
    | [] -> acc_list
    | x :: xs -> mapi' f xs ((f x acc_stevilo) :: acc_list) (acc_stevilo + 1)
  in
  mapi' f list [] 0 |> reverse_tlr

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije [List.combine].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let zip list1 list2 = 
  let rec zip' list1 list2 acc_list = 
    match list1 with
      |[] ->
      (match list2 with
        |[] -> acc_list
        | _ -> raise (Failure("Different lengths of input lists.")))
      | x :: xs ->
      match list2 with
        | [] -> raise (Failure("Different lengths of input lists."))
        | y :: ys -> zip' xs ys ((x,y) :: acc_list)
    in 
  (zip' list1 list2 []) |> reverse_tlr

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 Pri tem ne smete uporabiti vgrajene funkcije [List.split].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip seznam_parov =
  let rec unzip' seznam_parov acc_list1 acc_list2 =
    match seznam_parov with
    | [] -> (acc_list1 |> reverse_tlr, acc_list2 |> reverse_tlr)
    | (x,y) :: xs -> unzip' xs (x :: acc_list1) (y:: acc_list2)
  in
  unzip' seznam_parov [] []

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec seznam_parov =
  unzip seznam_parov


(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)

let rec loop condition f x =
  match (condition x) with
  | false -> x
  | true -> loop condition f (f x)


(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f list =
  match list with
    | [] -> raise (Failure("Premalo elementov."))
    | _ :: [] -> raise (Failure("Premalo elementov."))
    | [x;y] -> f x y
    | x :: y :: zs -> fold_left_no_acc f ((f x y) :: zs)

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence f x n =
  let rec apply_sequence' f x n acc_list =
    match n > 0 with
    | false -> acc_list
    | true -> apply_sequence' f (f x) (n-1) ((f x) :: acc_list)
  in 
  apply_sequence' f x n [] |> reverse_tlr

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 Pri tem ne smete uporabiti vgrajene funkcije [List.filter].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let filter f list =
  let rec filter' f list acc_list =
    match list with
    | [] -> acc_list
    | x :: xs ->
      match f x with
      | true -> filter' f xs ( x :: acc_list)
      | false -> filter' f xs (acc_list)
  in
  filter' f list [] |> reverse_tlr

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let exists f list = 
  match filter f list with
  | [] -> false
  | _ -> true

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let first f default list =
  match filter f list with
  | [] -> default
  | x :: _ -> x