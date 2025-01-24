(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val move : direction -> t -> t
  val read : t -> char
  val write : char -> t -> t
  val print : t -> unit
end

module Tape : TAPE = struct
  type t = char list * char list

  let make niz = ([], List.init (String.length niz) (fun x -> niz.[x])) 

  let move smer trak = 
    match smer with
    | Left -> 
      (match trak with
      | ([],ys) -> ([],' ' :: ys)
      | (x :: xs,ys) -> (xs, x::ys))
    | Right -> 
      (match trak with
      | (xs,[]) -> (' ' :: xs ,[])
      | (xs , y :: ys) -> (y :: xs, ys)
) 
  let read = 
    function
    | (_,[]) -> ' '
    | (_,y::ys) -> y 

  let write znak =
    function
    | (xs, []) -> (xs,[znak])
    | (xs, y :: ys) -> (xs , znak :: ys) 

  let rec odstrani_presledke_levo  =
     function
     | [] -> []
     | ' ' :: xs -> odstrani_presledke_levo xs
     | xs -> xs

  let print (xs,ys) = 
    let leva_stran = String.of_seq (xs |> List.rev |> odstrani_presledke_levo |> List.to_seq) in
    let desna_stran = String.of_seq (ys |> List.rev |> odstrani_presledke_levo |> List.rev |> List.to_seq) in
    let index_glave = String.length leva_stran in
    print_string ( leva_stran ^ desna_stran ^ "\n" ^ (String.make index_glave ' ') ^ "^\n")

end

let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

(* AVL Drevesa *)

module AVLDrevo = struct
    type 'a t = Prazno | Sestavljeno of int * 'a t * 'a * 'a t
  
    let rec vsebuje mn x =
      match mn with
      | Prazno -> false
      | Sestavljeno (_, l, y, d) when x = y -> true
      | Sestavljeno (_, l, y, d) when x < y -> vsebuje l x
      | Sestavljeno (_, l, y, d) when x > y -> vsebuje d x
      | _ -> assert false
  
    let prazno = Prazno
  
    let rec velikost = function
      | Prazno -> 0
      | Sestavljeno (_, l, _, d) -> 1 + velikost l + velikost d
  
    let visina drevo =
      match drevo with
      | Prazno -> 0
      | Sestavljeno (h, _, _, _) -> h
  
    let sestavljeno (l, x, d) =
      Sestavljeno (1 + max (visina l) (visina d), l, x, d)
  
    let zavrti_levo = function
      | Sestavljeno (_, l, x, Sestavljeno (_, dl, y, dd)) ->
          sestavljeno (sestavljeno (l, x, dl), y, dd)
      | _ -> failwith "Tega drevesa ne morem zavrteti"
  
    let zavrti_desno = function
      | Sestavljeno (_, Sestavljeno (_, ll, y, ld), x, d) ->
          sestavljeno (ll, y, sestavljeno (ld, x, d))
      | _ -> failwith "Tega drevesa ne morem zavrteti"
  
    let razlika = function
      | Prazno -> 0
      | Sestavljeno (_, l, _, d) -> visina l - visina d
  
    let uravnotezi drevo =
      match drevo with
      | Sestavljeno (_, l, x, d) when razlika drevo = 2 && razlika l = 1 ->
          zavrti_desno drevo
      | Sestavljeno (_, l, x, d) when razlika drevo = 2 ->
          sestavljeno (zavrti_levo l, x, d) |> zavrti_desno
      | Sestavljeno (_, l, x, d) when razlika drevo = -2 && razlika d = -1 ->
          zavrti_levo drevo
      | Sestavljeno (_, l, x, d) when razlika drevo = -2 ->
          sestavljeno (l, x, zavrti_desno d) |> zavrti_levo
      | _ -> drevo
  
    let rec isci x drevo =
      match drevo with
      | Prazno -> false
      | Sestavljeno (_, l, vrednost, d) ->
          if x < vrednost then isci x l
          else if x > vrednost then isci x d
          else true

    let rec drevo_match_ukaz x match_function drevo =
        match drevo with
        | Prazno -> None
        | Sestavljeno (_, l, vrednost, d) ->
            if x < (match_function vrednost) then drevo_match_ukaz x match_function l
            else if x > (match_function vrednost) then drevo_match_ukaz x match_function d
            else Some vrednost

    let rec drevo_spremeni_ukaz x match_function change_function drevo =
        match drevo with
        | Prazno -> failwith ("Ni naslo ukaza: " ^ x)
        | Sestavljeno (n, l, vrednost, d) ->
            if x < (match_function vrednost) then sestavljeno ((drevo_spremeni_ukaz x match_function change_function l), vrednost, d)
            else if x > (match_function vrednost) then sestavljeno (l, vrednost, (drevo_spremeni_ukaz x match_function change_function d))
            else sestavljeno (l, change_function vrednost, d) 

  
    let rec dodaj x drevo =
      match drevo with
      | Prazno -> 
        Sestavljeno (1, Prazno, x, Prazno)
      | Sestavljeno (h, l, vrednost, d) ->
          if x < vrednost then sestavljeno (dodaj x l, vrednost, d) |> uravnotezi
          else if x > vrednost then
            sestavljeno (l, vrednost, dodaj x d) |> uravnotezi
          else drevo
  end

(* Prekopirano iz zapiskov z par spremembami*)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end


module Machine : MACHINE = struct
  type ukaz = (char * string * char * direction)

  type t = string * ( (string * ukaz AVLDrevo.t) AVLDrevo.t)

  let make state state_list =
    let zacetno_drevo = AVLDrevo.dodaj (state, AVLDrevo.prazno) AVLDrevo.prazno in
    (state, List.fold_left (fun tree new_state ->  AVLDrevo.dodaj (new_state, AVLDrevo.prazno) tree) zacetno_drevo state_list)

  let initial (state,_) = state
  let add_transition stanje kljuc novo_stanje nov_znak smer (zacetno_stanje, stroj_drevo) = 
    (zacetno_stanje, AVLDrevo.drevo_spremeni_ukaz stanje (fun (ukaz,_) -> ukaz) (fun (stanje', stanje_drevo) -> (stanje', AVLDrevo.dodaj (kljuc, novo_stanje, nov_znak, smer) stanje_drevo)) stroj_drevo)



  let step (stanje, stroj) trenutno_stanje trak = 
    match AVLDrevo.drevo_match_ukaz trenutno_stanje (fun (ukaz, drevo) -> ukaz) stroj with
        | None -> None
        | Some (_, novo_drevo) -> 
            match AVLDrevo.drevo_match_ukaz (Tape.read trak) (fun (kljuc, _ , _ , _) -> kljuc) novo_drevo with
            | None -> None
            | Some (kljuc, stanje', nov_znak, smer) -> Some (stanje', trak |> (Tape.write nov_znak) |> (Tape.move smer))


end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

(* let test1 = Machine.(
    make "right" [ "carry"; "done" ]) *)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run stroj niz =
    let trak = Tape.make niz in
    let zacetno_stanje = Machine.initial stroj in
    Tape.print trak;
    print_string (zacetno_stanje ^ "\n");
    let rec slow_run' stroj stanje trak =
        match Machine.step stroj stanje trak with
            | None -> ()
            | Some (novo_stanje, nov_trak) -> 
                Tape.print nov_trak;
                print_string (novo_stanje ^ "\n");
                slow_run' stroj novo_stanje nov_trak
    in
    slow_run' stroj zacetno_stanje trak

let primer_slow_run =
  slow_run binary_increment "1011"
(*
1011
^
right
1011
  ^
right
1011
  ^
right
1011
    ^
right
1011
    ^
right
1011
    ^
carry
1010
  ^
carry
1000
  ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run stroj niz =
    let trak = Tape.make niz in
    let zacetno_stanje = Machine.initial stroj in
    let rec speed_run' stroj stanje trak =
        match Machine.step stroj stanje trak with
            | None -> Tape.print trak
            | Some (novo_stanje, nov_trak) -> 
                speed_run' stroj novo_stanje nov_trak
    in
    speed_run' stroj zacetno_stanje trak


let primer_speed_run =
  speed_run binary_increment "1011"
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)

let for_state stanje list stroj = List.fold_left (fun stroj funkcija -> funkcija stanje stroj) stroj list

let for_character znak poteza = poteza znak

let for_characters niz poteza stanje stroj = String.fold_left (fun x znak -> (for_character znak poteza) stanje x) stroj niz

let move smer = fun znak stanje stroj -> Machine.add_transition stanje znak stanje znak smer stroj

let switch_and_move novo_stanje smer = fun znak stanje stroj -> Machine.add_transition stanje znak novo_stanje znak smer stroj

let write_and_move nov_znak smer = fun znak stanje stroj -> Machine.add_transition stanje znak stanje nov_znak smer stroj

let write_switch_and_move nov_znak novo_stanje smer = fun znak stanje stroj -> Machine.add_transition stanje znak novo_stanje nov_znak smer stroj

let binary_increment' =
    Machine.make "right" ["carry"; "done"]
    |> for_state "right" [
      for_characters "01" @@ move Right;
      for_character ' ' @@ switch_and_move "carry" Left
    ]
    |> for_state "carry" [
      for_character '1' @@ write_and_move '0' Left;
      for_characters "0 " @@ write_switch_and_move '1' "done" Left
    ] 
(* val binary_increment' : Machine.t = <abstr> *)



(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse = Machine.make "zacetek" ["zacetek_drugi_del";"isci";"prenesi_0";"prenesi_1";"odlozi_0";"odlozi_1";"ponovi";"pocisti";"na_zacetek";"done"]
    |> for_state "zacetek" [
        for_characters "01" @@ move Right;
        for_character ' ' @@ write_switch_and_move '*' "zacetek_drugi_del" Left
    ]
    |> for_state "zacetek_drugi_del" [
        for_characters "01" @@ move Left;
        for_character ' ' @@ write_switch_and_move '*' "isci" Right
    ]
    |> for_state "isci" [
        for_character ' ' @@ move Right;
        for_character '0' @@ write_switch_and_move ' ' "prenesi_0" Left;
        for_character '1' @@ write_switch_and_move ' ' "prenesi_1" Left;
        for_character '*' @@ write_switch_and_move ' ' "pocisti" Left
    ]
    |> for_state "prenesi_0" [
        for_characters " 01" @@ move Left;
        for_character '*' @@ switch_and_move "odlozi_0" Left
    ]
    |> for_state "prenesi_1" [
        for_characters " 01" @@ move Left;
        for_character '*' @@ switch_and_move "odlozi_1" Left
    ]
    |> for_state "odlozi_0" [ 
        for_characters "01" @@ move Left;
        for_character ' ' @@ write_switch_and_move '0' "ponovi" Right
    ]
    |> for_state "odlozi_1" [ 
        for_characters "01" @@ move Left;
        for_character ' ' @@ write_switch_and_move '1' "ponovi" Right
    ]
    |> for_state "ponovi" [
        for_characters " 01" @@ move Right;
        for_character '*' @@ switch_and_move "isci" Right
    ]
    |> for_state "pocisti" [
        for_character ' ' @@ move Left;
        for_character '*' @@ write_switch_and_move ' ' "na_zacetek" Left
    ]
    |> for_state "na_zacetek" [
        for_characters "01" @@ move Left;
        for_character ' ' @@ switch_and_move "done" Right
    ]


let primer_reverse = speed_run reverse "0000111001"
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate = Machine.make "najdi_premik" ["premakni_0"; "premakni_1"; "ponovi"; "preveri"; "eno_levo"; "podvajaj"; "podvoji_0"; "podvoji_1";"done"]
    |> for_state "najdi_premik" [
        for_character '0' @@ write_switch_and_move ' ' "premakni_0" Left;
        for_character '1' @@ write_switch_and_move ' ' "premakni_1" Left;
        for_character ' ' @@ switch_and_move "ponovi" Right
    ]
    |> for_state "premakni_0" [
        for_character ' ' @@ write_switch_and_move '0' "najdi_premik" Left
    ]
    |> for_state "premakni_1" [
        for_character ' ' @@ write_switch_and_move '1' "najdi_premik" Left
    ]
    |> for_state "ponovi" [
        for_characters "01" @@ switch_and_move "preveri" Right;
        for_character ' ' @@ switch_and_move "eno_levo" Left
    ]
    |> for_state "preveri" [
        for_character ' ' @@ switch_and_move "ponovi" Right;
        for_character '0' @@ switch_and_move "najdi_premik" Left;
        for_character '1' @@ switch_and_move "najdi_premik" Left;
    ]
    |> for_state "eno_levo" [
        for_character ' ' @@ switch_and_move "podvajaj" Left
    ]
    |> for_state "podvajaj" [
        for_character '0' @@ switch_and_move "podvoji_0" Left;
        for_character '1' @@ switch_and_move "podvoji_1" Left;
        for_character ' ' @@ switch_and_move "done" Right
    ]
    |> for_state "podvoji_0" [
        for_character ' ' @@ write_switch_and_move '0' "podvajaj" Left
    ]
    |> for_state "podvoji_1" [
        for_character ' ' @@ write_switch_and_move '1' "podvajaj" Left
    ] 

let primer_duplicate = speed_run duplicate "010011"
(* 
001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = Machine.make "zacetek" ["zmanjsaj";"carry";"dodaj_1";"ponovi";"pocisti";"done"]
    |> for_state "zacetek" [
        for_characters "01" @@ move Right;
        for_character ' ' @@ write_switch_and_move '*' "zmanjsaj" Left
    ]
    |> for_state "zmanjsaj" [
        for_character '0' @@ move Left;
        for_character '1' @@ write_switch_and_move '0' "carry" Right;
        for_character ' ' @@ switch_and_move "pocisti" Right
    ]
    |> for_state "carry" [
        for_character '0' @@ write_and_move '1' Right;
        for_character '*' @@ switch_and_move "dodaj_1" Right
    ]
    |> for_state "dodaj_1" [
        for_character '1' @@ move Right;
        for_character ' ' @@ write_switch_and_move '1' "ponovi" Left
    ]
    |> for_state "ponovi" [
        for_character '1' @@ move Left;
        for_character '*' @@ switch_and_move "zmanjsaj" Left
    ]
    |> for_state "pocisti" [
        for_character '0' @@ write_and_move ' ' Right;
        for_character '*' @@ write_switch_and_move ' ' "done" Right
    ]

let primer_to_unary = speed_run to_unary "1010"
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = Machine.make "zacetek" ["na_start";"odstrani_1";"povecaj";"uncarry";"koncaj";"done"]
    |> for_state "zacetek" [
        for_character '1' @@ move Left;
        for_character ' ' @@ write_switch_and_move '*' "na_start" Right
    ]
    |> for_state "na_start" [
        for_character '1' @@ move Right;
        for_character ' ' @@ switch_and_move "odstrani_1" Left
    ]
    |> for_state "odstrani_1" [
        for_character '1' @@ write_switch_and_move ' ' "povecaj" Left;
        for_character ' ' @@ write_switch_and_move ' ' "done" Left;
        for_character '*' @@ write_switch_and_move ' ' "koncaj" Left
    ]
    |> for_state "povecaj" [
        for_characters "1*" @@ move Left;
        for_characters " 0" @@ write_switch_and_move '1' "uncarry" Right
    ]
    |> for_state "uncarry" [
        for_character '1' @@ write_and_move '0' Right;
        for_character '*' @@ switch_and_move "na_start" Right
    ]
    |> for_state "koncaj" [
        for_characters "01" @@ move Left;
        for_character ' ' @@ switch_and_move "done" Right
    ]

let primer_to_binary = speed_run to_binary (String.make 42 '1')
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)

(* TEST HITROSTI*)

(* 
let busy_beaver5 =
    Machine.(
      make "A" ["B"; "C"; "D"; "E"]
      |> add_transition "A" ' ' "B" '1' Right
      |> add_transition "A" '1' "C" '1' Left
      |> add_transition "B" ' ' "C" '1' Right
      |> add_transition "B" '1' "B" '1' Right
      |> add_transition "C" ' ' "D" '1' Right
      |> add_transition "C" '1' "E" ' ' Left
      |> add_transition "D" ' ' "A" '1' Left
      |> add_transition "D" '1' "D" '1' Left
      |> add_transition "E" '1' "A" ' ' Left
  )

let measure_time f x =
    let start_time = Sys.time () in
    let result = f x in
    let end_time = Sys.time () in
    Printf.printf "Execution time: %f seconds\n" (end_time -. start_time);
    result

let () =
    measure_time (fun () -> speed_run busy_beaver5 "") ()
 *)
