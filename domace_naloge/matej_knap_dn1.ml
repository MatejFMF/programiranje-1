(* 1. DOMAČA NALOGA *)
(* Matej Knap *)

(* Pomožne funkcije *)

let reverse_tlr list =
    let rec reverse_tlr' list stack =
      match list with
      | [] -> stack
      | x :: xs -> reverse_tlr' xs (x :: stack)
    in
    reverse_tlr' list []

let map_tlrec f seznam =
  let rec map_tlrec' f seznam acc_seznam =
      match seznam with
      | [] -> acc_seznam
      | x :: xs -> map_tlrec' f xs (f x :: acc_seznam)
  in
  map_tlrec' f seznam [] |> reverse_tlr

(* Stevke *)

let stevke baza stevilo = 
    let rec stevke' baza stevilo acc_list =
        match stevilo < baza with
        | true -> stevilo :: acc_list
        | false -> stevke' baza (stevilo / baza)  (stevilo mod baza :: acc_list)
    in
    stevke' baza stevilo [] 
 
(* Začetek seznama *)

let take stevilo seznam = 
    let rec take' stevilo seznam acc =
        match seznam with
        | [] -> acc
        | _ when stevilo <= 0 -> acc
        | x :: xs -> take' (stevilo - 1) xs (x :: acc)
    in
    take' stevilo seznam [] |> reverse_tlr

(* Odstranjevanje ujemajočih *)

let rec drop_while f seznam = 
    match seznam with
    | [] -> []
    | x :: xs when f x -> drop_while f xs
    | x -> x

(* Funkcija filter_mapi *)

let filter_mapi f_opt seznam =
    let rec filter_mapi' f_opt seznam acc_list stevilo =
        match seznam with
        | [] -> acc_list
        | x :: xs ->
            match f_opt stevilo x with
            | None -> filter_mapi' f_opt xs acc_list (stevilo+1)
            | Some y -> filter_mapi' f_opt xs (y :: acc_list) (stevilo+1)
    in
    filter_mapi' f_opt seznam [] 0 |> reverse_tlr

(* Opomba: ta koda je dobra *)

(* Izomorfizme še malo pustimo *)

(* POLINOMI *)

type polinom = int list

(* Odstranjevanje odvečnih ničel *)

let pocisti polinom =
    let rec pocisti' polinom_obrnjen =
        match polinom_obrnjen with
        | [] -> []
        | x :: xs -> 
            match x = 0 with
            | true -> pocisti' xs
            | false -> reverse_tlr (x :: xs)
    in
    pocisti' (reverse_tlr polinom)

(* Seštevanje *)

let sestej polinom1 polinom2 =
    let rec sestej' polinom1 polinom2 acc =
        match (polinom1,polinom2) with
        | ([],_ ) -> reverse_tlr polinom2 @ acc
        | (_,[]) -> reverse_tlr polinom1 @ acc
        | (x::xs,y::ys) -> sestej' xs ys ((x + y) :: acc)
    in
    sestej' polinom1 polinom2 [] |> reverse_tlr

let ( +++ ) polinom1 polinom2 = (sestej polinom1 polinom2) |> pocisti

(* Množenje *)

let mnozenje polinom1 polinom2 =
    let rec mnozenje' polinom1 polinom2 nicle acc =
        match polinom1 with
        | [] -> acc
        | x :: xs -> mnozenje' xs polinom2 (0 :: nicle) ((nicle @ (map_tlrec ( fun a -> a * x ) polinom2)) +++ acc)
    in
    mnozenje' polinom1 polinom2 [] []

let ( *** ) polinom1 polinom2 = mnozenje polinom1 polinom2


(* Izračun vrednosti v točki *)

let potenciranje x n =
    let rec potenciranje' x n acc =
        match n > 0 with
        | true -> potenciranje' x (n-1) (acc * x)
        | false -> acc
    in
    potenciranje' x n 1

let vrednost polinom tocka =
    let rec vrednost' polinom tocka potenca acc =
        match polinom with
        | [] -> acc
        | x :: xs -> vrednost' xs tocka (potenca + 1) (acc + x * (potenciranje tocka potenca))
    in
    vrednost' polinom tocka 0 0

(* Odvodi *)

let odvod polinom = 
    let rec odvod' polinom stevilo acc =
        match polinom with
        | [] -> acc
        | x :: xs -> 
            match stevilo with
            | 0 -> odvod' xs (stevilo+1) acc
            | _ -> odvod' xs (stevilo+1) ((stevilo * x) :: acc)
    in
    odvod' polinom 0 [] |> reverse_tlr

(* Lep izpis *)

let rec potence_izpis_pomozna =
    function
    | 0 -> "⁰"
    | 1 -> "¹"
    | 2 -> "²"
    | 3 -> "³"
    | 4 -> "⁴"
    | 5 -> "⁵"
    | 6 -> "⁶"
    | 7 -> "⁷"
    | 8 -> "⁸"
    | 9 -> "⁹"
    | x -> potence_izpis_pomozna (x / 10) ^ (potence_izpis_pomozna (x mod 10))

let potence_izpis =
    function
    | 0 -> ""
    | 1 -> ""
    | x -> potence_izpis_pomozna x
    
let koeficienti stevilo zacetek=
    match zacetek with
    | true -> 
        (match stevilo with
            | 0 -> ""
            | 1 -> "x"
            | -1 -> "-x"
            | n -> string_of_int n ^ "x"
        )
    | false -> 
        (match stevilo with
            | 0 -> ""
            | 1 -> " + x"
            | -1 -> " - x"
            | n when n > 0 -> " + " ^ string_of_int n ^ "x"
            | n -> " - " ^ string_of_int(-n) ^ "x"
        )




(* let izpis polinom =
    let rec izpis' polinom potenca acc_string =
        match polinom with
        | [] -> acc_string
        | x :: xs when potenca = 0 -> izpis' xs (potenca + 1) ( string_of_int(abs(x)) ^ (fun x)) ^ acc_string
        | x :: [] -> koeficienti x true ^ potence_izpis potenca  ^ acc_string
        | 0 :: xs -> izpis' xs (potenca + 1) (acc_string)
        | x :: xs -> izpis' xs (potenca + 1) ( (koeficienti x false  ^ potence_izpis potenca) ^ acc_string)
    in
    izpis' polinom 0 "" *)


(* SAMODEJNO ODVAJANJE *)

type odvedljiva = (float -> float) * (float -> float)

(* Vrednost odvoda *)

let vrednost funkcija : odvedljiva =
    match funkcija with
    | (f,_) -> f 

let odvod =
    function
    | (_,f') -> f'

(* Osnovne funkcije *)

let konstanta k =
    ((fun x -> x *. k),(fun _ -> k))

let identiteta : odvedljiva =
    ((fun x -> x),(fun x-> 1.))

(* Produkt in kvocient *)

let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
    fun (f, f') (g, g') -> ((fun x -> f x +. g x), (fun x -> f' x +. g' x))

let ( **. ) (func1,odv1) (func2,odv2) = ((fun x -> func1 x *. func2 x),(fun x -> func1 x *. odv2 x +. func2 x *. odv2 x))

let ( //. ) (func1,odv1) (func2,odv2) = ((fun x -> func1 x /. func2 x),(fun x -> (func1 x *. odv2 x -. func2 x *. odv2 x) /. (func2 x *. func2 x)))

(* Kompozitum *)

let ( @@. ) (func1,odv1) (func2,odv2) = ((fun x -> func1 @@ func2 x),(fun x -> odv1 @@ func2 x *. odv2 x ))


(* SUBSTITUCIJSKA ŠIFRA *)

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A') 


(* Šifriranje *)

let sifriraj sifra besedilo = 
    String.map besedilo 
    