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

(* IZOMORFIZMI *)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

let phi1 (x,y) = (y,x)
let psi1 = phi1

(* Še malo pustimo *)

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
    
let predznak je_najvisji_clen =
    function
    | 0 -> ""
    | n when n > 0 -> if je_najvisji_clen then "" else " + " 
    | n -> if je_najvisji_clen then "-" else " - "

let koeficient potenca =
    let x = if potenca = 0 then "" else " x" in
    function
    | 0 -> ""
    | 1 | -1 when not(potenca = 0) -> "x"
    | n -> string_of_int (abs(n)) ^ x

let clen stevilo potenca je_najvisji_clen =
    match stevilo = 0 with
    | true -> ""
    | false -> predznak je_najvisji_clen stevilo ^ koeficient potenca stevilo ^ potence_izpis potenca

let izpis polinom = 
    let rec izpis' polinom potenca acc_string =
        match polinom with
        | [] -> acc_string
        | x :: [] -> clen x potenca true ^ acc_string
        | x :: xs -> izpis' xs (potenca + 1) (clen x potenca false ^ acc_string)
    in
    izpis' polinom 0 ""

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

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"

let abeceda = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A') 


(* SUBSTITUCIJSKA ŠIFRA *)

(* Šifriranje *) 

let sifriraj_crko sifra znak =
    if 'A' <= znak && znak <= 'Z' then
        String.get sifra (indeks znak)
    else
        znak


let sifriraj sifra besedilo = 
    String.map (sifriraj_crko sifra) besedilo 
    
(* Inverzni ključ *)

let inverz_crke sifra znak =
    if 'A' <= znak && znak <= 'Z' then
        crka (String.index sifra znak)
    else
        znak

let inverz sifra =
    String.map (inverz_crke sifra) (String.sub abeceda 0 (String.length sifra))


(* Ugibanje ključa *)

let besede = "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see him two has look more day could go come did number sound no most people my over know water than call first who may down side been now find any new work part take get place made live where after back little only round man year came show every good me give our under name very through just form sentence great think say help low line differ turn cause much mean before move right boy old too same tell does set three want air well also play small end put home read hand port large spell add even land here must big high such follow act why ask men change went light kind off need house picture try us again animal point mother world near build self earth father head stand own page should country found answer school grow study still learn plant cover food sun four between state keep eye never last let thought city tree cross farm hard start might story saw far sea draw left late run don't while press close night real life few north open seem together next white children begin got walk example ease paper group always music those both mark often letter until mile river car feet care second book carry took science eat room friend began idea fish mountain stop once base hear horse cut sure watch color face wood main enough plain girl usual young ready above ever red list though feel talk bird soon body dog family direct pose leave song measure door product black short numeral class wind question happen complete ship area half rock order fire south problem piece told knew pass since top whole king space heard best hour better true . during hundred five remember step early hold west ground interest reach fast verb sing listen six table travel less morning ten simple several vowel toward war lay against pattern slow center love person money serve appear road map rain rule govern pull cold notice voice unit power town fine certain fly fall lead cry dark machine note wait plan figure star box noun field rest correct able pound done beauty drive stood contain front teach week final gave green oh quick develop ocean warm free minute strong special mind behind clear tail produce fact street inch multiply nothing course stay wheel full force blue object decide surface deep moon island foot system busy test record boat common gold possible plane stead dry wonder laugh thousand ago ran check game shape equate hot miss brought heat snow tire bring yes distant fill east paint language among grand ball yet wave drop heart am present heavy dance engine position arm wide sail material size vary settle speak weight general ice matter circle pair include divide syllable felt perhaps pick sudden count square reason length represent art subject region energy hunt probable bed brother egg ride cell believe fraction forest sit race window store summer train sleep prove lone leg exercise wall catch mount wish sky board joy winter sat written wild instrument kept glass grass cow job edge sign visit past soft fun bright gas weather month million bear finish happy hope flower clothe strange gone jump baby eight village meet root buy raise solve metal whether push seven paragraph third shall held hair describe cook floor either result burn hill safe cat century consider type law bit coast copy phrase silent tall sand soil roll temperature finger industry value fight lie beat excite natural view sense ear else quite broke case middle kill son lake moment scale loud spring observe child straight consonant nation dictionary milk speed method organ pay age section dress cloud surprise quiet stone tiny climb cool design poor lot experiment bottom key iron single stick flat twenty skin smile crease hole trade melody trip office receive row mouth exact symbol die least trouble shout except wrote seed tone join suggest clean break lady yard rise bad blow oil blood touch grew cent mix team wire cost lost brown wear garden equal sent choose fell fit flow fair bank collect save control decimal gentle woman captain practice separate difficult doctor please protect noon whose locate ring character insect caught period indicate radio spoke atom human history effect electric expect crop modern element hit student corner party supply bone rail imagine provide agree thus capital won't chair danger fruit rich thick soldier process operate guess necessary sharp wing create neighbor wash bat rather crowd corn compare poem string bell depend meat rub tube famous dollar stream fear sight thin triangle planet hurry chief colony clock mine tie enter major fresh search send yellow gun allow print dead spot desert suit current lift rose continue block chart hat sell success company subtract event particular deal swim term opposite wife shoe shoulder spread arrange camp invent cotton born determine quart nine truck noise level chance gather shop stretch throw shine property column molecule select wrong gray repeat require broad prepare salt nose plural anger claim continent oxygen sugar death pretty skill women season solution magnet silver thank branch match suffix especially fig afraid huge sister steel discuss forward similar guide experience score apple bought led pitch coat mass card band rope slip win dream evening condition feed tool total basic smell valley nor double seat arrive master track parent shore division sheet substance favor connect post spend chord fat glad original share station dad bread charge proper bar offer segment slave duck instant market degree populate chick dear enemy reply drink occur support speech nature range steam motion path liquid log meant quotient teeth shell neck"


let slovar = besede |> String.uppercase_ascii |> (String.split_on_char ' ')

(* Razširjanje ključa s črko *)

let zamenjaj_znak niz mesto nov_znak =
    String.sub niz 0 mesto ^ nov_znak ^ String.sub niz (mesto +1 ) (String.length niz - mesto - 1)



let dodaj_zamenjavo sifra (x,y) =
    match String.equal (String.get sifra (indeks x) |> Char.escaped) "_" && not (String.contains sifra y) with
    | true -> Some (zamenjaj_znak sifra (indeks x) (Char.escaped y)) 
    | false -> 
        (
            match String.equal (String.get sifra (indeks x) |> Char.escaped) (Char.escaped y) with
            | true -> Some sifra
            | false -> None
        )

(* Razširjanje ključa z besedo *)

let vse_razen_zacetek niz =
    String.sub niz 1 (String.length niz - 1)

let dodaj_zamenjave sifra (beseda1,beseda2) =
    let rec dodaj_zamenjave' sifra (beseda1,beseda2) =
        match sifra with
        | None -> None
        | Some sifra' -> 
            (match (beseda1,beseda2) with
            | ("","") -> sifra
            | ("",_) | (_,"") -> None
            | (x,y) -> dodaj_zamenjave' (dodaj_zamenjavo sifra' (String.get x 0,String.get y 0)) (vse_razen_zacetek x,vse_razen_zacetek y))
    in
    dodaj_zamenjave' (Some sifra) (beseda1,beseda2)

(* Vse možne razširitve *)

let mozne_razsiritve kljuc sifra slovar =
    List.filter_map (fun x -> dodaj_zamenjave kljuc (sifra,x)) slovar

(* Odšifriranje *)


let odsifriraj sifra =
    let besede = String.split_on_char ' ' sifra in
    let rec odsifriraj' kljuci besede =
        match besede with
        | [] -> kljuci
        | x :: xs -> odsifriraj' (List.concat_map (fun beseda -> mozne_razsiritve beseda x slovar ) kljuci) xs
    in
    match odsifriraj' [(String.make 26 '_')] besede with
    | [] -> None
    | x :: xs -> Some (sifriraj x sifra)
