def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

#check (concat ["a", "b"] ["c", "d"])

def reverse {A : Type} : List A → List A :=
  fun xs =>
    match xs with
    | [] => []
    | x :: xs' => concat (reverse xs') [x]


#check (reverse ["a", "b", "c", "d"])

def length {A : Type} : List A → Nat :=
  fun xs =>
    match xs with
    | [] => 0
    | _ :: xs' => 1 + length xs'


#check (length ["a", "b", "c", "d"])

theorem trd1  {A : Type} {x : A} : reverse [x] = [x] :=
  by
    simp [reverse]
    simp [concat]


theorem trd2 {A : Type} {xs ys : List A} : length (concat xs ys) = length xs + length ys :=
  by
    induction xs with
    | nil =>
      simp [concat,length]
    | cons x xs' ih =>
      simp [concat]
      simp [length]
      rw [ih]
      rw [Nat.add_assoc]

-- Tega poznamo že iz predavanj
theorem trd3 {A : Type} {xs : List A} : concat xs [] = xs :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]

theorem trd4 {A : Type} {xs ys zs : List A} : concat (concat xs ys) zs = concat xs (concat ys zs) :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]


theorem trd5 {A : Type} {xs ys : List A} : reverse (concat xs ys) = concat (reverse ys) (reverse xs) :=
  by
    induction xs with
    | nil =>
      simp [reverse,concat,trd3]
    | cons x xs' ih =>
      simp [concat,reverse]
      rw [ih]
      rw [trd4]

theorem trd6 {A : Type} {xs : List A} : length (reverse xs) = length xs :=
  by
    induction xs with
    | nil =>
     simp [reverse]
    | cons x xs' ih =>
      simp[reverse,length]
      rw [trd2]
      simp [length]
      rw [ih]
      simp [Nat.add_comm]

theorem trd7 {A : Type} {xs : List A} : reverse (reverse xs) = xs :=
  by
    induction xs with
    | nil =>
      simp [reverse]
    | cons x xs' ih =>
      simp [reverse]
      rw [trd5]
      simp [reverse,concat]
      rw [ih]

def map {A B : Type} : (A → B) → List A → List B :=
  fun f list =>
    match list with
    | [] => []
    | x :: xs => f x :: map f xs

theorem map_assoc {A B C : Type} {f : A → B} {g : B → C} {xs : List A} : map g (map f xs) = map (g ∘ f) xs :=
  by
    induction xs with
    | nil =>
      simp [map]
    | cons x xs' ih =>
      simp [map]
      rw [ih]

theorem map_id {A : Type} {xs : List A} : map id xs = xs :=
  by
    induction xs with
    | nil =>
      simp [map]
    | cons x xs' ih =>
      simp [map]
      rw [ih]


theorem map_concat {A B : Type} {f : A → B} {xs ys : List A} : map f (concat xs ys) = concat (map f xs) (map f ys) :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat,map]
      rw [ih]


theorem map_reverse {A B : Type} {f : A → B} {xs : List A} : map f (reverse xs) = reverse (map f xs) :=
  by
    induction xs with
    | nil =>
      simp [reverse,map]
    | cons x xs' ih =>
      simp [reverse]
      rw [map_concat]
      simp [map]
      rw [ih]

inductive tree (A : Type) : Type where
  | empty : tree A
  | node : A → tree A → tree A → tree A

#check tree.rec

#check id

def tree_map {A B : Type} : (A → B) → tree A → tree B :=
  fun f t =>
    match t with
    | tree.empty => tree.empty
    | tree.node x y z => tree.node (f x) (tree_map f y) (tree_map f z)


theorem tree_map_empty {A B : Type} {f : A → B} : tree_map f tree.empty = tree.empty :=
  by
    simp [tree_map]

theorem tree_map_comp {A B C : Type} {f : A → B} {g : B → C} {t : tree A} : tree_map g (tree_map f t) = tree_map (g ∘ f) t :=
  by
    induction t with
    | empty =>
      simp [tree_map]
    | node x l r lih rih =>
      simp [tree_map]
      simp [lih,rih]



def depth {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + Nat.max (depth l) (depth r)

-- S tem se ne bomo ukvarjali
theorem max_comm {a b : Nat} : Nat.max a b = Nat.max b a :=
  by
    simp [Nat.max_comm]

def mirror {A : Type} : tree A → tree A :=
  fun t =>
    match t with
    | tree.empty => tree.empty
    | tree.node x l r => tree.node x (mirror r) (mirror l)

theorem mirror_depth {A : Type} {t : tree A} : depth (mirror t) = depth t :=
  by
    induction t with
    | empty =>
      simp [mirror]
    | node x l r lih rih =>
      simp [mirror,depth]
      simp [rih,lih]
      simp [max_comm]

theorem mirror_mirror {A : Type} {t : tree A} : mirror (mirror t) = t :=
  by
    induction t with
    | empty =>
      simp [mirror]
    | node x l r lih rih =>
      simp [mirror]
      simp [lih,rih]

def collect {A : Type} : tree A → List A :=
  fun t =>
    match t with
    | tree.empty => []
    | tree.node x l r => concat (collect l) (concat [x]  (collect r))

theorem trd8 {A : Type} {x : A} {xs ys : List A} : concat xs (x::ys) = concat (concat xs [x]) ys :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x' xs' ih =>
      simp [concat]
      rw [ih]

theorem collect_mirror {A : Type} {t : tree A} : collect (mirror t) = reverse (collect t) :=
  by
    induction t with
    | empty =>
      simp [mirror,collect,reverse]
    | node x l r lih rih =>
     simp [mirror]
     simp [collect]
     simp [lih,rih]
     simp [trd5]
     simp [reverse,concat]
     rw [trd8]




def size {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + (size l) + (size r)

theorem size_mirror {A : Type} {t : tree A} : size (mirror t) = size t :=
<<<<<<< HEAD
  by
    induction t with
    | empty =>
      simp [mirror]
    | node x l r lih rih =>
      simp [mirror,size,lih,rih]
      simp [Nat.add_assoc]
      simp [Nat.add_comm]
=======
  sorry


--- Indukcija na pomožnih funkcijah z akumulatorjem

theorem concat2 : concat xs (x :: ys) = concat (concat (xs) [x]) ys :=
  by
    sorry

-- Definirajte repno rekurzivno funkcijo, ki obrne seznam
def reverse' {A : Type} : List A → List A :=
  sorry

-- Dokažite, da je vaša funkcija pravilna
theorem reverse_eq_reverse' {A : Type} : ∀ {xs : List A}, reverse xs = reverse' xs :=
  by
    sorry
>>>>>>> c914e27fb585a66bde37488fb77b29fbced94c4c
