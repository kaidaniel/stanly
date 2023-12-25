Functor:     use single-argument functions on a structure (unwrap a single argument)
Applicative: use  multi-argument functions on a structure (unwrap several arguments)
Monad:       use a structure  in functions on a structure (unwrap return values)

Applicative: 
Your function knows nothing about any wrappers, but your arguments are all wrapped.

Monad:
The function creates values that need to be wrapped.

Functor:
e.g.
f :: a -> (b -> c)
a :: f a
b :: f b
fmap f :: f a -> f (b -> c)
fmap f a :: f (b -> c)
fmap _ (fmap f a) :: fb -> fc 
fmap = <$>  // infixl 4
fmap (const 1)   

but there is no '?' s.t. "? (fmap f a) b" is typeable.

Functor
fmap :: forall ab.(a->b)->fa->fb  // NOT the same as f(a->b)->fa->fb
fmap id == id
fmap (f . g) == fmap f . fmap g
fmap (g >>> f) == fmap g >>> fmap f

Bifunctor
bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

e.g.
bimap {Str -> ()} {Int -> T} {Either Str Int} :: Either () T


e.g.
fmap @Maybe = \f Just x.Just(f x) | f None.None     <- implementation has to decide: how to produce value for fn
fmap @List  = \f \[].[] | x:xs.fx:(fmap f xs) 

fmap (\a b c.a+b+c) Just1 ~~> Just(\abc.a+b+c) 1 ~> Just \bc.1+b+c  <- implementation has to decide: how to produce more than one value for fn
                                                                       (but if they are produced independently the implementation doesn't suffer much..)


Functor: manipulate a given structure
    fmap::(a-b)-fa-fb
Applicative: combine several given structures
    (pure::a-fa)                             // lift  <=> fmap, <*>
    lift::(a-b-c)-fa-fb-fc                   // pure  <=> 'convertible from a'
    (fmap::(a-b)-fa-fb)                      // <*> pure ==> fmap (one-way)
Monad: make structures yourself
    (pure::a-fa)
    join::ffa-fa
    (fmap::(a-b)-fa-fb)


fmap::(a-b)-fa-fb      // (a-b) simple to write, but applied to a complicated structre.    e.g. intraprocedural passes on a compiler IR: passes are unary fns
pure::a-fa             // any function can be given a new meaning (inside the structure).  e.g. any object can be made a part of a container.
lift::(a-b-c)-fa-fb-fv // (a-b-c) can even combine arguments w/o knowing what they are.    e.g. interprocedural passes on a compiler IR.
join::ffa-fa           // there's only one structure.                                      e.g. throwing an error while handling one (?)
                                                                                           e.g. a state-transformer whose value is a state-transformer is still a state-transformer.
                                                                                                - if my code produces a state transformer, I can combine it with an existing state transformer. (vs lift: I can combine two existing state transformers. vs. map: I can modify an existing state transformer)

fmap f x = == join ((fmap \x' -> pure (f x')) x)
fmap f x == join ((fmap \x' -> (lift id) (pure f) (pure x')) x)
fmap f x == join ((lift id) pure (\x' -> (lift id) (pure f) (pure x')) x)
fmap == join . fmap . pure
join == join . fmap id

fmap@List  = fix \rec.\f \[].[] | x:xs.fx:(rec f xs) 
pure@List  = \x.[x]
lift@List  = fix \rec.\f.\[x][y].(f x y) | x:xs y:ys.(f x y):(rec f xs y:ys):(rec f x:xs ys) | etc
join@List  = fix \rec.   \[l].l          | [l:ls].l<>rec[ls]                                 | etc.
                 

pure x >>= f == f x
  x >>= pure == x
  x >>= \x'.fx >>= g == (x >>= f) >>= g
  join x = x >>= id
  join(fmap f x) == (>>=)
  \x::fa g::a-fb.join(fmap g x)::fb == >>=
  fmap f x == x >>= \x' -> return (f x')
  join (fmap f x) == x >>= \x' -> return (f x') == (fmap f x) >>= id

<*> ::f(a-b)-fa-fb
<*> = lift id
pure id <*> x = x
pure (.) <*> x <*> y <*> z == x <*> (y <*> z)


lift f x y = (fmap f x) <*> y == (pure f <*> x) <*> y
fmap f x  == pure f <*> x

pure f             == ..
pure f <*> x       == fmap f x
pure f <*> x <*> y == lift f x y

lift f x y = (fmap f x) <*> y

(pure . f) x = ((map f) . pure) x = x

(a-b-c)-fa-fc
(a-b-c)-fa-f(b-c)
\f a.map (f a)

or, simpler:
lift_fn : (a-b-c)-(fa-fb-fc)
lift_obj : a-fa

fmap f x == pure f <*> x   <- this shows why functor is a bit artificial imho.
pure == return
x <*> y = x >>= \x' -> y >>= \y' -> return (x' y')

lift id
(all abc)-(a-b-c)-fa-fb-fc (all a)a-a
(all bc)-f(b-c)-fb-fc

fmap@Maybe = \f Just x.Just(f x) | f None.None
lift@Maybe = \f \Nothing _.Nothing | _ Nothing.Nothing | Justx Justy.Just(lift f x y)

fmap@List  = \f \[].[] | x:xs.fx:(fmap f xs) 
lift@ZipList  = \f \[] _.[] | _ [].[] | x:xs y:ys.(f x y):(lift f xs ys)

pure@List  = \x.[x]
<*>@ZipList   = \[]_.[] | _[].[] | f:fs x:xs.fx:fs<*>xs 

[b-c]-[b]-[c]                         id::(b-c)-b-c
lift@ZipList id = \[]_.[]|_[].[]|x:fs x:ys.(f y):(lift id fs xs)

fmap id == id
fmap (f . g) == fmap f . fmap g

fmap::(a-b)-fa-b
fmap=\f::a-b a::fa.lift (\() x.fx) () a 
-> don't need functor if you have applicative

id@a-b = \f.f

lift@List = \f.\   []    _ .[]
                |  _    [] .[] 
                | x:xs y:ys.(f x y):(lift f xs y:ys):(lift f x:xs ys)
                | [x]  [y] .(f x y)


Functor:     You can *transform* values in the structure (using single-argument functions).
Applicative: You can *combine* values in the structure (using multi-argument functions).



# Tree Set

data Set a where
  Leaf  :: (Ord a) => a -> Set a
  Branch  :: (Ord a) => Set a -> Set a -> Set a

instance (Show a) => Show (Set a) where
  show = \case
    Leaf a -> show a
    Branch l r -> "(" <> show l <> " " <> show r <> ")"

instance Eq (Set a) where
  Leaf a == Leaf b = a == b
  Leaf _ == Branch{} = False
  Branch{} == Leaf _ = False
  Branch l r == Branch l' r' = l == l' && r == r'

ubound, lbound :: Set a -> a
ubound = \case Leaf a -> a; Branch _ r -> ubound r
lbound = \case Leaf a -> a; Branch l _ -> lbound l
size :: Set a -> Int
size = \case Leaf _ -> 1; Branch l r -> size l + size r
height :: Set a -> Int
height = \case Leaf _ -> 0; Branch l r -> 1 + max (height l) (height r)

-- >>> (Leaf 2 <> Leaf 1)
-- (1 (2 3))

instance Ord (Set a) where
  Leaf a <= Leaf b = a <= b
  Leaf a <= Branch _ r = a <= ubound r
  Branch l _ <= Leaf a = lbound l <= a
  Branch l _ <= Branch _ r' = l <= r'

instance Semigroup (Set a) where
  a <> b  | a == b           = a 
          | b < a            = b <> a
  Leaf a     <> Leaf b       = Branch (Leaf a) (Leaf b)
  Leaf a     <> Branch l r   = Branch (Leaf a <> l) r
  Branch l r <> Leaf a       = Branch l (r <> Leaf a)
  b <> Branch l' r' 
    | b <= l' = Branch (b <> l') r'
    | b <= r' && size l' < size r' = Branch (l' <> b) r'
    | otherwise = Branch l' (b <> r')