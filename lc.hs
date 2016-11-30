
data ExprText
  = V Int
  | A ExprText ExprText
  | L ExprText
  deriving (Show, Read)

data Expr
  = Var Int
  | App Expr Expr
  | Abs Expr
  | Val Int
  | Func (Expr -> Expr)

load (V v)   = Var v
load (A x y) = App (load x) (load y)
load (L x)   = Abs (load x)

shift :: Expr -> Int -> Int -> Expr
shift (Var x)   i d = Var (if x >= d then x + i else x)
shift (App x y) i d = App (shift x i d) (shift y i d)
shift (Abs x)   i d = Abs (shift x i (d + 1))
shift x         _ _ = x

free :: Expr -> Int -> Bool
free (Var x)   v = (x == v)
free (App x y) v = (free x v) || (free y v)
free (Abs x)   v = free x (v + 1)
free x         _ = True

subs :: Expr -> Int -> Expr -> Expr
subs (Var x)   v e = if x == v then e else Var x
subs (App x y) v e = App (subs x v e) (subs y v e)
subs (Abs x)   v e = Abs (subs x (v + 1) $ shift e 1 0)
subs x         _ _ = x

isbeta :: Expr -> Bool
isbeta (App (Abs _) _) = True
isbeta _               = False

iseta :: Expr -> Bool
iseta (Abs (App x (Var v))) = not(free x v) && v == 0
iseta _                     = False

beta :: Expr -> Expr
beta (App (Abs x) y) = shift (subs x 0 (shift y 1 0)) (-1) 0

eta :: Expr -> Expr
eta (Abs (App x _)) = shift x (-1) 0

fullbeta :: Expr -> Expr
fullbeta (App x y) =
  let r = App (fullbeta x) (fullbeta y) in
    if (isbeta r) then fullbeta (beta r) else r

fullbeta (Abs x) = Abs (fullbeta x)
fullbeta x       = x

normalorder :: Expr -> Expr
normalorder (App (Func f) y) = normalorder (f (normalorder y))
normalorder (App x y) =
  let r = (App (normalorder x) y) in
    if isbeta r then normalorder (beta r) else r

normalorder x = x

instance Show Expr where
  show (Var v)   = show v
  show (App x y) = "(" ++ (show x) ++ " " ++ (show y) ++ ")"
  show (Abs x)   = "λ " ++ (show x)
  show (Val x)   = show x
  show (Func x)  = "#function"

instance Eq Expr where
  (Var x)     == (Var y)     = (x == y)
  (App x0 y0) == (App x1 y1) = (x0 == x1) && (y0 == y1)
  (Abs x)     == (Abs y)     = (x == y)

natify e = App (App e (Func (\(Val v) -> Val (v + 1)))) (Val 0)

main = do
  s <- getContents
  print $ normalorder $ natify $ load (read s :: ExprText)
