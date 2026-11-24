import Lisp
import Control.Exception (SomeException, evaluate, try)
import Data.List (intercalate)

-- runProgram の結果を期待値と比較する。
-- error による例外も捕まえて NG として表示し、残りのテストを続行する。
check :: String -> Either String Value -> IO ()
check input expected = do
    result <- try $ do
        let actual = runProgram input
            shown  = show actual
        _  <- evaluate (length shown)
        ok <- evaluate (actual == expected)
        return (ok, shown)
    putStrLn $ case result of
        Right (True, shown)  -> "ok  " ++ show input ++ " => " ++ shown
        Right (False, shown) -> "NG  " ++ show input ++ " => " ++ shown ++ expectedMsg
        Left e               -> "NG  " ++ show input ++ " => exception: "
                                    ++ takeWhile (/= '\n') (show (e :: SomeException)) ++ expectedMsg
    where
        expectedMsg = "   (expected " ++ show expected ++ ")"

-- 複数行のプログラムを組み立てる。
-- unlines だと末尾に改行が付き、今のパーサではパースエラーになるため intercalate を使う。
program :: [String] -> String
program = intercalate "\n"

main :: IO ()
main = do
    -- アトム
    check "42"      (Right (VNum 42))
    check "#t"      (Right (VBool True))
    check "#f"      (Right (VBool False))
    check "null"    (Right VNull)
    -- quote
    check "'a"          (Right (VSymbol "a"))
    check "'(1 2)"      (Right (VList [VNum 1, VNum 2]))
    check "'(a (b c))"  (Right (VList [VSymbol "a", VList [VSymbol "b", VSymbol "c"]]))
    -- 組み込み関数
    check "(+ 1 2 3)"       (Right (VNum 6))
    check "(- 10 3 2)"      (Right (VNum 5))
    check "(- 5)"           (Right (VNum (-5)))
    check "(* 2 3 4)"       (Right (VNum 24))
    check "(/ 20 2 5)"      (Right (VNum 2))
    check "(+ 1 (* 2 3))"   (Right (VNum 7))
    check "(list 1 2 3)"    (Right (VList [VNum 1, VNum 2, VNum 3]))
    -- if
    check "(if #t 1 2)"     (Right (VNum 1))
    check "(if #f 1 2)"     (Right (VNum 2))
    -- lambda と関数適用
    check "((lambda (x y) (+ x y)) 1 2)"                (Right (VNum 3))
    check "((lambda (x) ((lambda (y) (+ x y)) 2)) 1)"   (Right (VNum 3))   -- クロージャ
    check "((lambda (f) (f 10)) (lambda (x) (* x 2)))"  (Right (VNum 20))  -- 高階関数
    -- define: 後続のトップレベル式から参照できる
    check "define x 42"                                 (Right (VNum 42))
    check "define x 1 (+ x 2)"                          (Right (VNum 3))
    check "define inc (lambda (n) (+ n 1))\n(inc 41)"   (Right (VNum 42))
    -- defmacro: 引数は評価されずに S式のまま渡され、展開結果が評価される
    check "defmacro unless (c, body) (list 'if c #f body) (unless #f (+ 1 2))"
          (Right (VNum 3))
    check "defmacro unless (c, body) (list 'if c #f body) (unless #t (+ 1 2))"
          (Right (VBool False))
    -- エラー
    check "foo"                     (Left "Undefined symbol: foo")
    check "(/ 1 0)"                 (Left "Division by zero")
    check "(if 1 2 3)"              (Left "Condition must evaluate to a boolean")
    check "((lambda (x) x) 1 2)"    (Left "Incorrect number of arguments")
    -- パース
    check "(1 2"                    (Left "Parse error: unexpected input")

    -- マクロにマクロを渡す
    -- (1) マクロ呼び出しを、別のマクロ呼び出しの引数に渡す
    --     外側の展開結果 (if #f #f (unless #f 42)) の中に残った内側の呼び出しも展開される
    check (program
        [ "defmacro unless (c, body) (list 'if c #f body)"
        , "(unless #f (unless #f 42))"
        ]) (Right (VNum 42))
    check (program
        [ "defmacro unless (c, body) (list 'if c #f body)"
        , "(unless #f (unless #t 42))"
        ]) (Right (VBool False))
    -- (2) マクロの展開結果が、別のマクロの呼び出しになる
    --     (unless #f 42) => (my-if #f #f 42) => (if #f #f 42)
    check (program
        [ "defmacro my-if (c, t, e) (list 'if c t e)"
        , "defmacro unless (c, body) (list 'my-if c #f body)"
        , "(unless #f 42)"
        ]) (Right (VNum 42))
    -- (3) マクロ名を引数として渡す（高階マクロ）
    --     m には シンボル double が束縛され、(call double 21) => (double 21) => (+ 21 21)
    check (program
        [ "defmacro double (x) (list '+ x x)"
        , "defmacro call (m, x) (list m x)"
        , "(call double 21)"
        ]) (Right (VNum 42))
    --     引数が複数あるマクロも渡せる: (apply2 unless #f 42) => (unless #f 42)
    check (program
        [ "defmacro unless (c, body) (list 'if c #f body)"
        , "defmacro apply2 (m, a, b) (list m a b)"
        , "(apply2 unless #f 42)"
        ]) (Right (VNum 42))
    -- (4) 渡されたマクロを2回使う
    --     (twice double 5) => (double (double 5)) => (+ (double 5) (double 5)) => (+ (+ 5 5) (+ 5 5))
    check (program
        [ "defmacro double (x) (list '+ x x)"
        , "defmacro twice (m, x) (list m (list m x))"
        , "(twice double 5)"
        ]) (Right (VNum 20))
    -- (5) 高階マクロの引数に、高階マクロの呼び出しを渡す
    --     (call double (call double 3)) => (+ (call double 3) (call double 3)) => (+ 6 6)
    check (program
        [ "defmacro double (x) (list '+ x x)"
        , "defmacro call (m, x) (list m x)"
        , "(call double (call double 3))"
        ]) (Right (VNum 12))
    -- (6) マクロは値ではないので、関数（lambda）には渡せない
    check (program
        [ "defmacro double (x) (list '+ x x)"
        , "((lambda (f) (f 1)) double)"
        ]) (Left "Expected a value, but found a macro: double")
