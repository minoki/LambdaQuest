module ChurchListSpec where
import Test.Hspec
import Data.Either
import LambdaQuest.SystemF

spec :: Spec
spec = do
  -- cList t = forall a. (t -> a -> a) -> a -> a
  let cList t = TyAll "a" $ TyArr (TyArr t (TyArr (TyRef 0 "a") (TyRef 0 "a"))) (TyArr (TyRef 0 "a") (TyRef 0 "a"))
  let econs = parseTerm "" "?t. \\hd:t. \\tl:(forall a. (t -> a -> a) -> a -> a). ?a. \\c:t->a->a. \\n:a. c hd (tl [a] c n)"
  let enil = parseTerm "" "?t. ?a. \\c:t->a->a. \\n:a. n"
  let eisnil = parseTerm "" "?t. \\l:(forall a. (t -> a -> a) -> a -> a). l [Bool] (\\x:t. \\y:Bool.False) True"
  let esumInt = parseTerm "" "\\l:(forall a. (Int -> a -> a) -> a -> a). l [Int] addInt 0"
  describe "SystemF.Parse with Church lists" $ do
    it "can parse ListInt type" $ do
      parseType "" "forall a. (Int -> a -> a) -> a -> a" `shouldBe` Right (cList TyInt)
    it "can parse cons" $ econs `shouldSatisfy` isRight
    it "can parse nil" $ enil `shouldSatisfy` isRight
    it "can parse isnil" $ eisnil `shouldSatisfy` isRight
    it "can parse sumInt" $ esumInt `shouldSatisfy` isRight

  let rest = do
        cons <- econs
        nil <- enil
        isnil <- eisnil
        sumInt <- esumInt
        let consInt = TTyApp cons TyInt
        let nilInt = TTyApp nil TyInt
        let isnilInt = TTyApp isnil TyInt
        let aList = TApp (TApp consInt (TPrimValue (PVInt 1))) $ TApp (TApp consInt (TPrimValue (PVInt 2))) $ TApp (TApp consInt (TPrimValue (PVInt 3))) nilInt
        return $ do
          describe "SystemF.TypeCheck with Church lists" $ do
            it "can type check cons" $ do
              typeOf [] cons `shouldBe` Right (TyAll "t" $ TyArr (TyRef 0 "t") $ TyArr (cList (TyRef 1 "t")) (cList (TyRef 1 "t")))
            it "can type check nil" $ do
              typeOf [] nil `shouldBe` Right (TyAll "t" $ cList (TyRef 1 "t"))
            it "can type check isnil" $ do
              typeOf [] isnil `shouldBe` Right (TyAll "t" $ TyArr (cList (TyRef 1 "t")) TyBool)
            it "can type check sumInt" $ do
              typeOf [] sumInt `shouldBe` Right (TyArr (cList TyInt) TyInt)
            it "can type check a integer list" $ do
              typeOf [] aList `shouldBe` Right (cList TyInt)
          describe "SystemF.Eval with Church lists" $ do
            it "isnil [Int] (nil [Int]) = True" $ do
              eval [] (TApp isnilInt nilInt) `shouldBe` Right (TPrimValue (PVBool True))
            it "isnil [Int] <some list>" $ do
              eval [] (TApp isnilInt aList) `shouldBe` Right (TPrimValue (PVBool False))
            it "sumInt <list 1 2 3> = 6" $ do
              eval [] (TApp sumInt aList) `shouldBe` Right (TPrimValue (PVInt 6))
  case rest of
    Left _ -> return ()
    Right a -> a
