module ChurchNumeralSpec where
import Test.Hspec
import Data.Either
import LambdaQuest.SystemF

spec :: Spec
spec = do
  let eNatType = parseType "" "forall a. (a -> a) -> a -> a"
  let ec0 = parseTerm "" "?a. \\s:a -> a. \\z:a. z"
  let ec1 = parseTerm "" "?a. \\s:a -> a. \\z:a. s z"
  let ec2 = parseTerm "" "?a. \\s:a -> a. \\z:a. s (s z)"
  let ecsucc = parseTerm "" "\\n:(forall a. (a -> a) -> a -> a). ?a. \\s:a->a. \\z:a. s (n [a] s z)"
  let ecplus = parseTerm "" "\\n:(forall a. (a -> a) -> a -> a). \\m:(forall a.(a->a)->a->a). ?a. \\s:a->a. \\z:a. (m [a] s) (n [a] s z)"
  let ectimes = parseTerm "" "\\n:(forall a. (a -> a) -> a -> a). \\m:(forall a.(a->a)->a->a). ?a. \\s:a->a. \\z:a. (n [a] (m [a] s) z)"
  let ecnat2int = parseTerm "" "\\n:(forall a. (a -> a) -> a -> a). n [Int] (addInt 1) 0"

  describe "SystemF.Parse with Church numerals" $ do
    it "can parse Nat type" $ do
      eNatType `shouldBe` Right (TyAll "a" $ TyArr (TyArr (TyRef 0 "a") (TyRef 0 "a")) (TyArr (TyRef 0 "a") (TyRef 0 "a")))
    it "can parse c0" $ do
      ec0 `shouldBe` Right (TTyAbs "a" $ TAbs "s" (TyArr (TyRef 0 "a") (TyRef 0 "a")) $ TAbs "z" (TyRef 0 "a") $ TRef 0 "z")
    it "can parse c1" $ ec1 `shouldSatisfy` isRight
    it "can parse c2" $ ec2 `shouldSatisfy` isRight
    it "can parse csucc" $ ecsucc `shouldSatisfy` isRight
    it "can parse cplus" $ ecplus `shouldSatisfy` isRight
    it "can parse ctimes" $ ectimes `shouldSatisfy` isRight
    it "can parse cnat2int" $ ecnat2int `shouldSatisfy` isRight

  let rest = do
        natType <- eNatType
        c0 <- ec0
        c1 <- ec1
        c2 <- ec2
        csucc <- ecsucc
        cplus <- ecplus
        ctimes <- ectimes
        cnat2int <- ecnat2int
        return $ do
          describe "SystemF.TypeCheck with Church numerals" $ do
            it "can type check c0" $ do
              typeOf [] c0 `shouldBe` Right natType
            it "can type check c1" $ do
              typeOf [] c1 `shouldBe` Right natType
            it "can type check c2" $ do
              typeOf [] c2 `shouldBe` Right natType
            it "can type check csucc" $ do
              typeOf [] csucc `shouldBe` Right (TyArr natType natType)
            it "can type check cplus" $ do
              typeOf [] cplus `shouldBe` Right (TyArr natType (TyArr natType natType))
            it "can type check ctimes" $ do
              typeOf [] ctimes `shouldBe` Right (TyArr natType (TyArr natType natType))
            it "can type check cnat2int" $ do
              typeOf [] cnat2int `shouldBe` Right (TyArr natType TyInt)

          describe "SystemF.Eval with Church numerals" $ do
            it "cnat2int c0 = 0" $ do
              eval [] (TApp cnat2int c0) `shouldBe` Right (TPrimValue (PVInt 0))
            it "cnat2int c1 = 1" $ do
              eval [] (TApp cnat2int c1) `shouldBe` Right (TPrimValue (PVInt 1))
            it "cnat2int c2 = 2" $ do
              eval [] (TApp cnat2int c2) `shouldBe` Right (TPrimValue (PVInt 2))
            it "cnat2int (csucc c1) = 2" $ do
              eval [] (TApp cnat2int (TApp csucc c1)) `shouldBe` Right (TPrimValue (PVInt 2))
            it "cnat2int (csucc c2) = 3" $ do
              eval [] (TApp cnat2int (TApp csucc c2)) `shouldBe` Right (TPrimValue (PVInt 3))
            it "cnat2int (csucc (csucc c2)) = 4" $ do
              eval [] (TApp cnat2int (TApp csucc (TApp csucc c2))) `shouldBe` Right (TPrimValue (PVInt 4))
            it "cnat2int (cplus c1 c1) = 2" $ do
              eval [] (TApp cnat2int (TApp (TApp cplus c1) c1)) `shouldBe` Right (TPrimValue (PVInt 2))
            it "cnat2int (ctimes c2 c2) = 4" $ do
              eval [] (TApp cnat2int (TApp (TApp ctimes c2) c2)) `shouldBe` Right (TPrimValue (PVInt 4))
  case rest of
    Left _ -> return ()
    Right a -> a
