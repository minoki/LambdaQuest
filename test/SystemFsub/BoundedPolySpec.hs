module SystemFsub.BoundedPolySpec where
import Test.Hspec
import Data.Either
import LambdaQuest.SystemFsub

spec :: Spec
spec = do
  let etm = parseTerm "" "?a <: Real. \\x:a. ?b <: a. \\y: b. addReal x y"
  let ety = parseType "" "forall a <: Real. a -> (forall b <: a. b -> Real)"
  describe "SystemFSub.Parse" $ do
    it "can parse bounded quantified type" $ do
      ety `shouldBe` Right (TyAll "a" TyReal $ TyArr (TyRef 0 "a") $ TyAll "b" (TyRef 1 "a") $ TyArr (TyRef 0 "b") TyReal)
      -- ety `shouldSatisfy` isRight
    it "can parse bounded quantified term" $ do
      etm `shouldBe` Right (TTyAbs "a" TyReal $ TAbs "x" (TyRef 0 "a") $ TTyAbs "b" (TyRef 1 "a") $ TAbs "y" (TyRef 0 "b") $ TApp (TApp (TPrimValue $ PVBuiltinBinary BAddReal) (TRef 2 "x")) (TRef 0 "y"))

  either (const $ return ()) id $ do
    tm <- etm
    ty <- ety
    return $ do
      describe "SystemFsub.TypeCheck" $ do
        it "can type check b" $ do
          typeOf [] tm `shouldBe` Right ty
      describe "SystemFsub.Eval" $ do
        it "b [Int] 1 [Int] 2 = 3" $ do
          eval [] (TApp (TTyApp (TApp (TTyApp tm TyInt) (TPrimValue (PVInt 1))) TyInt) (TPrimValue (PVInt 2))) `shouldBe` Right (TPrimValue (PVReal 3))
