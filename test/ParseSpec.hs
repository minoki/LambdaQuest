module ParseSpec where
import Test.Hspec
import LambdaQuest.SystemF

spec :: Spec
spec = describe "SystemF.Parse" $ do
  it "can parse a function type" $ do
    parseType "" "Int -> Bool -> Real" `shouldBe` Right (TyArr TyInt (TyArr TyBool TyReal))
  it "can parse a universal type" $ do
    parseType "" "forall a. a -> a -> a" `shouldBe` Right (TyAll "a" (TyArr (TyRef 0 "a") (TyArr (TyRef 0 "a") (TyRef 0 "a"))))

  it "can parse a function" $ do
    parseTerm "" "\\x: Int. \\y:Bool . 0.0" `shouldBe` Right (TAbs "x" TyInt $ TAbs "y" TyBool $ TPrimValue (PVReal 0.0))
  it "can parse a polymorhic function" $ do
    parseTerm "" "?a. \\x: a. \\y: a . x " `shouldBe` Right (TTyAbs "a" $ TAbs "x" (TyRef 0 "a") $ TAbs "y" (TyRef 0 "a") $ TRef 1 "x")
