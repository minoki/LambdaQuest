module SystemFsub.ParseSpec where
import Test.Hspec
import LambdaQuest.SystemFsub

spec :: Spec
spec = describe "SystemFsub.Parse" $ do
  it "can parse a function type" $ do
    parseType "" "Int -> Bool -> Real" `shouldBe` Right (TyArr TyInt (TyArr TyBool TyReal))
  it "can parse a universal type" $ do
    parseType "" "forall a. a -> a -> a" `shouldBe` Right (TyAll "a" TyTop (TyArr (TyRef 0 "a") (TyArr (TyRef 1 "a") (TyRef 2 "a"))))

  it "can parse a function" $ do
    parseTerm "" "\\x: Int. \\y:Bool . 0.0" `shouldBe` Right (TAbs "x" TyInt $ TAbs "y" TyBool $ TPrimValue (PVReal 0.0))
  it "can parse a polymorhic function" $ do
    parseTerm "" "?a. \\x: a. \\y: a . x " `shouldBe` Right (TTyAbs "a" TyTop $ TAbs "x" (TyRef 0 "a") $ TAbs "y" (TyRef 1 "a") $ TRef 1 "x")
