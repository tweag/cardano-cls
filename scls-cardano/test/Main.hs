import Cardano.SCLS.CDDL (namespaceSymbolFromText)
import Cardano.SCLS.CDDL.Validate
import Conformance
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Reference
import System.Environment (lookupEnv)
import Test.Hspec

main :: IO ()
main = do
  mReferenceCDDLs <- loadAllReferenceCDDLs

  sampleCount :: Integer <- lookupEnv "CONFORMANCE_SAMPLES" >>= \m -> pure $ maybe 1000 read m

  hspec $ do
    describe "Basic checks" $ do
      it "has no invalid namespaces" $
        invalidSpecs `shouldSatisfy` Map.null

    describe "Conformance test" $ do
      case mReferenceCDDLs of
        Nothing ->
          it "loads reference CDDL specs" $
            expectationFailure $
              "REFERENCE_CDDL_DIR environment variable is not set"
        Just referenceCDDLs ->
          forM_ referenceCDDLs (runConformanceTest sampleCount)
 where
  runConformanceTest sampleCount (nsText, mRef) =
    context (T.unpack nsText) $ it "passes bidirectional conformance tests" $ do
      case mRef of
        Left err ->
          expectationFailure $
            "Failed to load reference CDDL: " ++ show err
        Right refCDDL -> do
          case namespaceSymbolFromText nsText >>= flip Map.lookup validSpecs of
            Nothing ->
              expectationFailure $
                "No valid Huddle CDDL spec found for namespace: " ++ T.unpack nsText
            Just huddleSpec -> do
              mapM_
                (\_ -> propReferenceAcceptsCBOR refCDDL huddleSpec HuddleValidating `shouldReturn` Right ())
                [1 .. sampleCount]

              mapM_
                (\_ -> propReferenceAcceptsCBOR huddleSpec refCDDL ReferenceValidating `shouldReturn` Right ())
                [1 .. sampleCount]
