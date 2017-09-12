{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Text.PCF.Embed (embedPCFText) where

import Graphics.Text.PCF
import Language.Haskell.TH
import System.IO.Unsafe
import Foreign.ForeignPtr
import GHC.Exts
import Data.Vector.Storable as VS

embedPCFText :: FilePath -> String -> Q Exp
embedPCFText file str = do
    (w, h, img) <- (runIO $ do
        pcf <- either fail return =<< loadPCF file
        case renderPCFText pcf str of
            Just ret -> return ret
            Nothing ->
                fail "Failed to render texture atlas.")
    fp <- newName "fp"
    return $ TupE [ LitE $ IntegerL $ fromIntegral w
                  , LitE $ IntegerL $ fromIntegral h
                  , VarE 'unsafePerformIO
                      `AppE` (VarE 'fmap
                                  `AppE` LamE [VarP fp]
                                            (VarE 'unsafeFromForeignPtr
                                               `AppE` VarE fp
                                               `AppE` LitE (IntegerL 0)
                                               `AppE` LitE (IntegerL $ fromIntegral $ VS.length img))
                                  `AppE` (VarE 'newForeignPtr_ `AppE` (ConE 'Ptr `AppE` LitE (StringPrimL $ VS.toList img))))
                                    ]
