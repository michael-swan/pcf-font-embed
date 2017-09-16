{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | __pcf-font-embed__ allows users to generate and embed text rendered using X11 PCF fonts at compile-time.
module Graphics.Text.PCF.Embed (
        -- * Embedding
        embedPCFText
    ) where

import Graphics.Text.PCF
import Language.Haskell.TH
import System.IO.Unsafe
import Foreign.ForeignPtr
import GHC.Exts
import Data.List
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Lazy as B

-- | Render text at compile time. The generated expression's type is PCFText(..).
embedPCFText :: FilePath -> String -> Q Exp
embedPCFText file str = do
    PCFText gs w h img <- (runIO $ do
        pcf <- either fail return =<< loadPCF file
        case renderPCFText pcf str of
            Just ret -> return ret
            Nothing ->
                fail "Failed to render texture atlas.")
    fp <- newName "fp"
    return $ foldl' AppE (ConE 'PCFText) $
                  [ ListE $ map (\PCFGlyph{..} ->
                        foldl' AppE (ConE 'PCFGlyph) [ foldl' AppE (ConE 'Metrics) [ LitE $ IntegerL $ fromIntegral $ metrics_left_sided_bearings glyph_metrics
                                                                                   , LitE $ IntegerL $ fromIntegral $ metrics_right_sided_bearings glyph_metrics
                                                                                   , LitE $ IntegerL $ fromIntegral $ metrics_character_width glyph_metrics
                                                                                   , LitE $ IntegerL $ fromIntegral $ metrics_character_ascent glyph_metrics
                                                                                   , LitE $ IntegerL $ fromIntegral $ metrics_character_descent glyph_metrics
                                                                                   , LitE $ IntegerL $ fromIntegral $ metrics_character_attributes glyph_metrics ]
                                                     , LitE $ CharL glyph_char
                                                     , LitE $ IntegerL $ fromIntegral glyph_width
                                                     , LitE $ IntegerL $ fromIntegral glyph_height
                                                     , LitE $ IntegerL $ fromIntegral glyph_pitch
                                                     , LitE $ StringPrimL $ B.unpack glyph_bitmap ]) gs
                  , LitE $ IntegerL $ fromIntegral w
                  , LitE $ IntegerL $ fromIntegral h
                  , VarE 'unsafePerformIO
                      `AppE` (VarE 'fmap
                                  `AppE` LamE [VarP fp]
                                            (VarE 'VS.unsafeFromForeignPtr
                                               `AppE` VarE fp
                                               `AppE` LitE (IntegerL 0)
                                               `AppE` LitE (IntegerL $ fromIntegral $ VS.length img))
                                  `AppE` (VarE 'newForeignPtr_ `AppE` (ConE 'Ptr `AppE` LitE (StringPrimL $ VS.toList img))))]
