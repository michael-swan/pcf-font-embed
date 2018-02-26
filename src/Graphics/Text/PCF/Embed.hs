{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | __pcf-font-embed__ allows users to render and embed text with X11 PCF fonts at compile-time.
-- Perhaps the best use-case for this library is in generating textures for text rendering with
-- accelerated graphics. For reference, here is a simple example of __pcf-font-embed__ in action:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Graphics.Text.PCF
-- > import Graphics.Text.PCF.Embed
-- >
-- > -- | USAGE: program
-- > main :: IO ()
-- > main = putStrLn $ pcf_text_ascii $(embedPCFText "font.pcf.gz" "Hello!")
module Graphics.Text.PCF.Embed (
        -- * Embedding
        embedPCFText,
        embedPCFTextColor
    ) where

import Graphics.Text.PCF
import Language.Haskell.TH
import System.IO.Unsafe
import Foreign.ForeignPtr
import GHC.Exts
import Data.List
import Data.ByteString.Unsafe
import Data.ByteString.Lazy (fromStrict)
import Data.Word
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Lazy as B

-- | Render text at compile time. Default opaque and blank colors of 0x00 and 0xFF are used respectively. The generated expression consists of a `PCFText`.
embedPCFText :: FilePath
             -- ^ Font to render with
             -> String
             -- ^ Text to render
             -> Q Exp
embedPCFText file str = genPCFTextCode =<< (runIO $ do
        pcf <- either fail return =<< loadPCF file
        case renderPCFText pcf str of
            Just ret -> return ret
            Nothing ->
                fail "Failed to render texture atlas.")

-- | Render text at compile time. The generated expression consists of a `PCFText`.
embedPCFTextColor :: FilePath
                  -- ^ Font to render with
                  -> Word8
                  -- ^ Opaque color value
                  -> Word8
                  -- ^ Blank color value
                  -> String
                  -- ^ Text to render
                  -> Q Exp
embedPCFTextColor file opaque blank str = genPCFTextCode =<< (runIO $ do
        pcf <- either fail return =<< loadPCF file
        case renderPCFTextColor pcf opaque blank str of
            Just ret -> return ret
            Nothing ->
                fail "Failed to render texture atlas.")

genPCFTextCode :: PCFText -> Q Exp
genPCFTextCode (PCFText gs w h img) = do
    fp <- newName "fp"
    img_name <- newName "img"
    return $ VarE 'unsafePerformIO
               `AppE` DoE [ BindS (VarP img_name) (VarE 'fmap
                                                    `AppE` LamE [VarP fp]
                                                              (VarE 'VS.unsafeFromForeignPtr0
                                                                 `AppE` VarE fp
                                                                 `AppE` LitE (IntegerL $ fromIntegral $ VS.length img))
                                                    `AppE` (VarE 'newForeignPtr_ `AppE` (ConE 'Ptr `AppE` LitE (StringPrimL $ VS.toList img))))
                          , NoBindS $ AppE (VarE 'return) $ foldl' AppE (ConE 'PCFText) $
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
                                                                                                   , VarE 'fromStrict
                                                                                                         `AppE` (VarE 'unsafePerformIO
                                                                                                               `AppE` (VarE 'unsafePackAddressLen
                                                                                                                     `AppE` LitE (IntegerL $ fromIntegral $ B.length glyph_bitmap)
                                                                                                                           `AppE` LitE (StringPrimL $ B.unpack glyph_bitmap)))]) gs
                                                                , LitE $ IntegerL $ fromIntegral w
                                                                , LitE $ IntegerL $ fromIntegral h
                                                                , VarE img_name ]]
