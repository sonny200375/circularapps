import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R -- for Repa
import Prelude as P
import Graphics.Gloss

main :: IO ()
main = do
  picture <- loadBMP "bl30bmp.bmp"
  display (InWindow "Pic" (200, 200) (10, 10)) white picture
  data ImgFormat = Bmp | Jpg | Png | Tiff
  [ext, path] <- getArgs
  case fromExt ext of
    Nothing -> putStrLn "Sorry, Not Recognized Image format!"
    Just fmt -> convertImg fmt path

convertImg
  :: ImgFormat         -- ^ Format of resulting image
  -> FilePath          -- ^ Where to get source image
  -> IO ()
convertImg fmt path = do
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right img ->
      (case fmt of -- select saving function
        Bmp  -> saveBmpImage
        Jpg  -> saveJpgImage 100
        Png  -> savePngImage
        Tiff -> saveTiffImage)
      (replaceExtension path (toExt fmt)) -- replace file extension
      img -- pass it 'DynamicImage' we've read

-- | Get file extension corresponding to known image format.
toExt :: ImgFormat -> String
toExt Bmp      = "bmp"
toExt Jpg      = "jpeg"
toExt Png      = "png"
toExt Tiff     = "tiff"

-- Get image format corresponding to given extension or 'Nothing' if we
-- don't support that format.

fromExt :: String -> Maybe ImgFormat
fromExt "bmp"  = Just Bmp
fromExt "jpeg" = Just Jpg
fromExt "png"  = Just Png
fromExt "tiff" = Just Tiff
fromExt _      = Nothing

data DynamicImage =
       -- | A greyscale image.
       ImageY8    (Image Pixel8)
       -- | A greyscale image with 16bit components
     | ImageY16   (Image Pixel16)
       -- | A greyscale HDR image
     | ImageYF    (Image PixelF)
       -- | An image in greyscale with an alpha channel.
     | ImageYA8   (Image PixelYA8)
      -- | An image in greyscale with alpha channel on 16 bits.
     | ImageYA16  (Image PixelYA16)
       -- | An image in true color.
     | ImageRGB8  (Image PixelRGB8)
       -- | An image in true color with 16bit depth.
     | ImageRGB16 (Image PixelRGB16)
       -- | An image with HDR pixels
     | ImageRGBF  (Image PixelRGBF)
       -- | An image in true color and an alpha channel.
     | ImageRGBA8 (Image PixelRGBA8)
       -- | A true color image with alpha on 16 bits.
     | ImageRGBA16 (Image PixelRGBA16)
       -- | An image in the colorspace used by Jpeg images.
     | ImageYCbCr8 (Image PixelYCbCr8)
       -- | An image in the colorspace CMYK
     | ImageCMYK8  (Image PixelCMYK8)
       -- | An image in the colorspace CMYK and 16 bits precision
     | ImageCMYK16 (Image PixelCMYK16)