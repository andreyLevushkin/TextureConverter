module Main where

import System.Environment   
import qualified Data.ByteString as BS
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.Vector.Storable as V
import Data.Word

main = do
    args <- getArgs 
    case args of
        from:to:[] -> convertPNG from to
        from:[]    -> convertPNG from (from ++ ".tex")
        _          -> return ()

loadPng :: String -> IO (Either String DynamicImage)
loadPng path = do 
    dat <- BS.readFile path 
    return $ decodePng dat

convertPNG :: String -> String -> IO ()  
convertPNG from to = do
    png <- loadPng from
    case png of
        (Right (ImageRGBA8 image)) -> writeOutput to image
        _ -> putStrLn "Unsupported image"


writeOutput :: String -> Image PixelRGBA8 -> IO ()
writeOutput file image =  let pixelData = concat [ pixelToWords (pixelAt image x y) | y <- [0..imageHeight image - 1], x <- [0..imageWidth image - 1]]
                            in BS.writeFile file (BS.pack pixelData)


pixelToWords :: PixelRGBA8 -> [Word8]
pixelToWords (PixelRGBA8 r g b a) = [r,g,b,a]