{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Streaming.Char8 as Q
import           Options.Generic
import           Streaming
import qualified Streaming.Prelude as S
import           System.FilePath.Posix ((</>))
import           Text.Printf

---

type Line = BL.ByteString
type RIO = ResourceT IO

-- | The runtime environment.
data Env = Env { input :: FilePath, catalog :: FilePath } deriving (Generic, ParseRecord)

-- | The output path.
out :: FilePath -> Int -> FilePath
out fp = printf (fp </> "out-%08d.osm")

-- | Streams elements from the source file line-by-line.
-- This drops the first three lines, which are not Elements.
xml :: FilePath -> Stream (Of Line) RIO ()
xml = S.drop 3 . S.mapped Q.toLazy . Q.lines . Q.readFile

-- | Child tags begin with at least two whitespaces.
isChild :: Line -> Bool
isChild = BL.isPrefixOf "  "
{-# INLINE isChild #-}

-- | Stream a group of 10,000 or so lines, such that each Element's closing tag
-- is present in the Stream. Returns the rest of the Stream.
elements :: Stream (Of Line) RIO r -> Stream (Of Line) RIO (Stream (Of Line) RIO r)
elements s = do
  next <- S.splitAt 10000 s >>= S.span isChild >>= lift . S.next
  case next of
    Right (e,s') | BL.isPrefixOf " </" e -> S.yield e >> pure s'
                 | otherwise -> pure $ S.cons e s'
    Left r -> pure $ pure r

-- | Yield a legal @<osm> ... </osm>@ block.
osm :: Stream (Of Line) RIO r -> Stream (Of Line) RIO (Stream (Of Line) RIO r)
osm s = (\_ r _ -> r) <$> S.yield "<osm>" <*> elements s <*> S.yield "</osm>"

-- | Write each @<osm>@ block to a separate file.
work :: FilePath -> Int -> Stream (Of Line) RIO r -> RIO ()
work c !n s = do
  next <- (Q.writeFile (out c n) . Q.unlines $ S.with (osm s) Q.fromLazy) >>= S.next
  case next of
    Right (e,s') -> work c (n + 1) $ S.cons e s'
    Left _ -> pure ()

main :: IO ()
main = do
  Env i c <- getRecord "Axe - Cut arbitrarily large OSM XML files."
  runResourceT (work c 0 $ xml i) >> putStrLn "Done."
