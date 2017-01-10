{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Streaming.Char8 as Q
import           Options.Generic
import           Streaming
import qualified Streaming.Prelude as S
import           Text.Printf

---

type Line = BL.ByteString

-- | The runtime environment.
data Env = Env { input :: FilePath } deriving (Generic, ParseRecord)

-- | The output path.
out :: Int -> FilePath
out = printf "/home/colin/code/azavea/axe/catalog/out-%08d.osm"

-- | Streams elements from the source file line-by-line.
-- This drops the first three lines, which are not Elements.
xml :: MonadResource m => FilePath -> Stream (Of Line) m ()
xml = S.drop 3 . S.mapped Q.toLazy . Q.lines . Q.readFile

-- | Child tags begin with at least two whitespaces.
isChild :: Line -> Bool
isChild = BL.isPrefixOf "  "
{-# INLINE isChild #-}

-- | Stream a group of 10,000 or so lines, such that each Element's closing tag
-- is present in the Stream. Returns the rest of the Stream.
elements :: Monad m => Stream (Of Line) m r -> Stream (Of Line) m (Stream (Of Line) m r)
elements s = do
  next <- S.splitAt 10000 s >>= S.span isChild >>= lift . S.next
  case next of
    Right (e,s') | BL.isPrefixOf " </" e -> S.yield e >> pure s'
                 | otherwise -> pure $ S.cons e s'
    Left r -> pure $ pure r

-- | Yield a legal @<osm> ... </osm>@ block.
osm :: Monad m => Stream (Of Line) m r -> Stream (Of Line) m (Stream (Of Line) m r)
osm s = (\_ r _ -> r) <$> S.yield "<osm>" <*> elements s <*> S.yield "</osm>"

-- | Write each @<osm>@ block to a separate file.
work :: MonadResource m => Int -> Stream (Of Line) m r -> m ()
work !n s = do
  next <- (Q.writeFile (out n) . Q.unlines $ S.with (osm s) Q.fromLazy) >>= S.next
  case next of
    Right (e,s') -> work (n + 1) $ S.cons e s'
    Left _ -> pure ()

main :: IO ()
main = do
  Env i <- getRecord "Axe - Cut arbitrarily large OSM XML files."
  runResourceT (work 0 $ xml i) >> putStrLn "Done."
