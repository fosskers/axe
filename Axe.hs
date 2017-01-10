{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Streaming.Char8 as Q
import           Streaming
import qualified Streaming.Prelude as S
import           Text.Printf

---

type Line = BL.ByteString

file :: FilePath
file = "/home/colin/code/azavea/axe/north-van.osm"

-- | The output path.
out :: Int -> FilePath
out = printf "/home/colin/code/azavea/axe/catalog/out-%08d.osm"

-- | Streams elements from the source file line-by-line.
-- This drops the first three lines, which are not Elements.
xml :: MonadResource m => Stream (Of Line) m ()
xml = S.drop 3 . S.mapped Q.toLazy . Q.lines $ Q.readFile file

-- | Child tags begin with at least two whitespaces.
isChild :: Line -> Bool
isChild = BL.isPrefixOf "  "

-- | Stream a group of 10,000 or so lines, such that each Element's closing tag
-- is present in the Stream. Returns the rest of the Stream.
elements :: Monad m => Stream (Of Line) m r -> Stream (Of Line) m (Stream (Of Line) m r)
elements s = S.splitAt 10000 s >>= S.span isChild >>= lift . S.next >>= either (pure . pure) f
  where f (e,s') | BL.isPrefixOf " </" e = S.yield e >> pure s'
                 | otherwise = pure $ S.cons e s'

-- | Yield a legal @<osm> ... </osm>@ block.
osm :: Monad m => Stream (Of Line) m r -> Stream (Of Line) m (Stream (Of Line) m r)
osm s = (\_ r _ -> r) <$> S.yield "<osm>" <*> elements s <*> S.yield "</osm>"

-- | Write each @<osm>@ block to a separate file.
work :: MonadResource m => Int -> Stream (Of Line) m r -> m ()
work !n s = do
  rest <- Q.writeFile (out n) . Q.unlines $ S.with (osm s) Q.fromLazy
  S.next rest >>= either (\_ -> pure ()) (\(e,s') -> work (n + 1) $ S.cons e s')

main :: IO ()
main = runResourceT (work 0 xml) >> putStrLn "Done."
