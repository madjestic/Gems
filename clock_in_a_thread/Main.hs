module Main where

import Control.Monad (when)
import System.IO

import Reactive.Banana
import Reactive.Banana.Frameworks


main :: IO ()
main = do    
    sources <- (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler
    network <- setupNetwork sources
    actuate network
    eventLoop sources network

-- Read commands and fire corresponding events 
eventLoop :: (EventSource (), EventSource (),EventSource EventNetwork) -> EventNetwork -> IO ()
eventLoop (eplus, eminus, espause) network = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        hSetBuffering stdin NoBuffering


{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
-- Set up the program logic in terms of events and behaviors.
setupNetwork :: (EventSource (), EventSource (), EventSource EventNetwork) -> IO EventNetwork
setupNetwork (eplus, eminus, espause) = compile $ do
    counterUp   <- fromAddHandler (addHandler eplus)
    counterDown <- fromAddHandler (addHandler eminus)
    epause      <- fromAddHandler (addHandler espause)

    let ecount = accumE 0 $ ((+1) <$ counterUp) `union` (subtract 1 <$ counterDown)

    reactimate $ fmap print ecount
    reactimate $ fmap pause epause
