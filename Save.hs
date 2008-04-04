module Save where

import System.Directory
import Control.Exception as E hiding (handle)

import File
import Level
import State

savefile = "LambdaHack.save"

restoreGame :: IO (Either (Level, State) String)
restoreGame =
  E.catch (do
             r <- strictDecodeCompressedFile savefile
             removeFile savefile
             case r of
               (x,y,z) -> (z :: Bool) `seq` return $ Left (x,y))
          (\ e -> case e of
                    _ -> return (Right $ "Restore failed: " ++
                                 (unwords . lines) (show e)))

saveGame :: Level -> State -> IO ()
saveGame lvl state = encodeCompressedFile savefile (lvl,state,False)
