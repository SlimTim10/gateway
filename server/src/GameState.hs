module GameState
  -- (
  -- )
where

-- import qualified Data.Map.Strict as M
-- import Control.Monad.State -- (State, get, put, evalState)
-- import Data.Time.Clock.POSIX (getPOSIXTime)

-- initialState :: GameState
-- initialState = M.fromList
--   [ (1, PropState (TagReader 0) 1585516415)
--   , (2, PropState (TagReader 0) 1585516415)
--   , (3, PropState (Door Closed) 1585516415)
--   ]

    
    

-- data Prop
--   = TagReader Int
--   | Door DoorPosition
--   deriving (Show, Eq, Generic)

-- instance ToJSON Prop where
--   -- toJSON = A.genericToJSON (A.defaultOptions { A.sumEncoding = A.defaultTaggedObject  { A.tagFieldName = "prop", A.contentsFieldName = "value" } })
--   toJSON = A.genericToJSON (A.defaultOptions { A.sumEncoding = A.ObjectWithSingleField })

-- data Prop = Prop POptions
--   deriving (Show, Eq, Generic, FromJSON)
-- instance ToJSON Prop where
--   toJSON = A.genericToJSON (A.defaultOptions { A.sumEncoding = A.ObjectWithSingleField })

-- data PValue = Tag Int | Door DoorPosition
--   deriving (Show, Eq, Generic, FromJSON)
-- instance ToJSON PValue where
--   toJSON = A.genericToJSON (A.defaultOptions { A.sumEncoding = A.ObjectWithSingleField })
-- data DoorPosition = Open | Closed
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- data POptions
--   = POptions
--   { name :: String
--   , address :: Int
--   }
--   deriving (Show, Eq, Generic, FromJSON)
-- instance ToJSON POptions where
--   toJSON = A.genericToJSON (A.defaultOptions { A.sumEncoding = A.ObjectWithSingleField })

-- data PropConfig
--   = PropConfig String Int Int
--   | PropConfig String Int DoorPosition

-- type Timestamp = Integer
-- data PropState = PropState
--   { prop :: Prop
--   , timestamp :: Timestamp
--   }
--   deriving (Show)

-- type GameState = M.Map Address PropState

-- mockTagRead :: Timestamp -> State GameState String
-- mockTagRead t = do
--   x <- get
--   let v = PropState (TagReader 3) t
--   let y = M.insert 1 v x
--   return . show $ y

-- test :: IO ()
-- test = do
--   print initialState
--   t <- round <$> getPOSIXTime
--   let output = evalState (mockTagRead t) initialState
--   print output

-- mockTagRead2 :: StateT GameState IO String
-- mockTagRead2 = do
--   state <- get
--   t <- lift $ round <$> getPOSIXTime
--   let v = PropState (TagReader 3) t
--   return . show . M.insert 1 v $ state

-- test2 :: StateT GameState IO ()
-- test2 = do
--   state <- get
--   lift $ print state
--   output <- mockTagRead2
--   lift $ print output
--   return ()

-- runTest2 = evalStateT test2 initialState

-- data Config = Config
--   { props :: [Prop]
--   , rules :: [Rule]
--   }

-- data Rule
--   = RuleBasic [Prop] Prop
--   | RuleSequence [Prop] Prop
--   | RuleTimedSequence [Prop] [Int] Prop

-- Config
--   { props =
--     [ 
--     ]
--   , triggers =
--     [ RuleBasic [TagReader1 1] (Door Open)
--     , RuleBasic [TagReader1 3, TagReader2 2] (Door Open)
--     , RuleSequence [TagReader1 1, TagReader2 2] (Door Close)
--     , RuleTimedSequence [TagReader1 2, TagReader2 3] [0, 1] (Door Open)
--     ]
--   }
