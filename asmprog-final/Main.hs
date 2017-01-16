{-# Language TypeFamilies, GeneralizedNewtypeDeriving #-}
module Main (main, runUntilEnd) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Functor.Identity
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Text.Megaparsec
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import           Debug.Trace

filename12, filename23, filename25 :: FilePath
filename12 = "/Users/emertens/Source/advent2016/inputs/input12.txt"
filename23 = "/Users/emertens/Source/advent2016/inputs/input23.txt"
filename25 = "/Users/emertens/Source/advent2016/inputs/input25.txt"

main :: IO ()
main =
  do program12 <- loadFile basicInstructions filename12
     let compute12 c = fst $ runIdentity $ runMachineT initialRegisters
                     $ C =: c >> runUntilEnd program12 >> reg A
     putStrLn "Solutions for problem 12"
     print (compute12 0)
     print (compute12 1)

     program23 <- loadFile (toggleInstruction:basicInstructions) filename23
     let compute23 a = fmap fst
                     $ runMachineT initialRegisters
                     $ runToggleT (length program23)
                     $ A =: a >> runUntilEnd program23 >> reg A
     putStrLn "Solutions for problem 23"
     print =<< compute23  7
     -- print =<< compute23 12

     program25 <- loadFile (outputInstruction:basicInstructions) filename25
     putStrLn "Solutions for problem 25"
     print $ find (isGoodLoop program25) [1..]


------------------------------------------------------------------------
-- Registers
------------------------------------------------------------------------

data Register = PC | A | B | C | D

data Registers = Registers
  { registerPC, registerA, registerB, registerC, registerD :: !Int }
  deriving (Show, Eq)

initialRegisters :: Registers
initialRegisters = Registers 0 0 0 0 0

------------------------------------------------------------------------
-- Values that can be registers or constants
------------------------------------------------------------------------

data Value = ValueInt Int | ValueRegister Register

value :: MonadRegisters m => Value -> m Int
value (ValueInt      i) = return i
value (ValueRegister r) = reg r

------------------------------------------------------------------------
-- Classes of operations
------------------------------------------------------------------------

class Monad m => MonadRegisters m where
  reg  :: Register -> m Int
  (=:) :: Register -> Int -> m ()

(-=), (+=) :: MonadRegisters m => Register -> Int -> m ()
r -= d = r += (-d)
r += d = do x <- reg r
            r =: x+d

infix 2 =:, +=, -=

class Monad m => MonadMachine m where
  inc :: Register -> m ()
  dec :: Register -> m ()
  cpy :: Value -> Register -> m ()
  jnz :: Value -> Value -> m ()

class Monad m => MonadOutput m where
  out :: Value -> m ()

class Monad m => MonadToggle m where
  tgl :: Value -> m ()


------------------------------------------------------------------------
-- MachineT: monad transformer implementing MonadMachine
------------------------------------------------------------------------

newtype MachineT m a = Machine (StateT Registers m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans)

runMachineT :: Monad m => Registers -> MachineT m a -> m (a, Registers)
runMachineT r (Machine m) = runStateT m r

instance PrimMonad m => PrimMonad (MachineT m) where
  type PrimState (MachineT m) = PrimState m
  primitive = Machine . primitive

instance Monad m => MonadRegisters (MachineT m) where
  reg r =
    do Registers pc a b c d <- Machine get
       return $! case r of PC -> pc; A -> a; B -> b; C -> c; D -> d
  r =: x = Machine $
       modify' $ \rs -> case r of
         PC -> rs { registerPC = x }
         A  -> rs { registerA  = x }
         B  -> rs { registerB  = x }
         C  -> rs { registerC  = x }
         D  -> rs { registerD  = x }
  {-# INLINE (=:) #-}
  {-# INLINE reg #-}

instance Monad m => MonadMachine (MachineT m) where
  inc r   = r += 1             >> PC += 1
  dec r   = r -= 1             >> PC += 1
  cpy i o = value i >>= (o =:) >> PC += 1
  jnz cond offset =
    do x <- value cond
       o <- if x == 0 then return 1 else value offset
       PC += o
  {-# INLINE inc #-}
  {-# INLINE dec #-}
  {-# INLINE cpy #-}
  {-# INLINE jnz #-}


------------------------------------------------------------------------
-- OutputT: monad transformer implementing MonadOutput
------------------------------------------------------------------------

newtype OutputT m a = Output (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans, Alternative, MonadPlus)

runOutputT :: Int -> OutputT m a -> m (a, Int)
runOutputT x (Output m) = runStateT m x

-- | Pass registers through to underlying machine
instance MonadRegisters m => MonadRegisters (OutputT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)

-- | Pass basic instructions through to underlying machine
instance MonadMachine m => MonadMachine (OutputT m) where
  inc x   = lift (inc x)
  dec x   = lift (dec x)
  jnz x y = lift (jnz x y)
  cpy x y = lift (cpy x y)

-- | Output values checking for correct alternation of zero and one
instance (MonadRegisters m, MonadPlus m) => MonadOutput (OutputT m) where
  out v = value v >>= outputPrim >> PC += 1

outputPrim :: MonadPlus m => Int -> OutputT m ()
outputPrim x = Output $
  do n <- get
     put (n+1)
     guard (x == if even n then 0 else 1)

------------------------------------------------------------------------
-- ToggleT: monad transformer implementing MonadToggle
------------------------------------------------------------------------

newtype ToggleT m a = Toggle (ReaderT (MV.MVector (PrimState m) Bool) m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans ToggleT where lift = Toggle . lift

instance PrimMonad m => PrimMonad (ToggleT m) where
  type PrimState (ToggleT m) = PrimState m
  primitive = lift . primitive

runToggleT :: PrimMonad m => Int -> ToggleT m a -> m a
runToggleT n (Toggle m) = runReaderT m =<< MV.replicate n False

toggleVec :: Monad m => ToggleT m (MV.MVector (PrimState m) Bool)
toggleVec = Toggle ask

needReg :: MonadRegisters m => (Register -> m ()) -> Value -> m ()
needReg f (ValueRegister r) = f r
needReg _ _                 = PC += 1

instance MonadRegisters m => MonadRegisters (ToggleT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)
  {-# INLINE (=:) #-}
  {-# INLINE reg #-}

instance (PrimMonad m, MonadRegisters m, MonadMachine m) => MonadMachine (ToggleT m) where
  inc x   = toggler' (inc x)   (dec x)
  dec x   = toggler' (dec x)   (inc x)
  jnz x y = toggler' (jnz x y) (cpy x `needReg` y)
  cpy x y = toggler' (cpy x y) (jnz x (ValueRegister y))
  {-# INLINE inc #-}
  {-# INLINE dec #-}
  {-# INLINE cpy #-}
  {-# INLINE jnz #-}

instance (PrimMonad m, MonadRegisters m, MonadMachine m) => MonadToggle (ToggleT m) where
  tgl x = toggler (tglPrim x) (inc `needReg` x)

tglPrim :: (PrimMonad m, MonadRegisters m) => Value -> ToggleT m ()
tglPrim x =
  do v      <- toggleVec
     offset <- value x
     pc     <- reg PC
     PC += 1
     let pc' = pc+offset
     when (0 <= pc' && pc' < MV.length v) (MV.modify v not pc')

toggler' :: (PrimMonad m, MonadRegisters m) => m a -> m a -> ToggleT m a
toggler' x y = toggler (lift x) y

toggler :: (PrimMonad m, MonadRegisters m) => ToggleT m a -> m a -> ToggleT m a
toggler normal toggled =
  do v <- toggleVec
     x <- MV.read v =<< reg PC
     if x then lift toggled else normal


------------------------------------------------------------------------
-- Functions for executing a whole program
------------------------------------------------------------------------

-- | Big-step machine semantics. Runs machine until it halts.
runUntilEnd :: MonadRegisters m => Vector (m a) -> m ()
runUntilEnd program = go
  where
    go =
      do pc <- reg PC
         case program Vector.!? pc of
           Nothing -> return ()
           Just m  -> m >> go


-- | Predicate for machines that loop while producing a good output stream.
isGoodLoop :: Vector (OutputT (MachineT Maybe) ()) -> Int -> Bool
isGoodLoop program start = isWorking (go s0 <$> step s0)
  where
    -- initial machine state
    s0 = (0, initialRegisters { registerA = start })

    -- small-step machine semantics
    step (outs, regs) =
      do op                 <- program Vector.!? registerPC regs
         (((),outs'),regs') <- runMachineT regs $ runOutputT outs op
         return (outs', regs')

    -- machine produced a postive, even number of outputs
    isProductive (out1, _) (out2, _) =  out1 /= out2 && even (out1 - out2)

    -- machine is at same PC with same registers
    isLooping (_, regs1) (_, regs2) = regs1 == regs2

    -- Things are working if they return Just True
    isWorking = fromMaybe False

    -- advance the slow machine once and fast twice. check if the machine looped
    go slow fast
      | isLooping slow fast = isProductive slow fast
      | otherwise           = isWorking
                            $ liftA2 go (step slow) (step =<< step fast)

------------------------------------------------------------------------
-- Parser logic
------------------------------------------------------------------------

-- | Parse the given file using the given set of instructions.
loadFile :: [(String, Parser a)] -> FilePath -> IO (Vector a)
loadFile parsers filename =
  do txt <- readFile filename
     case parse (parseLines parsers) filename txt of
       Left e  -> throwIO e
       Right p -> return $! Vector.fromList p

-- | Parse the whole input as lines of instructions.
parseLines :: [(String, Parser a)] -> Parser [a]
parseLines parsers = many (parseLine parsers) <* eof

-- | Parse a single file line using the given parsers.
parseLine :: [(String,Parser a)] -> Parser a
parseLine xs =
  do name <- pInstructionName
     case lookup name xs of
       Just p  -> p <* eol
       Nothing -> fail "unknown instruction"

-- | Wrap a parser to handle trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (char ' '))

-- | Returns the next instruction name
pInstructionName :: Parser String
pInstructionName = lexeme (some letterChar)

-- | Parse a single register
pRegister :: Parser Register
pRegister =
  choice [ r <$ lexeme (char c) | (r,c) <- [(A,'a'),(B,'b'),(C,'c'),(D,'d')]]

-- | Parse a register or a integer constant
pValue :: Parser Value
pValue = ValueRegister          <$> pRegister
     <|> ValueInt . fromInteger <$> lexeme (L.signed (pure ()) L.decimal)

------------------------------------------------------------------------
-- Individual instruction parsers
------------------------------------------------------------------------

-- | Basic machine instructions common to all programs
basicInstructions :: MonadMachine m => [(String, Parser (m ()))]
basicInstructions =
  [ ("inc", liftA  inc pRegister)
  , ("dec", liftA  dec pRegister)
  , ("jnz", liftA2 jnz pValue pValue)
  , ("cpy", liftA2 cpy pValue pRegister) ]
{-# INLINE basicInstructions #-}

toggleInstruction :: MonadToggle m => (String, Parser (m ()))
toggleInstruction = ("tgl", liftA tgl pValue)
{-# INLINE toggleInstruction #-}

-- | Output machine instruction specific to proram 25
outputInstruction :: (MonadOutput m, MonadMachine m) => (String, Parser (m ()))
outputInstruction = ("out", liftA out pValue)
{-# INLINE outputInstruction #-}

------------------------------------------------------------------------
-- Render
------------------------------------------------------------------------

newtype Render a = Render (Writer (Endo String) a)
  deriving (Functor, Applicative, Monad)

execRender :: Render a -> String
execRender (Render m) = appEndo (execWriter m) ""

renderS :: ShowS -> Render ()
renderS f = Render (tell (Endo f))

class Renderable a where
  render     :: a -> Render ()
  renderList :: [a] -> Render ()
  renderList = mapM_ render

instance Renderable Char where
  render     = renderS . showChar
  renderList = renderS . showString

instance Renderable a => Renderable [a] where
  render = renderList

instance Renderable Value where
  render (ValueRegister r) = render r
  render (ValueInt      i) = render i

instance Renderable Int where
  render = renderS . shows

instance Renderable Register where
  render r = render (case r of PC -> "pc"; A -> "a"; B -> "b"; C -> "c"; D -> "d")


instance MonadMachine Render where
  inc x   = do render "inc "; render x; render '\n'
  dec x   = do render "dec "; render x; render '\n'
  cpy x y = do render "cpy "; render x;
               render ' '   ; render y; render '\n'
  jnz x y = do render "jnz "; render x;
               render ' '   ; render y; render '\n'

instance MonadToggle Render where
  tgl x   = do render "tgl "; render x; render '\n'

instance MonadOutput Render where
  out x   = do render "out "; render x; render '\n'
