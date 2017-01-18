{-# Language TypeFamilies               #-} -- for PrimMonad instance
{-# Language GeneralizedNewtypeDeriving #-}

module Main (main) where

import           Control.Applicative
import           Control.Exception (throwIO)
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Text.Megaparsec
import           Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

filename12, filename23, filename25 :: FilePath
filename12 = "../inputs/input12.txt"
filename23 = "../inputs/input23.txt"
filename25 = "../inputs/input25.txt"

main :: IO ()
main =
  do putStrLn "Problem 12"
     program12 <- loadFile basicInstructions filename12
     let compute12 c = evalMachineT
                     $ do C =: c; runUntilEnd (eval <$> program12); reg A
     print =<< compute12 0
     print =<< compute12 1

     putStrLn "Problem 23"
     program23 <- loadFile (toggleInstruction:basicInstructions) filename23
     let compute23 a = evalMachineT $ runToggleT (length program23)
                     $ do A =: a; runUntilEnd (eval . toggleEval <$> program23); reg A
     print =<< compute23  7
     -- print =<< compute23 12 -- takes about 2 minutes on my computer

     putStrLn "Problem 25"
     program25 <- loadFile (outputInstruction:basicInstructions) filename25
     print $ find (isGoodLoop (eval <$> program25)) [1..]

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

-- | Instructions common to problems 12, 23, and 25
class BasicOps a where
  inc :: Register          -> a
  dec :: Register          -> a
  cpy :: Value -> Register -> a
  jnz :: Value -> Value    -> a

-- | Instructions unique to problem 23
class OutputOp a where
  out :: Value -> a

-- | Instructions unique to problem 25
class ToggleOp a where
  tgl :: Value -> a


------------------------------------------------------------------------
-- Implementation of basic instructions in terms of a register machine
------------------------------------------------------------------------

newtype Eval m = Eval { eval :: m () }

instance MonadRegisters m => BasicOps (Eval m) where
  inc r   = Eval $ do r += 1;               PC += 1
  dec r   = Eval $ do r -= 1;               PC += 1
  cpy i o = Eval $ do v <- value i; o =: v; PC += 1
  jnz cond offset = Eval $
    do x <- value cond
       o <- if x == 0 then return 1 else value offset
       PC += o

class Monad m => MonadRegisters m where
  reg  :: Register -> m Int
  (=:) :: Register -> Int -> m ()

(+=), (-=) :: MonadRegisters m => Register -> Int -> m ()
r += d = do x <- reg r; r =: x+d
r -= d = r += negate d
infix 2 =:, +=, -=

------------------------------------------------------------------------
-- Implementation of out instruction
------------------------------------------------------------------------

class Monad m => MonadOutput m where
  recordOutput :: Int -> m ()

instance (MonadRegisters m, MonadOutput m) => OutputOp (Eval m) where
  out o = Eval $ do recordOutput =<< value o; PC += 1

------------------------------------------------------------------------
-- Implementation of tgl instruction and custom eval type
------------------------------------------------------------------------

newtype ToggleEval m = ToggleEval { toggleEval :: Eval m }

instance (MonadRegisters m, MonadToggleFlag m) => BasicOps (ToggleEval m) where
  inc x   = toggler' (inc x)   (dec x)
  dec x   = toggler' (dec x)   (inc x)
  jnz x y = toggler' (jnz x y) (cpy x `needReg` y)
  cpy x y = toggler' (cpy x y) (jnz x (ValueRegister y))

instance (MonadRegisters m, MonadToggleFlag m) => ToggleOp (ToggleEval m) where
  tgl x = toggler (tglPrim x) (inc `needReg` x)

class Monad m => MonadToggleFlag m where
  isToggled :: Int -> m Bool
  togglePC  :: Int -> m ()

nop :: MonadRegisters m => Eval m
nop = Eval (PC += 1)

needReg :: MonadRegisters m => (Register -> Eval m) -> Value -> Eval m
needReg f (ValueRegister r) = f r
needReg _ _                 = nop

toggler :: (MonadRegisters m, MonadToggleFlag m) => ToggleEval m -> Eval m -> ToggleEval m
toggler (ToggleEval (Eval normal)) (Eval toggled) = ToggleEval $ Eval $
  do x <- isToggled =<< reg PC
     if x then toggled else normal

toggler' :: (MonadRegisters m, MonadToggleFlag m) => Eval m -> Eval m -> ToggleEval m
toggler' x y = toggler (ToggleEval x) y

tglPrim :: (MonadRegisters m, MonadToggleFlag m) => Value -> ToggleEval m
tglPrim x = ToggleEval $ Eval $
  do offset <- value x
     pc     <- reg PC
     togglePC (pc+offset)
     PC =: pc+1

------------------------------------------------------------------------
-- MachineT: monad transformer implementing MonadRegisters
------------------------------------------------------------------------

newtype MachineT m a = Machine (StateT Registers m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans)

runMachineT :: Registers -> MachineT m a -> m (a, Registers)
runMachineT r (Machine m) = runStateT m r

evalMachineT :: Monad m => MachineT m a -> m a
evalMachineT m = fst <$> runMachineT initialRegisters m

instance PrimMonad m => PrimMonad (MachineT m) where
  type PrimState (MachineT m) = PrimState m
  primitive = lift . primitive

instance Monad m => MonadRegisters (MachineT m) where
  reg r = Machine $
       gets $ case r of
         PC -> registerPC
         A  -> registerA
         B  -> registerB
         C  -> registerC
         D  -> registerD
  r =: x = Machine $
       modify' $ \rs -> case r of
         PC -> rs { registerPC = x }
         A  -> rs { registerA  = x }
         B  -> rs { registerB  = x }
         C  -> rs { registerC  = x }
         D  -> rs { registerD  = x }
  {-# INLINE (=:) #-}
  {-# INLINE reg  #-}


------------------------------------------------------------------------
-- OutputT: monad transformer implementing MonadOutput verifying 0,1 pattern
------------------------------------------------------------------------

newtype OutputT m a = Output (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runOutputT :: Int -> OutputT m a -> m (a, Int)
runOutputT x (Output m) = runStateT m x

-- | Pass registers through to underlying machine
instance MonadRegisters m => MonadRegisters (OutputT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)

instance MonadPlus m => MonadOutput (OutputT m) where
  recordOutput x = Output $
    do n <- state $ \n -> let n' = n+1 in n' `seq` (n,n')
       guard (x == if even n then 0 else 1)

------------------------------------------------------------------------
-- ToggleT: monad transformer implementing MonadToggleFlag
------------------------------------------------------------------------

newtype ToggleT m a = ToggleT (ReaderT (MV.MVector (PrimState m) Bool) m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans ToggleT where
  lift = ToggleT . lift

runToggleT :: PrimMonad m => Int {- program size -} -> ToggleT m a -> m a
runToggleT n (ToggleT m) = runReaderT m =<< MV.replicate n False

instance PrimMonad m => MonadToggleFlag (ToggleT m) where
  isToggled pc = ToggleT $ ReaderT $ \v ->
    if pc >= 0 && pc < MV.length v then MV.read v pc else return False
  togglePC pc = ToggleT $ ReaderT $ \v ->
    when (pc >= 0 && pc < MV.length v) (MV.modify v not pc)

-- | Pass registers through to underlying monad
instance MonadRegisters m => MonadRegisters (ToggleT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)

------------------------------------------------------------------------
-- Functions for executing a whole program
------------------------------------------------------------------------

-- | Big-step machine semantics. Runs machine until it halts.
runUntilEnd :: MonadRegisters m => Vector (m ()) -> m ()
runUntilEnd program = go where
  go = do pc <- reg PC
          case program Vector.!? pc of
            Nothing -> return ()
            Just m  -> do m; go

-- | Predicate for machines that loop while producing a good output stream.
isGoodLoop :: Vector (OutputT (MachineT Maybe) ()) -> Int -> Bool
isGoodLoop program start = isWorking (go s0 <$> step s0)
  where
    -- initial machine state
    s0 = (0, initialRegisters { registerA = start })

    -- small-step machine semantics
    step (outs, regs) =
      do op                <- program Vector.!? registerPC regs
         ((_,outs'),regs') <- runMachineT regs (runOutputT outs op)
         return (outs', regs')

    -- machine produced a postive, even number of outputs
    isProductive (out1, _) (out2, _) = out1 /= out2 && even (out1 - out2)

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
  do txt <- Text.readFile filename
     case parse (parseLines parsers) filename txt of
       Left e  -> throwIO e
       Right p -> return $! Vector.fromList p

-- | Parse the whole input as lines of instructions.
parseLines :: [(String, Parser a)] -> Parser [a]
parseLines parsers = sepEndBy (dispatch parsers) eol <* eof

-- | Select a parser by selecting via the next alphabetic lexeme
dispatch :: [(String,Parser a)] -> Parser a
dispatch xs =
  do name <- lexeme (some letterChar)
     case lookup name xs of
       Just p  -> p
       Nothing -> fail ("unexpected '" ++ name ++ "'")

-- | Wrap a parser to handle trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (char ' '))

-- | Parse a single register
pRegister :: Parser Register
pRegister = dispatch [("a",pure A),("b",pure B),("c",pure C),("d",pure D)]

-- | Parse a register or a integer constant
pValue :: Parser Value
pValue = ValueRegister          <$> pRegister
     <|> ValueInt . fromInteger <$> lexeme (L.signed (pure ()) L.decimal)

------------------------------------------------------------------------
-- Individual instruction parsers
------------------------------------------------------------------------

-- | Basic machine instructions common to all programs
basicInstructions :: BasicOps a => [(String, Parser a)]
basicInstructions =
  [ ("inc", liftA  inc pRegister)
  , ("dec", liftA  dec pRegister)
  , ("jnz", liftA2 jnz pValue pValue)
  , ("cpy", liftA2 cpy pValue pRegister) ]
{-# INLINE basicInstructions #-}

toggleInstruction :: ToggleOp a => (String, Parser a)
toggleInstruction = ("tgl", liftA tgl pValue)
{-# INLINE toggleInstruction #-}

-- | Output machine instruction specific to proram 25
outputInstruction :: OutputOp a => (String, Parser a)
outputInstruction = ("out", liftA out pValue)
{-# INLINE outputInstruction #-}

------------------------------------------------------------------------
-- Render
------------------------------------------------------------------------

newtype Render = Render (Endo String)
  deriving (Monoid, Semigroup)

renderToString :: Render -> String
renderToString (Render m) = m `appEndo` ""

fromShowS :: ShowS -> Render
fromShowS = Render . Endo

class Renderable a where
  render     :: a -> Render
  renderList :: [a] -> Render
  renderList = foldMap render

instance Renderable Char where
  render     = fromShowS . showChar
  renderList = fromShowS . showString

instance Renderable a => Renderable [a] where
  render = renderList

instance Renderable a => Renderable (Vector a) where
  render = foldMap render

instance Renderable Value where
  render (ValueRegister r) = render r
  render (ValueInt      i) = render i

instance Renderable Int where
  render = fromShowS . shows

instance Renderable Register where
  render r = render $ case r of PC -> "pc"; A -> "a"; B -> "b"; C -> "c"; D -> "d"

-- Implementations of the instructions ---------------------------------

instance BasicOps Render where
  inc x   = render "inc " <> render x <> render '\n'
  dec x   = render "dec " <> render x <> render '\n'
  cpy x y = render "cpy " <> render x <>
            render ' '    <> render y <> render '\n'
  jnz x y = render "jnz " <> render x <>
            render ' '    <> render y <> render '\n'

instance ToggleOp Render where
  tgl x   = render "tgl " <> render x <> render '\n'

instance OutputOp Render where
  out x   = render "out " <> render x <> render '\n'
