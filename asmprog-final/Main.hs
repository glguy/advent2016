{-# Language TypeFamilies               #-} -- for PrimMonad
{-# Language GeneralizedNewtypeDeriving #-}

module Main (main) where

import           Control.Applicative        (Alternative((<|>), some, many), liftA, liftA2)
import           Control.Exception          (throwIO)
import           Control.Monad              (MonadPlus, when, guard)
import           Control.Monad.Primitive    (PrimMonad(primitive, PrimState))
import           Control.Monad.Trans.State  (StateT(runStateT), gets, modify', state)
import           Control.Monad.Trans.Reader (ReaderT(ReaderT,runReaderT))
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import           Control.Monad.Trans.Class  (MonadTrans(lift))
import           Data.Foldable              (find, fold)
import           Data.Functor.Identity      (Identity,runIdentity)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             (Semigroup, Endo(Endo,appEndo), (<>))
import           Data.Text                   (Text)
import qualified Data.Text.IO                as Text
import qualified Data.Vector                 as V
import           Data.Vector                (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Vector.Unboxed.Mutable (Unbox, MVector)
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Char        as P
import qualified Text.Megaparsec.Char.Lexer  as P
import           Control.Monad.ST           (stToIO)
import Debug.Trace

type Parser = P.Parsec (P.ErrorItem Char) Text

filename12, filename23, filename25 :: FilePath
filename12 = "../inputs/input12.txt"
filename23 = "../inputs/input23.txt"
filename25 = "../inputs/input25.txt"

main :: IO ()
main = do run25

run12 :: IO ()
run12 =
  do putStrLn "Problem 12"
     program12 <- loadFile basicInstructions filename12
     let compute12 c = runIdentity $ evalMachineT
                     $ do C =: c; runUntilEnd (eval <$> program12); reg A
     print (compute12 0)
     print (compute12 1)

run23 :: IO ()
run23 =
  do putStrLn "Problem 23"
     program23 <- loadFile (toggleInstruction:basicInstructions) filename23
     let compute23 a = stToIO $ evalMachineT $ runToggleT (length program23)
                     $ do A =: a; runUntilEnd (toggleEval <$> program23); reg A
     print =<< compute23  7
     print =<< compute23 12 -- takes under 2 minutes on my computer

run25 :: IO ()
run25 =
  do putStrLn "Problem 25"
     (pgm1, pgm2) <- V.unzip <$> loadFile (outputInstruction:basicInstructions) filename25
     putStrLn "First 50 outputs when A initialized to 1"
     print $ take 50 $ runIdentity $ evalMachineT $ execOutputListT
           $ do A =: 1; runUntilEnd (eval <$> pgm1)
     print $ find (isGoodLoop (eval <$> pgm2)) [1..]

------------------------------------------------------------------------
-- Instructions
------------------------------------------------------------------------

data Register = PC | A | B | C | D

data Expr = ExprInt !Int | ExprRegister !Register

-- | Instructions common to problems 12, 23, and 25
class BasicOps a where
  inc :: Register         -> a
  dec :: Register         -> a
  cpy :: Expr -> Register -> a
  jnz :: Expr -> Expr     -> a

-- | Instructions unique to problem 23
class OutputOp a where
  out :: Expr -> a

-- | Instructions unique to problem 25
class ToggleOp a where
  tgl :: Expr -> a

------------------------------------------------------------------------
-- Registers and a monadic interface to managing them.
------------------------------------------------------------------------

class Monad m => MonadRegisters m where
  reg  :: Register -> m Int
  (=:) :: Register -> Int -> m ()

(+=), (-=) :: MonadRegisters m => Register -> Int -> m ()
r += d = do x <- reg r; r =: x+d
r -= d = r += negate d
infix 2 =:, +=, -=

value :: MonadRegisters m => Expr -> m Int
value (ExprInt      i) = return i
value (ExprRegister r) = reg r

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
  {-# INLINE inc #-}
  {-# INLINE dec #-}
  {-# INLINE cpy #-}
  {-# INLINE jnz #-}

------------------------------------------------------------------------
-- Implementation of out instruction
------------------------------------------------------------------------

class Monad m => MonadOutput m where
  recordOutput :: Int -> m ()

instance (MonadRegisters m, MonadOutput m) => OutputOp (Eval m) where
  out o = Eval $ do recordOutput =<< value o; PC += 1

------------------------------------------------------------------------
-- Implementation of the primitive tgl instrution
------------------------------------------------------------------------

class Monad m => MonadToggleFlag m where
  isToggled :: Int -> m Bool
  togglePC  :: Int -> m ()

instance (MonadRegisters m, MonadToggleFlag m) => ToggleOp (Eval m) where
  tgl x = Eval $
    do offset <- value x
       pc     <- reg PC
       togglePC (pc+offset)
       PC =: pc+1

------------------------------------------------------------------------
-- Alternative evaluation type that can support toggling
------------------------------------------------------------------------

-- | Like 'Eval', but dispatches to the "toggled" instruction when toggle flag is set
newtype ToggleEval m = ToggleEval { toggleEval :: m () }

instance (MonadRegisters m, MonadToggleFlag m) => BasicOps (ToggleEval m) where
  inc x   = toggler (inc x  ) (dec x)
  dec x   = toggler (dec x  ) (inc x)
  jnz x y = toggler (jnz x y) (cpy x `needReg` y)
  cpy x y = toggler (cpy x y) (jnz x (ExprRegister y))

instance (MonadRegisters m, MonadToggleFlag m) => ToggleOp (ToggleEval m) where
  tgl x = toggler (tgl x) (inc `needReg` x)

-- | Instructions flipped to invalid operations become no-ops
needReg :: MonadRegisters m => (Register -> Eval m) -> Expr -> Eval m
needReg f (ExprRegister r) = f r
needReg _ _                = Eval (PC += 1)

toggler :: (MonadRegisters m, MonadToggleFlag m) => Eval m -> Eval m -> ToggleEval m
toggler (Eval normal) (Eval toggled) = ToggleEval $
  do x <- isToggled =<< reg PC
     if x then toggled else normal
{-# INLINE toggler #-}

------------------------------------------------------------------------
-- MachineT: An implementation of MonadRegisters
------------------------------------------------------------------------

newtype MachineT m a = MachineT (StateT Registers m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans)

data Registers = Registers
  { registerPC, registerA, registerB, registerC, registerD :: !Int }
  deriving (Show, Eq)

instance PrimMonad m => PrimMonad (MachineT m) where
  type PrimState (MachineT m) = PrimState m
  primitive = lift . primitive

instance Monad m => MonadRegisters (MachineT m) where
  reg r = MachineT $
       gets $ case r of
         PC -> registerPC
         A  -> registerA
         B  -> registerB
         C  -> registerC
         D  -> registerD
  r =: x = MachineT $
       modify' $ \rs -> case r of
         PC -> rs { registerPC = x }
         A  -> rs { registerA  = x }
         B  -> rs { registerB  = x }
         C  -> rs { registerC  = x }
         D  -> rs { registerD  = x }
  {-# INLINE (=:) #-}
  {-# INLINE reg  #-}

runMachineT :: Registers -> MachineT m a -> m (a, Registers)
runMachineT r (MachineT m) = runStateT m r

evalMachineT :: Monad m => MachineT m a -> m a
evalMachineT m = fst <$> runMachineT initialRegisters m

initialRegisters :: Registers
initialRegisters = Registers 0 0 0 0 0

------------------------------------------------------------------------
-- OutputT
------------------------------------------------------------------------

-- | An implementation of 'MonadOutput' that checks for alternating output
newtype OutputT m a = OutputT (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runOutputT :: Int -> OutputT m a -> m (a, Int)
runOutputT x (OutputT m) = runStateT m x

-- | Run an 'OutputT' but ignore the /result/ and keep the output counter.
execOutputT :: Functor m => Int -> OutputT m a -> m Int
execOutputT x o = snd <$> runOutputT x o

-- | Pass registers through to underlying machine
instance MonadRegisters m => MonadRegisters (OutputT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)

instance MonadPlus m => MonadOutput (OutputT m) where
  recordOutput x = OutputT $
    do n <- state $ \n -> let n' = n+1 in n' `seq` (n,n')
       guard (x == if even n then 0 else 1)

------------------------------------------------------------------------
-- OutputListT
------------------------------------------------------------------------

-- | An implementation of MonadOutput that gathers outputs in list
newtype OutputListT m a = OutputListT (WriterT (Endo [Int]) m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Run an 'OutputListT' computation and ignore the /result/ keeping only
-- the output list.
execOutputListT :: Monad m => OutputListT m a -> m [Int]
execOutputListT (OutputListT m) = (`appEndo` []) <$> execWriterT m

instance Monad m => MonadOutput (OutputListT m) where
  recordOutput = OutputListT . tell . Endo . (:)

-- | Pass registers through to underlying monad
instance MonadRegisters m => MonadRegisters (OutputListT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)
  {-# INLINE (=:) #-}
  {-# INLINE reg  #-}

------------------------------------------------------------------------
-- ToggleT
------------------------------------------------------------------------

-- | Monad transformer implementing 'MonadToggleFlag'
newtype ToggleT m a = ToggleT (ReaderT (MVector (PrimState m) Bool) m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans ToggleT where lift = ToggleT . lift -- GND can't handle this one

-- | Run a togglable machine with all instructions set to not be toggled.
runToggleT :: PrimMonad m => Int {- program size -} -> ToggleT m a -> m a
runToggleT n (ToggleT m) = runReaderT m =<< MV.replicate n False

-- | Check that given index is in range for given vector
inRange :: Unbox a => MVector s a -> Int -> Bool
inRange v i = i >= 0 && i < MV.length v

instance PrimMonad m => MonadToggleFlag (ToggleT m) where
  isToggled i = ToggleT $ ReaderT $ \v ->
    if inRange v i then MV.read v i else return False
  togglePC i = ToggleT $ ReaderT $ \v ->
    when (inRange v i) (MV.modify v not i)
  {-# INLINE isToggled #-}
  {-# INLINE togglePC #-}

-- | Pass registers through to underlying monad
instance MonadRegisters m => MonadRegisters (ToggleT m) where
  x =: y = lift (x =: y)
  reg x  = lift (reg x)
  {-# INLINE (=:) #-}
  {-# INLINE reg  #-}

------------------------------------------------------------------------
-- Functions for executing a whole program
------------------------------------------------------------------------

-- | Big-step machine semantics. Runs machine until it halts.
runUntilEnd :: MonadRegisters m => Vector (m ()) -> m ()
runUntilEnd program = go where
  go = do pc <- reg PC
          case program V.!? pc of
            Nothing -> return ()
            Just m  -> do m; go


-- | Predicate for machines that loop while producing a good output stream.
isGoodLoop ::
  Vector (OutputT (MachineT Maybe) ()) {- ^ instructions             -} ->
  Int                                  {- ^ initial register A value -} ->
  Bool                                 {- ^ is 0,1 generator         -}
isGoodLoop program start = isWorking (go s0 <$> step s0)
  where
    -- initial machine state
    s0 = (0, initialRegisters { registerA = start })

    -- small-step machine semantics
    step (outs, regs) =
      do op <- program V.!? registerPC regs
         runMachineT regs (execOutputT outs op)

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

-- | Parse the file using the set of possible instructions.
loadFile :: [(String, Parser a)] -> FilePath -> IO (Vector a)
loadFile instructions filename =
  do txt <- Text.readFile filename
     case P.parse (parseLines (dispatch instructions)) filename txt of
       Left e  -> throwIO e
       Right p -> return $! V.fromList p

-- | Parser the lines of the file using a single parser
parseLines :: Parser a -> Parser [a]
parseLines p = P.sepEndBy p P.eol <* P.eof

-- | Select a parser by selecting via the next alphabetic lexeme
dispatch :: [(String,Parser a)] -> Parser a
dispatch xs =
  do name <- lexeme (some P.letterChar)
     case lookup name xs of
       Just p  -> p
       Nothing -> fail ("unexpected '" ++ name ++ "'")

-- | Wrap a parser to handle trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = P.lexeme (P.skipMany (P.char ' '))

-- | Parse a single register
pRegister :: Parser Register
pRegister = dispatch [("a",pure A),("b",pure B),("c",pure C),("d",pure D)]

-- | Parse a register or a integer constant
pExpr :: Parser Expr
pExpr = ExprRegister          <$> pRegister
    <|> ExprInt . fromInteger <$> lexeme (P.signed (pure ()) P.decimal)

------------------------------------------------------------------------
-- Individual instruction names and argument parsers
------------------------------------------------------------------------

-- | Basic machine instructions common to all programs
basicInstructions :: BasicOps a => [(String, Parser a)]
basicInstructions =
  [ ("inc", strict (liftA  inc pRegister      ))
  , ("dec", strict (liftA  dec pRegister      ))
  , ("jnz", strict (liftA2 jnz pExpr pExpr    ))
  , ("cpy", strict (liftA2 cpy pExpr pRegister)) ]
{-# INLINE basicInstructions #-}

strict :: Monad m => m a -> m a
strict m = do x <- m; return $! x
{-# INLINE strict #-}

-- | Toggle machine instruction specific to proram 23
toggleInstruction :: ToggleOp a => (String, Parser a)
toggleInstruction = ("tgl", strict (liftA tgl pExpr))
{-# INLINE toggleInstruction #-}

-- | Output machine instruction specific to proram 25
outputInstruction :: OutputOp a => (String, Parser a)
outputInstruction = ("out", strict (liftA out pExpr))
{-# INLINE outputInstruction #-}

------------------------------------------------------------------------
-- String builder representation
------------------------------------------------------------------------

-- | Newtype'd 'ShowS' used to interpret instructions as pretty-printed output
newtype Render = Render (Endo String)
  deriving (Monoid, Semigroup)

renderProgram :: Foldable t => t Render -> String
renderProgram = renderToString . fold

renderToString :: Render -> String
renderToString (Render m) = m `appEndo` ""

fromShowS :: ShowS -> Render
fromShowS = Render . Endo

rString :: String -> Render
rString = fromShowS . showString

rExpr :: Expr -> Render
rExpr (ExprRegister r) = rRegister r
rExpr (ExprInt      i) = fromShowS (shows i)

rRegister :: Register -> Render
rRegister r = rString $ case r of PC -> "pc"; A -> "a"; B -> "b"; C -> "c"; D -> "d"

------------------------------------------------------------------------
-- Renderer interpretation of machine instructions ---------------------
------------------------------------------------------------------------

instance BasicOps Render where
  inc x   = rString "inc " <> rRegister x <> rString "\n"
  dec x   = rString "dec " <> rRegister x <> rString "\n"
  cpy x y = rString "cpy " <> rExpr x <>
            rString " "    <> rRegister y <> rString "\n"
  jnz x y = rString "jnz " <> rExpr x <>
            rString " "    <> rExpr y <> rString "\n"

instance ToggleOp Render where
  tgl x   = rString "tgl " <> rExpr x <> rString "\n"

instance OutputOp Render where
  out x   = rString "out " <> rExpr x <> rString "\n"

------------------------------------------------------------------------
-- Multiple interpretations in parallel
------------------------------------------------------------------------

instance (BasicOps a, BasicOps b) => BasicOps (a,b) where
  inc x   = (inc x  , inc x  )
  dec x   = (dec x  , dec x  )
  cpy x y = (cpy x y, cpy x y)
  jnz x y = (jnz x y, jnz x y)

instance (ToggleOp a, ToggleOp b) => ToggleOp (a,b) where
  tgl x   = (tgl x, tgl x)

instance (OutputOp a, OutputOp b) => OutputOp (a,b) where
  out x   = (out x, out x)
