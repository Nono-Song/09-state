{-
---
fulltitle: "In class exercise: Random Generation"
date:
---
-}

module RandomGen where

-- Make sure you have filled in all of the 'undefined' values in the State module.
-- If you have not, modify the State import below to Control.Monad.State
-- but don't import both State and Control.Monad.State
-- It also might be tempting to import Test.QuickCheck, but do not import anything
-- from quickcheck for this exercise.
import Control.Monad
import State qualified as S
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, randomIO, uniform, uniformR)

{-
Random Generation
-----------------

Recall that QuickCheck needs to randomly generate values of any type. It turns
out that we can use the state monad to define something like the `Gen` monad
used in the QuickCheck libary.

First, a brief discussion of pseudo-random number generators. [Pseudo-random
number generators](http://en.wikipedia.org/wiki/Pseudorandom_number_generator)
aren't really random, they just look like it. They are more like functions
that are so complicated that they might as well be random. The nice property
about them is that they are repeatable, if you give them the same *seed* they
produce the same sequence of "random" numbers.

Haskell has a library for Pseudo-Random numbers called
[`System.Random`](http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html).
It features the following elements:

~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type StdGen  -- A type for a "standard" random number generator.
             -- Keeps track of the current seed.

-- | Construct a generator from a given seed. Distinct arguments
-- are likely to produce distinct generators.
mkStdGen :: Int -> StdGen

-- The `uniform` function is overloaded, but we will only use two instances of
-- it today.
-}

-- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
uniformInt :: StdGen -> (Int, StdGen)
uniformInt = Random.uniform

-- | Returns True / False with even chances
uniformBool :: StdGen -> (Bool, StdGen)
uniformBool = Random.uniform

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~

Side note: the default constructor `mkStdGen` is a bit weak so we wrap it to
perturb the seed a little first:
-}

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

{-
For example, we can generate a random integer by constructing a random
number generator, calling `uniform` and then projecting the result.
-}

testRandom :: Int -> Int
testRandom i = fst (uniformInt (mkStdGen i))

{-
Our random integers depend on the seed that we provide. Make sure that you
get different numbers from these three calls.
-}

-- >>> testRandom 1
-- -8728299723972136512

-- >>> testRandom 2
-- 7133861895013252414

-- >>> testRandom 3
-- 5757771102651567923

{-
But we can also produce several different random `Int`s by using the
output of one call to `Random.uniform` as the input to the next.
-}

(int1 :: Int, stdgen1) = uniformInt (mkStdGen 1)

(int2 :: Int, stdgen2) = uniformInt stdgen1

(int3 :: Int, _) = uniformInt stdgen2

-- >>> int1
-- -8728299723972136512

-- >>> int2
-- 1508247628499543321
{-
>
-}

-- >>> int3
-- 4708425006071971359

{-
If we'd like to constrain that integer to a specific range `(0, n)`
we can use the mod operation.
-}

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound s = let (x, s1) = uniformInt s in (x `mod` bound, s1)

{-
These tests should all produce random integers between 0 and 20.
-}

testBounded :: Int -> Int
testBounded i = fst (nextBounded 20 (mkStdGen i))

-- >>> testBounded 1
-- 8

-- >>> testBounded 2
-- 14

-- >>> testBounded 3
-- 3

{-
QuickCheck is defined by a class of types that can construct random
values. Let's do it first the hard way... i.e. by explicitly passing around the
state of the random number generator.

-}

-- | Extract random values of any type
class Arb1 a where
  arb1 :: StdGen -> (a, StdGen)

instance Arb1 Int where
  arb1 :: StdGen -> (Int, StdGen)
  arb1 = uniformInt

instance Arb1 Bool where
  arb1 :: StdGen -> (Bool, StdGen)
  arb1 = uniformBool

{-
With this class, we can also generalize our "testing" function.
-}

testArb1 :: (Arb1 a) => Int -> a
testArb1 = fst . arb1 . mkStdGen

{-
What about for pairs? Note that Haskell needs the type annotations for
the two calls to `arb1` to resolve ambiguity.
-}

instance (Arb1 a, Arb1 b) => Arb1 (a, b) where
  arb1 :: StdGen -> ((a, b), StdGen)
  arb1 s =
    let (a :: a, s1) = arb1 s
        (b :: b, s2) = arb1 s1
     in ((a, b), s2)

{-
Try out this definition, noting the different integers in the two
components in the pair. If both calls to `arb1` above used `s`, then we'd get
the same number in both components.
-}

-- >>> testArb1 4 :: Bool
-- True

-- >>> testArb1 1 :: (Int, Int)
-- (-8728299723972136512,1508247628499543321)
{-
>
-}

-- >>> testArb1 2 :: (Int, Int)
-- (7133861895013252414,-3695387158857804490)

{-
How about for the `Maybe` type? Use the `arb1` instance for the `Bool` type
 above to generate a random boolean and then test it to decide whether you
should return `Nothing` or `Just a`, where the `a` also comes from `arb1`.
-}

instance (Arb1 a) => Arb1 (Maybe a) where
  arb1 :: StdGen -> (Maybe a, StdGen)
  arb1 s =
    let (b, s1) = arb1 s
     in if b
          then
            let (a, s2) = arb1 s1
             in (Just a, s2)
          else (Nothing, s1)

-- >>> testArb1 4 :: (Maybe Int)
-- Just (-7813227110773096510)

{-
And for lists? Give this one a try!  Although we don't have QCs combinators
available, you should be able to control the frequency of when cons and nil
is generated so that you get reasonable lists.

-}

instance (Arb1 a) => Arb1 [a] where
  arb1 :: (Arb1 a) => StdGen -> ([a], StdGen)
  arb1 = arbList 5
    where
      arbList depth s =
        if depth > 0
          then
            let (x :: a, s1) = arb1 s
                (xs :: [a], s2) = arbList (depth - 1) s1
             in (x : xs, s2)
          else ([], s)

-- >>> testArb1 1 :: [Int]
-- [-8728299723972136512,1508247628499543321,4708425006071971359,3295616113604390420,-6470300321862196297]

-- >>> testArb1 2 :: [Int]
-- [7133861895013252414,-3695387158857804490,9055518795393354129,6913472612286637009,9000619708019330313]

-- >>> testArb1 3 :: [Int]
-- [5757771102651567923,-4340067171599948441,3693384238643783450,3754176236000241207,7555297385651819641]

{-
Ouch, there's a lot of state passing going on here.

State Monad to the Rescue
-------------------------

Previously, we have developed a reusable library for the State monad.
Let's use it to *define* a generator monad for QuickCheck.

Our reusable library defines an abstract type for the state monad, and
the following operations for working with these sorts of computations.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type State s a = ...

instance Monad (State s) where ...

get      :: State s s
put      :: s -> State s ()

runState :: State s a -> s -> (a,s)
~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let's define a type for generators, using the state monad.
-}

type Gen a = S.State StdGen a

{-
With this type, we can create a type class similar to the one in the
QuickCheck library.
-}

class Arb a where
  arb :: Gen a

{-
For example, we can use the operations on the state monad to access and update the
random number generator stored in the `State StdGen a` type.
-}

instance Arb Int where
  arb :: Gen Int
  arb = do
    s <- S.get
    let (y :: Int, s') = Random.uniform s
    S.put s'
    return y

{-
What if we want a bounded generator? See if you can define one without using `Random.uniformR`.
-}

bounded :: Int -> Gen Int
bounded b = do
  n <- arb
  return (n `mod` b)

{-
Now define a `sample` function, which generates and prints 10 random values.
-}

sample :: (Show a) => Gen a -> IO ()
sample gen = do
  seed <- (Random.randomIO :: IO Int) -- get a seed from the global random number generator
  -- hidden in the IO monad
  undefined

{-
For example, you should be able to sample using the `bounded` combinator.

    ghci> sample (bounded 10)
    5
    9
    0
    5
    4
    6
    0
    0
    7
    6

What about random generation for other types?  How does the state
monad help that definition? How does it compare to the version above?
-}

instance (Arb a, Arb b) => Arb (a, b) where
  arb :: (Arb a, Arb b) => Gen (a, b)
  arb = undefined

{-
Can we define some standard QuickCheck combinators to help us?
What about `elements`, useful for the `Bool` instance ?
-}

elements :: [a] -> Gen a
elements = undefined

instance Arb Bool where
  arb :: Gen Bool
  arb = elements [False, True]

{-
or `frequency`, which we can use for the `[a]` instance ?
-}

frequency :: [(Int, Gen a)] -> Gen a
frequency = undefined

instance (Arb a) => Arb [a] where
  arb :: (Arb a) => Gen [a]
  arb = frequency [(1, return []), (3, (:) <$> arb <*> arb)]
