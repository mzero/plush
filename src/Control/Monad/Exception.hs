{-
Copyright 2012 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# Language CPP, GeneralizedNewtypeDeriving, RankNTypes #-}
{-# Language DeriveTraversable #-}

{-| This module supports monads that can throw extensible exceptions. The
    exceptions are the very same from "Control.Exception", and the operations
    offered very similar, but here they are not limited to 'IO'.

    This code is in the style of both transformers and mtl, and is compatible
    with them, though doesn't mimic the module structure or offer the complete
    range of features in those packages.

    This is very similar to 'ErrorT' and 'MonadError', but based on features of
    "Control.Exception". In particular, it handles the complex case of
    asynchronous exceptions by including 'mask' in the typeclass. Note that the
    extensible extensions feature relies the RankNTypes language extension.
-}

module Control.Monad.Exception (
    -- * Typeclass
    -- $mtl
    MonadException(..),

    -- * Transformer
    -- $transformer
    ExceptionT, runExceptionT,

    -- * Utilities
    -- $utilities
    catchAll, catchIOError, catchJust, catchIf,
    onException,
    bracket, bracket_,
    finally,
  ) where

#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif

import Control.Applicative (Alternative, Applicative)
import Control.Arrow (left)
import Control.Exception (Exception(..), SomeException(..))
import qualified Control.Exception as ControlException
import Control.Monad (liftM, MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)
import qualified Control.Monad.Trans.Error as Error
import qualified Control.Monad.Trans.State.Lazy as LazyState
#if MIN_VERSION_transformers(0,3,0)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

-- $mtl
-- The mtl style typeclass

class (Monad m) => MonadException m where
    -- | Throw an exception. Note that this throws when this action is run in
    -- the monad /@m@/, not when it is applied. It is a generalization of
    -- "Control.Exception"'s 'Control.Exception.throwIO'.
    throwM :: (Exception e) => e -> m a

    -- | Provide a handler for exceptions thrown during execution of the first
    -- action. Note that type of the type of the argument to the handler will
    -- constrain which exceptions are caught. See "Control.Exception"'s
    -- 'Control.Exception.catch'.
    catch :: (Exception e) => m a -> (e -> m a) -> m a

    -- | Runs an action with asynchronous exceptions diabled. The action is
    -- provided a method for restoring the async. environment to what it was
    -- at the 'mask' call. See "Control.Exception"'s 'Control.Exception.mask'.
    mask :: ((forall a. m a -> m a) -> m b) -> m b

instance MonadException IO where
    throwM = ControlException.throwIO
    catch = ControlException.catch
    mask = ControlException.mask

-- Support for other transformers
instance (MonadException m) => MonadException (LazyState.StateT s m) where
    throwM e = lift $ throwM e
    catch = LazyState.liftCatch catch
    mask a = LazyState.StateT $ \s ->
        mask $ \u -> LazyState.runStateT (a $ q u) s
      where
        q u b = LazyState.StateT $ \s -> u $ LazyState.runStateT b s


-- $transformer
-- The transformers style monad transfomer

-- | Add exception abilities to a monad.
newtype ExceptionT m a =
    ExceptionT { unEx :: Error.ErrorT WrappedException m a }
#if MIN_VERSION_transformers(0,3,0)
  deriving (Functor, Foldable, Traversable, Applicative, Alternative,
            Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)
#else
  deriving (Functor, Applicative, Alternative,
            Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)
#endif

runExceptionT :: (Monad m) => ExceptionT m a -> m (Either SomeException a)
runExceptionT = liftM (left unWrapException) . Error.runErrorT . unEx

instance (Monad m) => MonadException (ExceptionT m) where
    throwM = ExceptionT . Error.throwError . WrapException . toException
    catch a c = ExceptionT $ unEx a `Error.catchError`
        (\e -> maybe (Error.throwError e) (unEx . c)
                (fromException . unWrapException $ e))
    mask a = a id


newtype WrappedException = WrapException { unWrapException :: SomeException }
instance Error.Error WrappedException where
    strMsg = WrapException . toException . userError


-- $utilities
-- These functions follow those from "Control.Exception", except that they are
-- based on methods from the 'MonadException' typeclass. See
-- "Control.Exception" for API usage.

-- | Catches all exceptions, and somewhat defeats the purpose of the extensible
-- exception system. Use sparingly.
catchAll :: (MonadException m) => m a -> (SomeException -> m a) -> m a
catchAll = catch

-- | Catch all 'IOError' (eqv. 'IOException') exceptions. Still somewhat too
-- general, but better than using 'catchAll'. See 'catchIf' for an easy way
-- of catching specific 'IOError's based on the predicates in "System.IO.Error".
catchIOError :: (MonadException m) => m a -> (IOError -> m a) -> m a
catchIOError = catch

-- | Catch exceptions only if they pass some predicate. Often useful with the
-- predicates for testing 'IOError' values in "System.IO.Error".
catchIf :: (MonadException m, Exception e) =>
    (e -> Bool) -> m a -> (e -> m a) -> m a
catchIf f a b = a `catch` (\e -> if f e then b e else throwM e)

-- | A more generalized way of determining which exceptions to catch at
-- run time.
catchJust :: (MonadException m, Exception e) =>
    (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` (\e -> maybe (throwM e) b $ f e)

-- | Run an action only if an exception is thrown in the main action. The
-- exception is not caught, simply rethrown.
onException :: (MonadException m) => m a -> m b -> m a
onException action handler = action `catchAll` (\e -> handler >> throwM e)

-- | Generalized abstracted pattern of safe resource acquisition and release
-- in the face of exceptions. The first action \"aquires\" some value, which
-- is \"released\" by the second action at the end. The third action \"uses\"
-- the value and its result is the result of the 'bracket'.
--
-- If an exception occurs during the use, the release still happens before the
-- exception is rethrown.
bracket :: (MonadException m) => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire release use = mask $ \unmasked -> do
    resource <- acquire
    result <- unmasked (use resource) `onException` release resource
    _ <- release resource
    return result

-- | Version of 'bracket' without any value being passed to the second and
-- third actions.
bracket_ :: (MonadException m) => m a -> m b -> m c -> m c
bracket_ before after action = bracket before (const after) (const action)

-- | Perform an action with a finalizer action that is run, even if an
-- exception occurs.
finally :: (MonadException m) => m a -> m b -> m a
finally action finalizer = bracket_ (return ()) finalizer action
