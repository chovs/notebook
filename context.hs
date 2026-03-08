-- Model the React context concept in Haskell

-- In React, "context" is a mechanism for passing data deeply through a component tree without having to pass props manually at every level.
-- In Haskell, we can model this using the Reader monad, which provides a read-only shared environment, similar to React's context.

import Control.Monad.Reader

-- Let's define a 'Context' type holding shared information, like a theme and user name (as an example).
data Context = Context { theme :: String, username :: String } deriving Show

-- A "component" is something that runs in this Context and produces a value (e.g. renders to String).
type Component a = Reader Context a

-- Example: Access the theme from the context
themeComponent :: Component String
themeComponent = do
  ctx <- ask
  return $ "Theme is: " ++ theme ctx

-- Example: Access the username from the context
userComponent :: Component String
userComponent = do
  ctx <- ask
  return $ "Hello, " ++ username ctx

-- Compose components
pageComponent :: Component String
pageComponent = do
  t <- themeComponent
  u <- userComponent
  return $ t ++ "\n" ++ u

-- To "render" the components, run them with a Context:
-- runReader pageComponent (Context {theme="dark", username="alice"})

-- Exercises:

-- 1. Add a new field to Context, such as 'language', and write a component that greets the user in different languages.
-- 2. Write a component that combines theme and language information in a single string.
-- 3. Create a function that takes a Component and a Context and prints its result, simulating a context provider.

-- Thoughts:
-- - The Reader monad in Haskell elegantly models React's context provider/consumer pattern.
-- - Remapping, composing, and transforming context is easy with functional patterns.
-- - It's simple to "provide" different contexts by running the Reader with different Context values.




context [weight] [value]