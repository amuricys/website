-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeFamilies #-}

-- import Data.Kind (Type)
-- import GHC.TypeLits (Symbol)
-- import Data.Proxy (Proxy)

-- -- Define a type-level list of keys.
-- data KeyList = End | Key Symbol KeyList

-- -- Define a data type that has a different type depending on the type-level key list.
-- data Graph (k :: KeyList) :: Type where
--   Leaf :: Graph 'End
--   Node :: Graph k -> Graph ('Key s k)

-- -- Define a type family for safe lookup operation. This will fail to compile
-- -- if you try to lookup a key that is not in the list.
-- type family Lookup (s :: Symbol) (k :: KeyList) :: KeyList where
--   Lookup s ('Key s k) = k
--   Lookup s ('Key s' k) = Lookup s k

-- -- Now, you can define a safe lookup operation.
-- lookup :: Graph k -> Proxy s -> Graph (Lookup s k)
-- lookup (Node g) _ = g
