module Reussir.Core.Uitls.HashTable where
import Data.HashTable.Class qualified as HC
import Effectful (IOE, (:>), Eff, MonadIO (liftIO))
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable)
import Data.HashTable.ST.Cuckoo qualified as HST

{-# SPECIALIZE new @HST.HashTable #-}
{-# INLINABLE new #-}
new :: (HC.HashTable h, IOE :> es) => Eff es (H.IOHashTable h k v)
new = liftIO H.new

{-# SPECIALIZE insert @HST.HashTable #-}
{-# INLINABLE insert #-}
insert :: (HC.HashTable h, Hashable k, IOE :> es) => 
    H.IOHashTable h k v -> k -> v -> Eff es ()
insert table key value = liftIO $ H.insert table key value

{-# SPECIALIZE Reussir.Core.Uitls.HashTable.lookup @HST.HashTable #-}
{-# INLINABLE lookup #-}
lookup :: (HC.HashTable h, Hashable k, IOE :> es) => 
    H.IOHashTable h k v -> k -> Eff es (Maybe v)
lookup table key = liftIO $ H.lookup table key

{-# SPECIALIZE delete @HST.HashTable #-}
{-# INLINABLE delete #-}
delete :: (HC.HashTable h, Hashable k, IOE :> es) => 
    H.IOHashTable h k v -> k -> Eff es ()
delete table key = liftIO $ H.delete table key

{-# SPECIALIZE toList @HST.HashTable #-}
{-# INLINABLE toList #-}
toList :: (HC.HashTable h, Hashable k, IOE :> es) => 
    H.IOHashTable h k v -> Eff es [(k, v)]
toList table = liftIO $ H.toList table
