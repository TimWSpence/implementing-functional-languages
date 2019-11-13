{-# LANGUAGE RecordWildCards #-}

module Utils
  ( Heap
  , Addr
  , hInitial
  , hAlloc
  , hUpdate
  , hFree
  , hLookup
  , hAddresses
  , hSize
  , hNull
  , hIsNull
  ) where

import qualified Data.Map as M

hInitial :: Heap a
hInitial = Heap {
  size = 0
, free = [1..]
, store = M.empty
                }

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc Heap{..} a = case free of
  (next:rest) -> (Heap {
    size = size + 1
  , free = rest
  , store = M.insert next a store
                      }, next)
  _ -> error "Unreachable"

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate h@Heap{..} addr a = h { store = M.insert addr a store }

hFree :: Heap a -> Addr -> Heap a
hFree Heap{..} addr = Heap {
  size = size - 1
, free = free
, store = M.delete addr store
                           }

hLookup :: Heap a -> Addr -> a
hLookup Heap{..} addr = store M.! addr

hAddresses :: Heap a -> [Addr]
hAddresses Heap{..} = M.keys store

hSize :: Heap a -> Int
hSize = size

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull = (== 0)

data Heap a = Heap {
  size :: Int
, free :: [Addr]
, store :: M.Map Addr a
                   }

type Addr = Int
