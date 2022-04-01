{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.PackageId
  ( PackageIdentifier(..)
  , PackageId
  , RepoUpdate(..)
  , printPackages
  , getRepoUpdate
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec            (Parsec (..), simpleParsec)
import Distribution.Pretty
import Distribution.Types.PackageName
import Distribution.Version           (Version, nullVersion)

import qualified Data.List.NonEmpty              as NE
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp
import qualified Text.PrettyPrint as PP
import Data.List (sortOn)
import Data.Maybe (fromJust)

-- | Type alias so we can use the shorter name PackageId.
type PackageId = PackageIdentifier

-- | The name and version of a package.
data PackageIdentifier
    = PackageIdentifier {
        pkgName    :: PackageName, -- ^The name of this package, eg. foo
        pkgVersion :: Version -- ^the version of this package, eg 1.2
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageIdentifier
instance Structured PackageIdentifier

instance Pretty PackageIdentifier where
  pretty (PackageIdentifier n v)
    | v == nullVersion = pretty n -- if no version, don't show version.
    | otherwise        = pretty n <<>> Disp.char '-' <<>> pretty v

-- |
--
-- >>> simpleParsec "foo-bar-0" :: Maybe PackageIdentifier
-- Just (PackageIdentifier {pkgName = PackageName "foo-bar", pkgVersion = mkVersion [0]})
--
-- >>> simpleParsec "foo-bar" :: Maybe PackageIdentifier
-- Just (PackageIdentifier {pkgName = PackageName "foo-bar", pkgVersion = mkVersion []})
--
-- /Note:/ Stricter than 'Text' instance
--
-- >>> simpleParsec "foo-bar-0-0" :: Maybe PackageIdentifier
-- Nothing
--
-- >>> simpleParsec "foo-bar.0" :: Maybe PackageIdentifier
-- Nothing
--
-- >>> simpleParsec "foo-bar.4-2" :: Maybe PackageIdentifier
-- Nothing
--
-- >>> simpleParsec "1.2.3" :: Maybe PackageIdentifier
-- Nothing
--
instance Parsec PackageIdentifier where
  parsec = do
      xs' <- P.sepByNonEmpty component (P.char '-')
      (v, xs) <- case simpleParsec (NE.last xs') of
          Nothing -> return (nullVersion, toList xs') -- all components are version
          Just v  -> return (v, NE.init xs')
      if not (null xs) && all (\c ->  all (/= '.') c && not (all isDigit c)) xs
      then return $ PackageIdentifier (mkPackageName (intercalate  "-" xs)) v
      else fail "all digits or a dot in a portion of package name"
    where
      component = P.munch1 (\c ->  isAlphaNum c || c == '.')

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

data RepoUpdate = RepoUpdate {
  packagesUpdated   :: [PackageId],
  packagesAdded     :: [PackageId],
  packagesRemoved   :: [PackageId],

  numPackagesUpdated :: Int,
  numPackagesAdded :: Int,
  numPackagesRemoved :: Int
}

instance Pretty RepoUpdate where
  pretty (RepoUpdate  _ _ _ numUpdated numAdded numRemoved) = PP.text $ ""
    ++ "\nPackages Updated: " ++ show numUpdated
    ++ "\nPackages Added: " ++ show numAdded
    ++ "\nPackages Removed: " ++ show numRemoved

printPackages :: RepoUpdate -> String
printPackages (RepoUpdate updated added removed numUpdated numAdded numRemoved) =
  getS "Updated" updated numUpdated ++
  getS "Added" added numAdded ++
  getS "Removed" removed numRemoved
  where
    getS :: String -> [PackageId] -> Int -> String
    getS s list num = whenS (not . null $ list) ("\n\nPackages" ++ s ++": " ++ show num ++ "\n")
      ++ foldl (\acc pkg -> acc ++ "- " ++ prettyShow pkg ++ "\n") "" list
      ++ whenS (num > 100) ("and " ++ show (num - 100) ++ " more...\n")
    whenS :: Bool -> String -> String
    whenS True s = s
    whenS _ _ = ""

getRepoUpdate :: [PackageId] -> [PackageId] -> RepoUpdate
getRepoUpdate before after = 
  let
    filteredBefore = foldl filterForLatest [] before
    filteredAfter = foldl filterForLatest [] after

    updatedPackages = getDifferentPackageVersions filteredBefore filteredAfter
    newPackages = getDifferentPackageNames filteredBefore filteredAfter
    removedPackages = getDifferentPackageNames filteredAfter filteredBefore
  in
    RepoUpdate {
      packagesUpdated = take 100 updatedPackages,
      packagesAdded = take 100 newPackages,
      packagesRemoved = take 100 removedPackages,

      numPackagesUpdated = length updatedPackages,
      numPackagesAdded = length newPackages,
      numPackagesRemoved = length removedPackages
    }
  where
    getDifferentPackageVersions :: [PackageId] -> [PackageId] -> [PackageId]
    getDifferentPackageVersions before' after' = sortOn getSort $ filter (\pkg@(PackageIdentifier _ ver) -> isJust (findPackage pkg before') && pkgVersion (fromJust $ findPackage pkg before') < ver) after'
    getDifferentPackageNames :: [PackageId] -> [PackageId] -> [PackageId]
    getDifferentPackageNames before' after' = sortOn getSort $ filter (\pkg -> not . isJust $ findPackage pkg before') after'

    filterForLatest :: [PackageId] -> PackageId -> [PackageId]
    filterForLatest acc pkg@(PackageIdentifier name _)
      | not . isJust $ findPackage pkg acc = pkg : acc
      | isLatest pkg acc = pkg : filter (\(PackageIdentifier listName _) -> listName /= name) acc
      | otherwise = acc
    isLatest :: PackageId -> [PackageId] -> Bool
    isLatest pkg@(PackageIdentifier _ ver) list
      | isJust $ findPackage pkg list = (pkgVersion . fromJust $ findPackage pkg list) < ver
      | otherwise = True
    findPackage :: PackageId -> [PackageId] -> Maybe PackageId
    findPackage (PackageIdentifier name _) = find (\(PackageIdentifier listName _) -> listName == name)
    getSort :: PackageId -> String
    getSort (PackageIdentifier pName _ ) = unPackageName pName