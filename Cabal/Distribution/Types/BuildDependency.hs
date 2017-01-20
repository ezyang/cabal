{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Types.BuildDependency (
    BuildDependency(..),
    buildDependencyToDependency,
    buildDependencyToMixin,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Types.UnqualComponentName
import Distribution.Types.Mixin
import Distribution.Types.Dependency
import Distribution.Types.IncludeRenaming
import Distribution.Version

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
import Distribution.Text
import Text.PrettyPrint ((<+>), text)

-- | Like 'Dependency', but this corresponds exactly to
-- the syntax we support in a Cabal file.
data BuildDependency = BuildDependency {
        buildDependsPackageName :: PackageName,
        buildDependsLibraryName :: Maybe UnqualComponentName,
        buildDependsVersionRange :: VersionRange
    }
    deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary BuildDependency
instance NFData BuildDependency where rnf = genericRnf

instance Text BuildDependency where
    disp (BuildDependency name Nothing vr) =
        disp name <+> disp vr
    disp (BuildDependency name (Just cname) vr) =
        disp name <<>> text ":" <<>> disp cname <+> disp vr

    parse = do name <- parse
               mb_cname <- option Nothing $ do
                                _ <- char ':'
                                fmap Just parse
               skipSpaces
               ver <- parse <++ return anyVersion
               return (BuildDependency name mb_cname ver)

buildDependencyToDependency :: BuildDependency -> Dependency
buildDependencyToDependency (BuildDependency pn _ vr) = Dependency pn vr

buildDependencyToMixin :: BuildDependency -> Mixin
buildDependencyToMixin (BuildDependency pn cn _) = Mixin pn cn defaultIncludeRenaming
