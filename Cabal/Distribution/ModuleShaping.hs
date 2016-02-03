module Distribution.ModuleShaping where

import Distribution.Package
import Distribution.Backpack
import Distribution.InstalledPackageInfo hiding (installedUnitId)
import Distribution.ModuleName

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

-- Food for thought: suppose we apply the Merkel tree optimization.
-- Imagine this situation:
--
--      component p
--          signature H
--          module P
--      component h
--          module H
--      component a
--          signature P
--          module A
--      component q(P)
--          include p
--          include h
--      component r
--          include q (P)
--          include p (P) requires (H)
--          include h (H)
--          include a (A) requires (P)
--
-- Component r should not have any conflicts, since after mix-in linking
-- the two P imports will end up being the same, so we can properly
-- instantiate it.  But to know that q's P is p:P instantiated with h:H,
-- we have to be able to expand its unit id.  Maybe we can expand it
-- lazily but in some cases it will need to be expanded.
shapeInstalledPackage :: InstalledPackageInfo -> ModuleShape
shapeInstalledPackage ipi = ModuleShape (mkModSubst provs) reqs
  where
    uid = installedUnitId ipi
    provs = map shapeExposedModule (exposedModules ipi)
    reqs = unitIdFreeHoles uid
    shapeExposedModule (ExposedModule mod_name Nothing)
        = (mod_name, Module uid mod_name)
    shapeExposedModule (ExposedModule mod_name (Just (Module uid' mod_name')))
        = (mod_name, Module uid' mod_name')
