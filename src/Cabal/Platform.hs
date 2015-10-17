module Cabal.Platform where

import Distribution.System(OS,Arch)
import Cabal.Conditional(CompilerVersion
                        ,PlatformBasicConditional(..)
                        ,PlatformConditional
                        ,PlatformConditionalType
                        ,FlagConditional
                        ,unwrapPlatformConditionalType,
                        ConditionalTree(..), simplifyTree, changeVars)

--A data structure to represent a given platform that packages
-- Can be configured and built on.
--Includes the basic tests that cn be performed in a cabal file

data Platform = Platform {
  operatingSystem :: OS,
  architecture :: Arch,
  compiler :: CompilerVersion
} 

--Localise a platform conditional by 
--Substituting the plaform conditions in with the given platform
--Then simplify the resultant tree
localiseConditional :: Platform -> PlatformConditional -> FlagConditional
localiseConditional platform condTree = simplifyTree $ changeVars localiseVars condTree
  where localiseVars (Right flagVar) = Var flagVar
        localiseVars (Left (OperatingSystem os)) = Logic (os == operatingSystem platform)
        localiseVars (Left (Architecture arch)) = Logic (arch == architecture platform)
        

localiseConditionalType :: Platform -> PlatformConditionalType -> FlagConditional
localiseConditionalType platform = localiseConditional platform . unwrapPlatformConditionalType