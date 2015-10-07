module Conditional where

import Distribution.Version(VersionRange)

--A data structure to represent the compilier version
data Compiler = GHC | JHC | UHC | LHC --The valid haskell compiler flavours
data CompilerVersion = CompilerVersion Compiler VersionRange -- The compiler and a version range

--A data stucture to hold the conditionals that cabal can ask of the platform
data PlatformBasicConditional = OpertionSystem String -- Tests if the platform operating system is the given string
                              | Architecture String --Tests if the platform architecture is the given string
                              | Implimentaton CompilerVersion --Tests the platform compiler version
--A data structure to hold the conditionals that cabal can ask of a configuration flag
data FlagBasicConditional = Flag String --Tests the setting of the given flag on the given platform

--The combinators that can be used to combine conditionals
data ConditionalTree cond = Logic Bool --Returns the given boolean
                          | And (ConditionalTree cond) (ConditionalTree cond) --If both conditionals hold
                          | Or (ConditionalTree cond) (ConditionalTree cond) --If either conditional hold
                          | Not (ConditionalTree cond) --If the conditional does not hold

--The conditionals possible conditionals that cabal can ask of the configuration flags
type FlagConditional = ConditionalTree FlagBasicConditional

--The conditionals possible conditionals that cabal can ask of the platform or the configuration flags
type PlatformConditional = ConditionalTree (Either PlatformBasicConditional FlagBasicConditional)