module Cabal.Conditional where

import Distribution.Version(VersionRange)
import Distribution.System(OS,Arch)
import Distribution.Compiler(CompilerFlavor)

--A data structure to represent the compilier version
type Compiler = CompilerFlavor --The valid haskell compiler flavours
data CompilerVersion = CompilerVersion Compiler VersionRange -- The compiler and a version range
          deriving(Eq,Show,Read)

--A data stucture to hold the conditionals that cabal can ask of the platform
data PlatformBasicConditional = OperatingSystem OS -- Tests if the platform operating system is the given string
                              | Architecture Arch --Tests if the platform architecture is the given string
                              | Implimentaton CompilerVersion --Tests the platform compiler version
          deriving(Eq,Show,Read)

--A data structure to hold the conditionals that cabal can ask of a configuration flag
data FlagBasicConditional = Flag String --Tests the setting of the given flag on the given platform
          deriving(Eq,Show,Read)

--The combinators that can be used to combine conditionals
data ConditionalTree cond = Logic Bool --Returns the given boolean
                          | Var cond --A basic conditonal variable
                          | And (ConditionalTree cond) (ConditionalTree cond) --If both conditionals hold
                          | Or (ConditionalTree cond) (ConditionalTree cond) --If either conditional hold
                          | Not (ConditionalTree cond) --If the conditional does not hold
          deriving(Eq,Show,Read)

--The conditionals possible conditionals that cabal can ask of the configuration flags
type FlagConditional = ConditionalTree FlagBasicConditional

--The conditionals possible conditionals that cabal can ask of the platform or the configuration flags
type PlatformConditional = ConditionalTree (Either PlatformBasicConditional FlagBasicConditional)
--Newtype container for a platform conditional
newtype PlatformConditionalType = PlatformConditional PlatformConditional deriving(Eq,Show,Read)
