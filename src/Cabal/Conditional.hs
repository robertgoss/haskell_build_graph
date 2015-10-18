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
          
--Reduce the conditional tree by using rules with and or and not
-- And explicit logical true and false
-- Simplify subtrees to see if they reduce.
simplifyTree :: ConditionalTree cond -> ConditionalTree cond
simplifyTree (Logic cond) = Logic cond
simplifyTree (Var cond) = Var cond

simplifyTree (And lSubTree rSubTree)
          = case lSimplifiedSubTree of 
                (Logic True) -> rSimplifiedSubTree
                (Logic False) -> Logic False
                _ -> case rSimplifiedSubTree of
                            (Logic True) -> lSimplifiedSubTree
                            (Logic False) -> Logic False
                            _ -> And lSimplifiedSubTree rSimplifiedSubTree
  where lSimplifiedSubTree = simplifyTree lSubTree
        rSimplifiedSubTree = simplifyTree rSubTree

simplifyTree (Or lSubTree rSubTree)
          = case lSimplifiedSubTree of 
                (Logic True) -> Logic True
                (Logic False) -> rSimplifiedSubTree
                _ -> case rSimplifiedSubTree of
                            (Logic True) -> Logic True
                            (Logic False) -> lSimplifiedSubTree
                            _ -> Or lSimplifiedSubTree rSimplifiedSubTree
  where lSimplifiedSubTree = simplifyTree lSubTree
        rSimplifiedSubTree = simplifyTree rSubTree

simplifyTree (Not subTree) = case simplifiedSubTree of
                                 (Logic boolean) -> Logic (not boolean)
                                 otherTree -> otherTree
  where simplifiedSubTree = simplifyTree subTree


--Induce a map of conditional trees by substituting variables in one tree with subtrees in another
changeVars :: (cond1 -> ConditionalTree cond2) -> ConditionalTree cond1 -> ConditionalTree cond2
changeVars _ (Logic boolean) = Logic boolean
changeVars varChange (Var var) = varChange var
changeVars varChange (Not cond) = Not $ changeVars varChange cond
changeVars varChange (Or cond1 cond2) 
         = Or (changeVars varChange cond1) (changeVars varChange cond2)
changeVars varChange (And cond1 cond2) 
         = And (changeVars varChange cond1) (changeVars varChange cond2)
--The conditionals possible conditionals that cabal can ask of the configuration flags
type FlagConditional = ConditionalTree FlagBasicConditional
--Newtype container for a platform conditional
newtype FlagConditionalType = FlagConditional FlagConditional deriving(Eq,Show,Read)
wrapFlagConditionalType cond = (FlagConditional cond)
unwrapFlagConditionalType (FlagConditional cond) = cond


--The conditionals possible conditionals that cabal can ask of the platform or the configuration flags
type PlatformConditional = ConditionalTree (Either PlatformBasicConditional FlagBasicConditional)
--Newtype container for a platform conditional
newtype PlatformConditionalType = PlatformConditional PlatformConditional deriving(Eq,Show,Read)
wrapPlatformConditionalType cond = (PlatformConditional cond)
unwrapPlatformConditionalType (PlatformConditional cond) = cond


