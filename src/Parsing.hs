module Parsing where

import Distribution.Package
import Distribution.PackageDescription.Parse
import qualified Distribution.PackageDescription as PD

import Package
import Conditional

--Use cabal parsing to get a generic package and then form a package global
--Package parsing function
--Throw away errors and warnings and turn into a maybe
parsePackage :: String -> Maybe GlobalPackage
parsePackage string = fmap fromGenericPackage parseMaybe
  where parseResult = parsePackageDescription string
        parseMaybe = case parseResult of
                        (ParseFailed _) -> Nothing
                        (ParseOk _ genericPackage) -> Just genericPackage

--Convert a generic package to a global package
fromGenericPackage :: PD.GenericPackageDescription -> GlobalPackage
fromGenericPackage genericPackage = Package {
                                        globalProperties = toGlobalPackageData (PD.packageDescription genericPackage),
                                        flags = (PD.genPackageFlags genericPackage),
                                        library = fmap toLibrary $ PD.condLibrary genericPackage,
                                        executables = map toExecutable $ PD.condExecutables genericPackage,
                                        tests = map toTest $ PD.condTestSuites genericPackage,
                                        benchmarks = map toBenchmark $ PD.condBenchmarks genericPackage
                                    }

--Convert a package description to global properties
--Map null entries to nothing.
toGlobalPackageData :: PD.PackageDescription -> GlobalPackageData
toGlobalPackageData packageDescription = GlobalPackageData {
        name = pkgName (PD.package packageDescription),
        version = pkgVersion (PD.package packageDescription),
        synopsis = maybeNonNull (PD.synopsis packageDescription),
        description = maybeNonNull (PD.description packageDescription),
        category = maybeNonNull (PD.category packageDescription),
        stability = maybeNonNull (PD.stability packageDescription),
        license = show (PD.license packageDescription),
        copyright = maybeNonNull (PD.copyright packageDescription),
        homepage = maybeNonNull (PD.homepage packageDescription),
        author = maybeNonNull (PD.author packageDescription),
        maintainer = maybeNonNull (PD.maintainer packageDescription),
        bugReports = maybeNonNull (PD.bugReports packageDescription),
        packageUrl = maybeNonNull (PD.pkgUrl packageDescription)
}
   where maybeNonNull string | string /= "" = Just string
                             | otherwise = Nothing

--Parse conditional tree
--Take the internal parsing tree and some property (list) and 
-- construct the list of conditional, property pairs
parseConditionalDependency :: PD.CondTree PD.ConfVar [Dependency] a -> [(PlatformConditional, Dependency)]
parseConditionalDependency condNode = fixedConditonalDependencies ++ subComponetsDependencies
   where fixedDependencies = PD.condTreeConstraints condNode
         fixedConditonalDependencies = [(Logic True, dependence) | dependence <- fixedDependencies]
         subComponents = PD.condTreeComponents condNode
         subComponetsDependencies = concat $ map liftUpTree subComponents
         --Take the pair of a conditional and an if / if else cond tree and map condtionals to lists of dependencies
         --If branch
         liftUpTree (cond, subTreeIf, Nothing) = ifConditionalDependencies
           where conditional = toConditional cond
                 --Parse the if sub tree then the newDependencies are the same with the additional condition condition.
                 ifDependencies = parseConditionalDependency subTreeIf
                 ifConditionalDependencies = [(And conditional subConditional, dependence) | (subConditional, dependence) <- ifDependencies]
         liftUpTree (cond, subTreeIf, Just subTreeElse) = ifConditionalDependencies ++ elseConditionalDependencies
           where conditional = toConditional cond
                 --Parse the if sub tree then the newDependencies are the same with the additional condition condition.
                 ifDependencies = parseConditionalDependency subTreeIf
                 ifConditionalDependencies = [(And conditional subConditional, dependence) | (subConditional, dependence) <- ifDependencies]
                 --Parse the else sub tree then the newDependencies are the same with the additional condition that condition if false.
                 elseDependencies = parseConditionalDependency subTreeIf
                 elseConditionalDependencies = [(And (Not conditional) subConditional, dependence) | (subConditional, dependence) <- elseDependencies]

--Convert the cabal conditon into the platform conditional tree
toConditional :: PD.Condition PD.ConfVar -> PlatformConditional
--Convert variables over
toConditional (PD.Var var) = case var of
                                PD.OS os -> Var . Left $ OperatingSystem os
                                PD.Arch arch -> Var . Left $ Architecture arch
                                PD.Impl compiler version -> Var . Left . Implimentaton $ CompilerVersion compiler version
                                PD.Flag flagName -> Var . Right . Flag $ show flagName
--Convert logical over
toConditional (PD.Lit lit) = Logic lit
--Convert combinators over
toConditional (PD.CNot cond) = Not $ toConditional cond
toConditional (PD.CAnd cond1 cond2) = And (toConditional cond1) (toConditional cond2)
toConditional (PD.COr cond1 cond2) = Or (toConditional cond1) (toConditional cond2)

--Convert a conditional library in cabal into a Library with Platform conditional
--Convert to a list of condition dependency pairs
toLibrary :: PD.CondTree PD.ConfVar [Dependency] PD.Library -> Library PlatformConditional
toLibrary condTree = Library {
  libraryBuildDependencies = parseConditionalDependency condTree
}

--Convert a conditional executable in cabal into a Executable with Platform conditional
--Convert to a list of condition dependency pairs
toExecutable :: (String,PD.CondTree PD.ConfVar [Dependency] PD.Executable) -> Executable PlatformConditional
toExecutable (name,condTree) = Executable {
  executableTargetName = name,
  executableBuildDependencies = parseConditionalDependency condTree
}

--Convert a conditional test in cabal into a Test with Platform conditional
--Convert to a list of condition dependency pairs
toTest :: (String, PD.CondTree PD.ConfVar [Dependency] PD.TestSuite) -> TestSuite PlatformConditional
toTest (name,condTree) = TestSuite {
  testTargetName = name,
  testBuildDependencies = parseConditionalDependency condTree
}

--Convert a conditional benchmark in cabal into a Benchmark with Platform conditional
--Convert to a list of condition dependency pairs
toBenchmark :: (String, PD.CondTree PD.ConfVar [Dependency] PD.Benchmark) -> Benchmark PlatformConditional
toBenchmark (name,condTree) = Benchmark {
  benchmarkTargetName = name,
  benchmarkBuildDependencies = parseConditionalDependency condTree
}

