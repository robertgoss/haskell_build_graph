module Parsing where

import Distribution.Package
import Distribution.PackageDescription.Parse
import qualified Distribution.PackageDescription as PD

import Package

--Use cabal parsing to get a generic package and then form a package global
--Package parsing function
--Throw away errors and warnings and turn into a maybe
parsePackage :: String -> Maybe PackageGlobal
parsePackage string = fmap fromGenericPackage parseMaybe
  where parseResult = parsePackageDescription string
        parseMaybe = case parseResult of
                        (ParseFailed _) -> Nothing
                        (ParseOk _ genericPackage) -> Just genericPackage

--Convert a generic package to a global package
fromGenericPackage :: PD.GenericPackageDescription -> PackageGlobal
fromGenericPackage genericPackage = Package {
                                        globalProperties = toGlobalPackageData (PD.packageDescription genericPackage)
                                    }

--Convert a package description to global properties
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