{- package.yaml
name:                more-math
version:             0.1.0.0
synopsis:            More Math for Haskell (Big integers, fractions, etc).
category:            Math
author:              dmitmel
maintainer:          dmytro.meleshko@gmail.com
license:             Apache-2.0
github:              dmitmel/more-math
extra-source-files:
    - ChangeLog.md
dependencies:
    - base >=4.9 && <4.10

library:
    source-dirs:     src
    ghc-options:     -Wall
    exposed-modules:
        - Math.MoreMath
        - Math.MoreMath.BigInt
        - Math.MoreMath.Fraction
        - Math.MoreMath.Color
-}

{- cabalw
#!/bin/sh

# Wrapper for Cabal executable
# Recompiles hpack's configuration file (package.yaml) if any changes detected

CHECKSUM_FILE=".package.yaml.checksum.txt"

prevHash="$([ -f "$CHECKSUM_FILE" ] && cat "$CHECKSUM_FILE")"
thisHash="$(sha1sum package.yaml)"

if [ "$prevHash" != "$thisHash" ]; then
    echo "Detected changes in package.yaml"
    echo "$thisHash" > $CHECKSUM_FILE
    echo "Aligning package.yaml"
    align package.yaml 1> /dev/null
    echo "Generating more-math.cabal"
    hpack --silent
    echo
fi

cabal $@
-}

{- listing.txt
Permissions  Size User    Group Date Modified Name
drwxr-xr-x      - dmitmel staff 31 Mar  2017  ./
drwxr-xr-x      - dmitmel staff 15 Dec  2016  ├── src/
drwxr-xr-x      - dmitmel staff 31 Mar  2017  │  └── Math/
drwxr-xr-x      - dmitmel staff 31 Mar  2017  │     ├── MoreMath/
.rw-r--r--  4,9Ki dmitmel staff 11 Jan  2017  │     │  ├── BigInt.hs
.rw-r--r--   1020 dmitmel staff 31 Mar  2017  │     │  ├── Color.hs
.rw-r--r--   1005 dmitmel staff 12 Jan  2017  │     │  └── Fraction.hs
.rw-r--r--    170 dmitmel staff 12 Jan  2017  │     └── MoreMath.hs
.rwxr-xr-x    534 dmitmel staff 11 Jan  2017  ├── cabalw*
.rw-r--r--    115 dmitmel staff 14 Dec  2016  ├── ChangeLog.md
.rw-r--r--   11Ki dmitmel staff 14 Dec  2016  ├── LICENSE
.rw-r--r--    957 dmitmel staff 31 Mar  2017  ├── more-math.cabal
.rw-r--r--    588 dmitmel staff 31 Mar  2017  ├── package.yaml
.rw-r--r--     46 dmitmel staff 14 Dec  2016  ├── Setup.hs
.rw-r--r--  2,2Ki dmitmel staff 11 Jan  2017  └── stack.yaml
-}

module Math.MoreMath
( module Math.MoreMath.BigInt
, module Math.MoreMath.Fraction
) where

import           Math.MoreMath.BigInt
import           Math.MoreMath.Fraction
