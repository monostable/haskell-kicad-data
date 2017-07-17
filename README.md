# Haskell KiCAD Data
[![Build Status](https://travis-ci.org/monostable/haskell-kicad-data.svg?branch=master)](https://travis-ci.org/kasbah/haskell-kicad-data)

Parse and write [KiCAD](http://kicad-pcb.org) data (currently .kicad_mod files only).

This library is tested with QuickCheck to ensure it can parse whatever it outputs. 
The parser is also regularily checked against over 19,000 kicad_mod files currently part of [monostable/kicad_footprints](https://github.com/monostable/kicad_footprints). The resulting output of these parsed files is then checked with KiCAD scripting to make sure KiCAD can load it.

# Usage

    $ cabal repl

```
*Data.Kicad.PcbnewExpr> let pad = parse "(pad 1 smd rect (size 1 1) (at 1 1) (layers F.Cu))"
*Data.Kicad.PcbnewExpr> pad
Right (PcbnewExprItem (PcbnewPad {padNumber = "1", padType = SMD, padShape =
Rect, itemAt = PcbnewAtT {pcbnewAtPoint = (1.0,1.0), pcbnewAtOrientation =
0.0}, itemSize = (1.0,1.0), padLayers = [FCu], padAttributes_ = []}))
*Data.Kicad.PcbnewExpr> fmap write pad
Right "(pad \"1\" smd rect (at 1 1) (size 1 1) (layers F.Cu))"
```


See [docs on Hackage](https://hackage.haskell.org/package/kicad-data).
