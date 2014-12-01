# Haskell KiCad Data

Parse and write KiCad data (currently .kicad_mod files only).

    $ cabal repl

    *Data.Kicad.PcbnewExpr> let pad = parse "(pad 1 smd rect (size 1.0 1.0) (at 1.0 1.0))"
    *Data.Kicad.PcbnewExpr> pad
    Right (PcbnewExprItem (PcbnewPad {padNumber = "1", padType = SMD, padShape
    = Rect, itemAt = PcbnewAtT {pcbnewAtPoint = (1.0,1.0), pcbnewAtOrientation =
    0.0}, itemSize = (1.0,1.0), padLayers = [], padAttributes_ = []}))
    *Data.Kicad.PcbnewExpr> fmap pretty pad
    Right (pad "1" smd rect (at 1 1) (size 1 1) (layers))


See [docs on Hackage](https://hackage.haskell.org/package/kicad-data).
