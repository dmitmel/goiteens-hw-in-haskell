module Math.MoreMath.Color where

data Color = Color { colorR :: Float, colorG :: Float, colorB :: Float } deriving (Show, Eq, Ord, Read)

colorAdd :: Color -> Color -> Color
(Color ar ag ab) `colorAdd` (Color br bg bb) = Color (ar + br) (ag + bg) (ab + bb)

colorSubt :: Color -> Color -> Color
(Color ar ag ab) `colorSubt` (Color br bg bb) = Color (ar - br) (ag - bg) (ab - bb)

colorMult :: Color -> Float -> Color
(Color r g b) `colorMult` n = Color (r * n) (g * n) (b * n)

colorDiv :: Color -> Float -> Color
(Color r g b) `colorDiv` n = Color (r / n) (g / n) (b / n)

colorLerp :: Float -> Color -> Color -> Color
colorLerp t (Color ar ag ab) (Color br bg bb) = Color (ar + (br - ar) * ct) (ag + (bg - ag) * ct) (ab + (bb - ab) * ct)
    where ct = max 0 (min t 1)

colorLerpUnclamped :: Float -> Color -> Color -> Color
colorLerpUnclamped t (Color ar ag ab) (Color br bg bb) = Color (ar + (br - ar) * t) (ag + (bg - ag) * t) (ab + (bb - ab) * t)

colorBlend :: Color -> Color -> Color
colorBlend = colorLerp 0.5
