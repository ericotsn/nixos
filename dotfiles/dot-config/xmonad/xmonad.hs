import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import System.Exit

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh $ myConfig

myConfig = def
    { modMask    = mod4Mask
    , terminal   = "alacritty"
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    }
  `additionalKeysP`
    [ ("M-<Return>", spawn "emacsclient -c"                   )
    , ("M-S-e"     , io exitSuccess                           )
    , ("M-S-r"     , spawn "xrandr --output Virtual-1 --auto" )
    ]

myLayoutHook = smartBorders $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook = composeAll
    [ className =? "1Password" --> doCenterFloat
    , isDialog                 --> doCenterFloat
    ]
