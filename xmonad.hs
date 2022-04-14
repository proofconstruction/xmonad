-- |-----------------------------------------------------------------------------
-- | Module declaration

module Main (main) where



-- |-----------------------------------------------------------------------------
-- | Imports

import System.Exit
import System.IO

import Data.Monoid
import Control.Monad

import XMonad
import XMonad.Config.Desktop

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.InsertPosition

-- Actions
import XMonad.Actions.DynamicProjects
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Navigation2D
import XMonad.Actions.CycleWS

-- Layouts
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.Column
import XMonad.Layout.Fullscreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Named
import XMonad.Layout.IfMax
import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutModifier (ModifiedLayout)


-- Prompt
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Font
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

-- StackSet
import XMonad.StackSet as W



-- |-----------------------------------------------------------------------------
-- | MAIN

main :: IO ()
main = do
  xmonad
    . docks
    . ewmh
    =<<  myStatusBar (myConfig `additionalKeysP` myKeybindings)



-- |-----------------------------------------------------------------------------
-- | CONFIG

myConfig = def {
  --  Basic stuff
  terminal           = shell,
  focusFollowsMouse  = True,
  modMask            = mod4Mask,

  -- Theming
  normalBorderColor  = myBorderColor,
  focusedBorderColor = myFocusedBorderColor,
  borderWidth        = myBorderWidth,

  -- hooks, layouts
  layoutHook         = myLayoutHook,
  manageHook         = myManageHook,
  handleEventHook    = ewmhDesktopsEventHook,
  logHook            = updatePointer (0.95,0.95) (0,0),
  startupHook        = ewmhDesktopsStartup
                       >> setWMName "XMonad"

                       -- Launch startup programs by alias here
                       >> spawnOnce cursor
                       >> spawnOnce wallpaper
                       >> spawnOnce inputMethod
  }



-- |-----------------------------------------------------------------------------
-- | LAYOUT
-- |
-- | This layout configuration uses 4 primary layouts: 'ThreeColMid' (suitable
-- | for ultrawide displays), 'ResizableTall', 'BinarySpacePartition', and 'Grid'.
-- | Portrait-mode variants of 'tall' and 'threeColumn' are also provided.
-- |
-- | ThreeColMid and ResizableTall both come with mirrored, reflected variants
-- | which put the master pane at the bottom of the screen (in ThreeColMid this
-- | is only true for the 2-window case), suitable for ultrawide displays
-- | rotated to a portrait orientation.
-- |
-- | You can also use the 'M-<Esc>' key binding to toggle
-- | between the current layout and a fullscreen layout.

myLayoutHook = smartBorders . avoidStruts $ desktopLayoutModifiers $ toggleLayouts (noBorders Full) myLayouts
  where
    myLayouts =
          tall
      ||| threeColumn
      ||| bsp
      ||| grid

full = named "Fullscreen"
       $ noBorders (fullscreenFull Full)

tall = named "Tall"
       $ reflectHoriz
       $ IfMax 1 full
       $ ResizableTall 1 (1/100) (3/5) []

vertTall = named "VertTall"
           $ Mirror tall

threeColumn = named "ThreeCol"
              $ reflectHoriz
              $ IfMax 1 full
              $ (ThreeColMid 1 (3/100) (1/2))

vertThreeColumn = named "VertThreeCol"
                  $ Mirror threeColumn

bsp = named "Binary"
      $ IfMax 1 full
      $ reflectVert
      $ Mirror emptyBSP

grid = named "Grid"
       $ reflectVert
       $ Grid



-- |-----------------------------------------------------------------------------
-- | PROMPT

myPrompt :: XPConfig
myPrompt = def
   { position          = Bottom
   , alwaysHighlight   = True
   , fgColor           = myPromptFgColor
   , bgColor           = myPromptBgColor
   , font              = xmobarFont
   , promptBorderWidth = 0
   , height            = 30
   , defaultText       = " "
   , historySize       = 5
   , maxComplRows      = Just 1
   }



-- |-----------------------------------------------------------------------------
-- | SCRATCHPAD

myScratchpad :: ManageHook
myScratchpad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.2 -- height is 20% of the screen height
    w = 1.0 -- scratchpad should be the width of the screen it appears on
    t = 1.0-h -- the top of the scratchpad should be 80% of the way down the screen
    l = 1.0-w -- distance for the left border from the left edge of the screen



-- |-----------------------------------------------------------------------------
-- | XMOBAR

myStatusBar = statusBar "xmobar" myPP strutsToggle
  where
    myPP = def
      { ppCurrent = xmobarColor myCurrentColor ""
      , ppVisible = xmobarColor myVisibleColor ""
      , ppHidden = xmobarColor myHiddenColor ""
      , ppHiddenNoWindows = xmobarColor myEmptyColor ""
      , ppUrgent = xmobarColor myUrgentColor "" . xmobarStrip
      , ppLayout = xmobarColor myLayoutColor ""
      , ppWsSep = "  "
      , ppSep = xmobarColor mySepColor "" "   |   "
      , ppTitle = xmobarColor myTitleColor "" . shorten 120 . trim
      }
    strutsToggle XConfig {modMask = modm} = (modm, xK_b)



-- |-----------------------------------------------------------------------------
-- | WINDOW BEHAVIOR
-- |
-- | Use the "xprop WM_CLASS" tool to get strings for the className matches.

myManageHook :: ManageHook
myManageHook = composeOne
  [ isFullscreen                          -?> doFullFloat
  , isDialog                              -?> doCenterFloat
  , isDialog <&&> className =? "Firefox"  -?> doCenterFloat
  , isInProperty
      "_NET_WM_WINDOW_TYPE"
      "_NET_WM_WINDOW_TYPE_SPLASH"
    -?> doCenterFloat
  , transience
  ]
  <+> manageDocks
  <+> myScratchpad



-- |-----------------------------------------------------------------------------
-- | KEYBINDINGS

myKeybindings :: [(String, X ())]
myKeybindings =
  [
  -- Layouts
    ("M-<Space>"     , sendMessage NextLayout)
  , ("M-p"           , spawn launcher)
  , ("M-`"           , scratchpadSpawnActionCustom scratchpad)
  , ("M-<Esc>"       , sendMessage (Toggle "Full"))
  , ("M-h"           , sendMessage Expand)
  , ("M-l"           , sendMessage Shrink)

  -- Applications
  , ("M-S-e"         , spawn editor)
  , ("M-S-f"         , spawn browser)
  , ("M-S-<Return>"  , spawn shell)
  , ("M-S-r)"        , spawn element)

  -- Prompt
  , ("M-S-p"         , shellPrompt myPrompt)

  -- Session
  , ("M-S-q"         , confirmPrompt myPrompt "exit" (io exitSuccess))
  , ("M-S-l"         , spawn suspend)
  , ("M-q"           , broadcastMessage ReleaseResources
                       >> restart "xmonad" True)
  ]



-- |-----------------------------------------------------------------------------
-- | ALIASES

cursor :: String
cursor = "xsetroot -cursor_name left_ptr"

inputMethod :: String
inputMethod = "fcitx"

editor :: String
editor = "emacsclient -nc"

browser :: String
browser = "firefox"

launcher :: String
launcher = "rofi -combi-modi run,drun -show combi -modi combi"

wallpaper :: String
wallpaper = "/home/alex/.nix-profile/bin/feh --no-fehbg --bg-center '/home/alex/circle333333.png'"

element :: String
element = "element-desktop"

suspend :: String
suspend = "systemctl suspend"

shell :: String
shell = "terminator"

scratchpad :: String
scratchpad = "urxvt -name scratchpad"


-- |-----------------------------------------------------------------------------
-- | Theme

-- Font definitions
fontSize :: String
fontSize = "10"

sansSerif :: String
sansSerif = "xft:Fira Sans:size=" ++ fontSize

monoSpace :: String
monoSpace = "xft:Fira Sans Mono:antialias=true:size=" ++ fontSize

xmobarFontSize :: String
xmobarFontSize = "10"

xmobarFont :: String
xmobarFont = "xft:Fira Sans Mono:antialias=true:size=" ++ xmobarFontSize ++ ":antialias=true"

-- Color definitions
myFont                  = xmobarFont
myPromptBgColor         = "#2e3440" -- nord0
myPromptFgColor         = "#eceff4" -- nord6
myBorderColor           = "#40464b" -- dark gray
myFocusedBorderColor    = "#839cad" -- light gray
myCurrentColor          = "#bf616a" -- nord11
myEmptyColor            = "#4c4c4c" -- dark gray but lighter than xmobar bg
myHiddenColor           = "#8fbcbb" -- nord7
myLayoutColor           = "#5e81ac" -- nord10
myUrgentColor           = "#bf616a" -- nord11
myTitleColor            = "#eceff4" -- nord6
mySepColor              = "#81a1c1" -- nord10
myVisibleColor          = "#ebcb8b" -- nord13

-- Window borders
myBorderWidth = 2
