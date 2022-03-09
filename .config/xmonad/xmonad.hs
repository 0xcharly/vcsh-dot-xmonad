import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Ungrab
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as S

delayScratchpads =
  [ NS "brain" spawnBrainScratchpad findBrainScratchpad positionScratchpad -- Run Obsidian, find it by title.
  , NS "discord" spawnDiscordScratchpad findDiscordScratchpad positionScratchpad -- Run Discord, find it by title.
  , NS "pip" spawnPiP findPiP positionPiP -- Only work with existing Picture-in-picture window, find it by title.
  , NS "term" spawnTermScratchpad findTermScratchpad positionScratchpad -- Run Kitty, find it by title.
  , NS "tmux" spawnTmuxScratchpad findTmuxScratchpad positionScratchpad -- Run tmux in Kitty, find it by title.
  , NS "spotify" spawnSpotifyScratchpad findSpotifyScratchpad positionScratchpad -- Run Spotify, find it by title.
  ] where
    spawnBrainScratchpad = "obsidian --force-device-scale-factor=1"
    findBrainScratchpad = className =? "obsidian"
    spawnDiscordScratchpad = "discord"
    findDiscordScratchpad = className =? "discord"
    spawnPiP = ""
    findPiP = title =? "Picture in picture"
    positionPiP = customFloating $ S.RationalRect (1/5) (1/5) (3/5) (3/5)
    spawnTermScratchpad = "$HOME/.local/bin/kitty -1 --title kitty-scratchpad"
    findTermScratchpad = title =? "kitty-scratchpad"
    spawnTmuxScratchpad = "$HOME/.local/bin/kitty -1 --title tmux-scratchpad -e tmux new-session -A -s scratchpad"
    findTmuxScratchpad = title =? "tmux-scratchpad"
    spawnSpotifyScratchpad = "spotify"
    findSpotifyScratchpad = title =? "Spotify"
    positionScratchpad = customFloating $ S.RationalRect (1/5) (1/5) (3/5) (3/5)

delayLayoutHook = tiled ||| Mirror tiled ||| Full ||| threeColumns
  where
    threeColumns = spacing gap $ ThreeColMid nmaster delta ratio
    tiled = spacing gap $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
    gap = 8

delayWorkspaces =
  [ "1:mail"
  , "2:web"
  , "3:code"
  , "4:debug"
  , "private:delay"
  ]

-- Toggle floating on/off on a window.
toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (S.floating s)
          then S.sink w s
          else (S.float w (S.RationalRect (1/5) (1/5) (3/5) (3/5)) s)
    )

-- Returns a query which checks if the window has the given property.
hasProperty :: String -> Query Bool
hasProperty name = ask >>= \w -> liftX $ withDisplay $ queryFunc w
  where queryFunc window display = do 
          atom <- getAtom name

          prop8 <- io $ getWindowProperty8 display atom window
          prop16 <- io $ getWindowProperty16 display atom window
          prop32 <- io $ getWindowProperty32 display atom window

          --
          -- This is actually the opposite of the Maybe monad (I want to
          -- *continue* on Nothing), so I can't just use a monad here.
          --
          case prop8 of
            Just x  -> return True
            Nothing ->
              case prop16 of
                Just x  -> return True
                Nothing ->
                  case prop32 of
                    Just x  -> return True
                    Nothing -> return False

-- Whether a window is a splash screen.
-- The second criteria is used to match Jetbrains' IDE splash popup.
isSplash :: Query Bool
isSplash = 
  (isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH") <||>
  ((hasProperty "_MOTIF_WM_HINTS") <&&> (className =? "jetbrains-studio") <&&> (title =? "win0"))

-- Cycle through workspaces one by one but filtering out NSP (scratchpads).

nextWS' = switchWS Next
prevWS' = switchWS Prev

switchWS dir =
  findWorkspace filterOutNSP dir anyWS 1 >>= windows . S.view

filterOutNSP =
  let g f xs = filter (\(S.Workspace t _ _) -> t /= "NSP") (f xs)
  in  g <$> getSortByIndex

-- Polybar settings.

delayPolybarConfig = def
  { sbStartupHook = spawn "$HOME/.config/polybar/launch.sh"
  , sbCleanupHook = spawn "killall polybar"
  }

delayManageHook = aswbManageHooks
aswbManageHooks = composeAll
  [ className =? "Firefox" --> doShift "private:delay"
  , className =? "jetbrains-studio" --> doShift "3:code"
  , isSplash --> doIgnore
  ]

delayKeys =
  [ ("M-S-l", spawn "slock")
  , ("M-0", windows $ S.greedyView "private:delay")
  , ("M-S-0", windows $ S.shift "private:delay")
  , ("M-<Left>",  prevWS')
  , ("M-<Right>",  nextWS')
  , ("M-S-<Left>", shiftToPrev >> prevWS')
  , ("M-S-<Right>", shiftToNext >> nextWS')
  , ("M-f", withFocused $ toggleFloat)
  , ("M-p", spawn "$HOME/.local/bin/rofi -no-config -no-lazy-grab -show drun -modi drun -theme ~/.config/rofi/launcher.rasi")
  , ("M-c", spawn "ROFI_PLUGIN_PATH=$HOME/.local/usr/lib/rofi $HOME/.local/bin/rofi -show calc -modi calc -no-show-match -no-sort -theme ~/.config/rofi/launcher.rasi")
  , ("M-b", namedScratchpadAction delayScratchpads "brain")
  , ("M-d", namedScratchpadAction delayScratchpads "discord")
  , ("M-m", namedScratchpadAction delayScratchpads "pip")
  , ("M-t", namedScratchpadAction delayScratchpads "term")
  , ("M-s", namedScratchpadAction delayScratchpads "spotify")
  ] ++
  [ (otherModMasks ++ "M-" ++ [key], action tag)
  | (tag, key) <- zip delayWorkspaces "123456789"
  , (otherModMasks, action) <- [("", windows . S.view), ("S-", windows . S.shift)]
  ]

main :: IO()
main = xmonad
  $ withUrgencyHook NoUrgencyHook
  $ ewmhFullscreen . ewmh
  $ withSB delayPolybarConfig
  $ docks
  $ def
    { modMask = mod4Mask  -- Rebind Mod to the Super key.
    , terminal = "$HOME/.local/bin/kitty -1"
    , borderWidth = 1
    , normalBorderColor  = "#22212c" -- Dark gray.
    , focusedBorderColor = "#9580ff" -- Purple.
    , workspaces = delayWorkspaces
    , handleEventHook = handleEventHook def
    , layoutHook = desktopLayoutModifiers $ delayLayoutHook
    , logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ def
    , manageHook = manageDocks <+> namedScratchpadManageHook delayScratchpads <+> delayManageHook <+> manageHook def
    , startupHook = startupHook def
    }
    `additionalKeysP` delayKeys
    `additionalMouseBindings`
    [ ((mod4Mask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((mod4Mask, button2), (\w -> focus w >> mouseResizeWindow w))
    ]
