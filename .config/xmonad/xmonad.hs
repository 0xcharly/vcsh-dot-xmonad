import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
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
  [ NS "brave" spawnBraveBrowserScratchpad findBraveBrowserScratchpad bravePositionScratchpad -- Brave.
  , NS "mail" spawnMailScratchpad findMailScratchpad positionScratchpad -- Google Mail.
  , NS "term" spawnTermScratchpad findTermScratchpad positionScratchpad -- Kitty.
  , NS "calendar" spawnCalendarScratchpad findCalendarScratchpad positionScratchpad -- Google Calendar.
  , NS "chat" spawnChatScratchpad findChatScratchpad positionScratchpad -- Google Chat.
  ] where
    spawnBraveBrowserScratchpad = "~/.local/bin/Brave.AppImage --force-device-scale-factor=1"
    findBraveBrowserScratchpad = className =? "Brave-browser"
    spawnMailScratchpad = "/opt/google/chrome/google-chrome --profile-directory='Profile 1' --app-id=fmgjjmmmlfnkbppncabfkddbjimcfncm"
    findMailScratchpad = resource =? "crx_fmgjjmmmlfnkbppncabfkddbjimcfncm"
    spawnTermScratchpad = "kitty -1 --title kitty-scratchpad"
    findTermScratchpad = title =? "kitty-scratchpad"
    spawnCalendarScratchpad = "/opt/google/chrome/google-chrome --profile-directory='Profile 1' --app-id=kjbdgfilnfhdoflbpgamdcdgpehopbep"
    findCalendarScratchpad = resource =? "crx_kjbdgfilnfhdoflbpgamdcdgpehopbep"
    spawnChatScratchpad = "/opt/google/chrome/google-chrome --profile-directory='Profile 1' --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
    findChatScratchpad = resource =? "crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi"
    positionScratchpad = customFloating $ S.RationalRect (1/3) (1/9) (1/3) (7/9)
    bravePositionScratchpad = customFloating $ S.RationalRect (1/4) (1/9) (2/4) (7/9)

delayLayoutHook = threeColumns ||| noBorders Full
  where
    threeColumns = smartSpacingWithEdge gap $ fullscreenFull $ ThreeColMid nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2
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
  -- , ("M-0", windows $ S.greedyView "private:delay")
  -- , ("M-S-0", windows $ S.shift "private:delay")
  , ("M-<Left>",  prevWS')
  , ("M-<Right>",  nextWS')
  , ("M-S-<Left>", shiftToPrev >> prevWS')
  , ("M-S-<Right>", shiftToNext >> nextWS')
  , ("M-f", withFocused $ toggleFloat)
  , ("M-p", spawn "$HOME/.local/bin/rofi -no-config -no-lazy-grab -show drun -modi drun -theme ~/.config/rofi/launcher.rasi")
  , ("M-S-p", spawn "ROFI_PLUGIN_PATH=$HOME/.local/usr/lib/rofi $HOME/.local/bin/rofi -show calc -modi calc -no-show-match -no-sort -theme ~/.config/rofi/launcher.rasi")
  , ("M-1", namedScratchpadAction delayScratchpads "brave")
  , ("M-2", namedScratchpadAction delayScratchpads "mail")
  , ("M-3", namedScratchpadAction delayScratchpads "term")
  , ("M-4", namedScratchpadAction delayScratchpads "calendar")
  , ("M-5", namedScratchpadAction delayScratchpads "chat")
  ] ++
  [ ("M-M4-" ++ [key], windows $ S.greedyView tag) | (tag, key) <- zip delayWorkspaces "12345"
  ]

-- keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
--   [ ((modMask, key), windows $ S.greedyView tag) | (tag, key) <- zip (XMonad.workspaces conf) [xK_6 .. xK_9]
--   -- quit, or restart
--   , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
--   , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
--   ]
--
main :: IO()
main = xmonad
  $ withUrgencyHook NoUrgencyHook
  $ ewmhFullscreen . ewmh
  $ withSB delayPolybarConfig
  $ docks
  $ def
    { modMask = mod1Mask  -- Rebind Mod to the Super key.
    , terminal = "kitty -1"
    , borderWidth = 2
    , normalBorderColor  = "#21252b"
    , focusedBorderColor = "#c678dd"
    , workspaces = delayWorkspaces
    -- , XMonad.keys = Main.keys
    , handleEventHook = handleEventHook def
    , layoutHook = desktopLayoutModifiers $ delayLayoutHook
    , logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ def
    , manageHook = manageDocks <+> namedScratchpadManageHook delayScratchpads <+> delayManageHook <+> manageHook def
    , startupHook = startupHook def
    }
    `additionalKeysP` delayKeys
    `additionalMouseBindings`
    [ ((mod1Mask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((mod1Mask .|. shiftMask, button2), (\w -> focus w >> mouseResizeWindow w))
    ]
