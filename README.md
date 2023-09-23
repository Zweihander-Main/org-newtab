<div align="center">
	<img src="./assets/icon-300x300-min.png" alt="Org-NewTab Logo" width="300" />

# Org-NewTab

_Supercharge your browser's New Tab with Org-Agenda_

🗃️ Org-NewTab is a browser extension which sets the org-agenda task you should be working on as your new tab page.

https://github.com/Zweihander-Main/org-newtab/assets/1928813/d638a915-2ca1-4b5b-9de2-0af16b902f24

![GitHub Repo stars](https://img.shields.io/github/stars/Zweihander-Main/org-newtab?style=for-the-badge&color=ae5a95)
![GitHub issues](https://img.shields.io/github/issues/Zweihander-Main/org-newtab?style=for-the-badge&color=ae5a95)
![GitHub pull requests](https://img.shields.io/github/issues-pr/Zweihander-Main/org-newtab?style=for-the-badge&color=ae5a95)

**Current Status:** Functional, usable, little messy but getting close to a first release

</div>

## 🚀 Getting Started

### Installation:

Until the extension is available from all the repositories and stores, you'll have to set it up manually:

1. Add the lisp files into your Emacs. Sample Doom config:

```elisp
;; packages.el
(package! org-newtab
  :recipe '(:host github :repo "Zweihander-Main/org-newtab"
            :files ("lisp/org-newtab*.el" "LICENSE")))
;; config.el
(use-package! org-newtab-mode)
```

2. Clone this repo and run `pnpm run build` for Chrome, `pnpm run build --target=firefox-mv3` for Firefox.
3. In Chrome, head to `chrome://extensions/`, enabled Developer Mode, and `Load unpacked` the `./build/chrome-mv3-prod` directory.
   In Firefox, head to `about:debugging#/runtime/this-firefox` and `Load Temporary Add-on` the `./build/firefox-mv3-prod` directory. Note that you'll have to do this every time you restart Firefox until this is on the extension store (soon hopefully!). Alternatively, follow the `web-ext` instructions [from here](https://stackoverflow.com/questions/62237202/firefox-add-ons-how-to-install-my-own-local-add-on-extension-permanently-in-f).
4. `M-x org-newtab-mode`

### Usage:

To control what shows up when a task isn't clocked in, hit the menu button in the top left and change the `Match Query` under the `Behavior` tab. Instructions for creating match queries [can be found here](https://orgmode.org/manual/Matching-tags-and-properties.html).

The background of the item that shows up is controlled by the `org-tag-faces` variable and which tags apply to the item. For now, the assumption is that only one colored tag will be applied to each item.

## Dev Notes

### Technologies used:

-   Emacs:
    -   [Eldev](https://github.com/emacs-eldev/eldev) for project management
    -   [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup/) for testing
    -   [websocket](https://github.com/ahyatt/emacs-websocket) and [async](https://github.com/jwiegley/emacs-async) libs
-   [Plasmo](https://www.plasmo.com/) Browser Extension framework
    -   React
    -   TypeScript
    -   Playwright for E2E integration testing
    -   Jest+Runners, Husky+Lint-staged
-   pnpm, eslint, prettier, stylelint

### Possible Future Additions

-   [x] Browser storage
-   [x] Persistent default match query
-   [x] Layout editor
-   [x] Display clock status in browser
-   [x] Hooks on Emacs actions (TODO change, clock change, ect.)
-   [ ] Clock in and out from browser
-   [ ] Mark items as done from browser
-   [ ] Smart caching for match query lookups
-   [ ] Sortable layout elements
-   [ ] Allow moving layout elements left/right
-   [ ] Display schedule/deadline info
-   [ ] Display priority
-   [ ] Match query builder
-   [ ] Multiple match queries
-   [ ] Render heading links
-   [ ] Open heading links
-   [ ] Vim keybindings
-   [ ] Age countdown
-   [ ] Icons for connection status
-   [ ] Deeper agenda integration
-   [ ] Multiple themes
-   [ ] Built-out documentation (README or separate site)
-   [ ] Provide sorting beyond what match query provides
-   [ ] Firefox compatibility
-   [ ] In-app tutorial/getting started

### Misc

-   `chrome.storage.local.get(console.log)` to get extension storage
-   Development is done against Chrome -- there is some flaky behavior when using `pnpm run dev --target=firefox-mv3` though a production build is checked against for every release

## Available for Hire

I'm available for freelance, contracts, and consulting both remotely and in the Hudson Valley, NY (USA) area. [Some more about me](https://www.zweisolutions.com/about.html) and [what I can do for you](https://www.zweisolutions.com/services.html).

Feel free to drop me a message at:

```
hi [a+] zweisolutions {●} com
```

## License

[AGPLv3](./LICENSE)

    org-newtab
    Copyright (C) 2023 Zweihänder

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
