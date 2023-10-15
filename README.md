<div align="center">
	<img src="./assets/icon-300x300-min.png" alt="Org-NewTab Logo" width="300" />

# Org-NewTab

_Supercharge your browser's New Tab with Org-Agenda_

🗃️ Org-NewTab is a browser extension which sets the org-agenda task you should be working on as your new tab page.

https://github.com/Zweihander-Main/org-newtab/assets/1928813/79b44915-2a1a-42af-828e-f31105c2e5be

<p align="center">
	<a href="https://chrome.google.com/webstore/detail/org-newtab/ojpofmnbleffgacihnocmcaefbmehehj"><img src="https://user-images.githubusercontent.com/585534/107280622-91a8ea80-6a26-11eb-8d07-77c548b28665.png" alt="Get Org-NewTab for Chromium"></a>
	<a href="https://addons.mozilla.org/en-US/firefox/addon/org-newtab/"><img src="https://user-images.githubusercontent.com/585534/107280546-7b9b2a00-6a26-11eb-8f9f-f95932f4bfec.png" alt="Get Org-NewTab for Firefox"></a>
	<a href="https://melpa.org/#/org-newtab"><img src="https://raw.githubusercontent.com/melpa/melpa/master/html/logo.svg" height="64" alt="Get Org-NewTab on Melpa"></a>
</p>

![GitHub Repo stars](https://img.shields.io/github/stars/Zweihander-Main/org-newtab?style=for-the-badge&color=ae5a95)
![GitHub issues](https://img.shields.io/github/issues/Zweihander-Main/org-newtab?style=for-the-badge&color=ae5a95)
![GitHub pull requests](https://img.shields.io/github/issues-pr/Zweihander-Main/org-newtab?style=for-the-badge&color=ae5a95)

**Current Status:** Functional, installation still very dodgy/difficult, getting close to a first release

</div>

## 🚀 Getting Started

### Installation:

1. Install package from MELPA
 <details>
 <summary>Using `package.el`</summary>

You can install `org-newtab` from [MELPA](https://melpa.org/) or [MELPA
Stable](https://stable.melpa.org/) using `package.el`:

```
M-x package-install RET org-newtab RET
```

</details>

<details>
<summary>Using `straight.el`</summary>

Installation from MELPA or MELPA Stable using `straight.el`:

```emacs-lisp
(straight-use-package 'org-newtab)
```

Or with `use-package`:

```emacs-lisp
(use-package org-newtab
  :straight t
  ...)
```

If you need to install the package directly from the source repository, instead
of from MELPA, the next sample shows how to do so:

```emacs-lisp
(use-package org-newtab
  :straight (:host github :repo "Zweihander-Main/org-newtab"
             :files (:defaults))
  ...)
```

If you plan to use your own local fork for the development and contribution, the
next sample will get you there:

```emacs-lisp
(use-package org-newtab
  :straight (:local-repo "/path/to/org-newtab-fork"
             :files (:defaults)
             :build (:not compile))
  ...)
```

</details>

<details>
<summary>Using Doom Emacs</summary>

```emacs-lisp
(package! org-newtab)
```

With the next sample you can install the package directly from the source
repository:

```emacs-lisp
(package! org-newtab
  :recipe (:host github :repo "Zweihander-Main/org-newtab"
           :files (:defaults)))
```

And if you plan to use your own local fork for the development or contribution,
the next sample will get you there:

```emacs-lisp
(package! org-newtab
  :recipe (:local-repo "/path/to/org-newtab-fork"
           :files (:defaults)
           :build (:not compile)))
```

</details>

<details>
<summary>Without a package manager</summary>

Uou will need to ensure that you have all the required dependencies. These include:

-   websocket
-   async

After installing the package, you will need to properly setup `load-path` to the
package:

```emacs-lisp
(add-to-list 'load-path "/path/to/org-newtab/lisp/")
```

After which you should be able to resolve `(require 'org-newtab)` call without any
problems.

</details>

2. Install [Chrome](https://chrome.google.com/webstore/detail/org-newtab/ojpofmnbleffgacihnocmcaefbmehehj) or [Firefox](https://addons.mozilla.org/en-US/firefox/addon/org-newtab/) extension
3. `M-x org-newtab-mode`

### Usage:

To control what shows up when a task isn't clocked in, hit the menu button in the top left and change the `Match Query` under the `Behavior` tab. Instructions for creating match queries [can be found here](https://orgmode.org/manual/Matching-tags-and-properties.html).

The background of the item that shows up is controlled by the `org-tag-faces` variable and which tags apply to the item. For now, the assumption is that only one colored tag will be applied to each item.

### Development setup:

1. Add the lisp files into your Emacs as outlined above.
2. Clone this repo and run `pnpm run build` for Chrome, `pnpm run build --target=firefox-mv3` for Firefox.
3. In Chrome, head to `chrome://extensions/`, enabled Developer Mode, and `Load unpacked` the `./build/chrome-mv3-prod` directory.
   In Firefox, head to `about:debugging#/runtime/this-firefox` and `Load Temporary Add-on` the `./build/firefox-mv3-prod` directory. Note that you'll have to do this every time you restart Firefox until this is on the extension store (soon hopefully!). Alternatively, follow the `web-ext` instructions [from here](https://stackoverflow.com/questions/62237202/firefox-add-ons-how-to-install-my-own-local-add-on-extension-permanently-in-f).
4. `M-x org-newtab-mode`

## 🖥️ Dev Notes

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
-   [x] Firefox compatibility
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
-   [ ] Built-out documentation (separate site)
-   [ ] Provide sorting beyond what match query provides
-   [ ] In-app tutorial/getting started

### Misc

-   `chrome.storage.local.get(console.log)` to get extension storage
-   Development is done against Chrome -- there is some flaky behavior when using `pnpm run dev --target=firefox-mv3` though a production build is checked against for every release

## 💼 Available for Hire

I'm available for freelance, contracts, and consulting both remotely and in the Hudson Valley, NY (USA) area. [Some more about me](https://www.zweisolutions.com/about.html) and [what I can do for you](https://www.zweisolutions.com/services.html).

Feel free to drop me a message at:

```
hi [a+] zweisolutions {●} com
```

## ⚖️ License

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
