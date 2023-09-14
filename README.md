# Org-NewTab

> 🗃️ Supercharge your browser's New Tab with Org-Agenda

Org-NewTab is a browser extension which sets the org-agenda task you should be working on as your new tab page.

## Dev Notes

### Technologies used:

-   Emacs:
    -   Buttercup for testing
    -   websocket and async
-   Plasmo Browser Extension framework
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

### Misc

-   `chrome.storage.local.get(console.log)` to get extension storage

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
