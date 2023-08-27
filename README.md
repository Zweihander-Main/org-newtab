# Org-NewTab

> 🗃️ Supercharge your browser's New Tab with Org-Agenda

Org-NewTab is a Chrome extension which talks to your Emacs org-agenda and sets the task you should be working on as your new tab page.

## Dev Notes

### Technologies used:

-   Emacs:
    -   Buttercup for testing
    -   websocket and async
-   Plasmo Chrome Extension framework
    -   React
    -   TypeScript
    -   Playwright for E2E integration testing
    -   Jest+Runners, Husky+Lint-staged
-   pnpm, eslint, prettier, stylelint

### Possible Future Additions

-   [x] Browser storage
-   [x] Persistent default match query
-   [x] Layout editor
-   [ ] Sortable layout elements
-   [ ] Display priority and clock status in browser
-   [ ] Mark items as done from browser
-   [ ] Clock in and out from browser
-   [ ] Match query builder
-   [ ] Multiple match queries
-   [ ] Render heading links
-   [ ] Open heading links
-   [ ] Vim keybindings
-   [ ] Age countdown
-   [ ] Icons for connection status
-   [ ] Hooks on Emacs actions (TODO change, clock change, ect.)
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
