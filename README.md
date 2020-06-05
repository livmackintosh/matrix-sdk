# Matrix Bot Haskell

Another 🤖 for the [[matrix]](https://matrix.org) protocol written in Haskell.

This is early alpha software and as such does not implement the full suite of
features that one might normally expect from such a project.

There are two parts to this project: the _bot_ and the (HTTP) _API_ client for
[matrix]. Both components are being developed simultaniously.

## Design

* Uses ReaderT to provide runtime configuration and dependencies thoughout the API.

* The client API and the bot framework are separate to allow for these to to be
    re-used independently and to allow for custom bots using this project.

* Core library does not store state. In fact, there is no state persistence at all
    currently. This should be the default so as to preserve privacy.

* Should be blazing fast from the outset by taking advantage of the available
    Haskell concurrency features.

## Copying

Copyright © 2020 Olivia Mackintosh

```
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
```

The full license text is available in the LICENSE file.
