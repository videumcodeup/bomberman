# API

This is the API for the server-side version of Bomberman. Written by [Andreas
Lundahl](https://github.com/andreaslundahl) and [Kevin
Sjöberg](https://github.com/kevinsjoberg).

## Commands

Commands are sent as [JSON](http://json.org/) over
[WebSockets](https://developer.mozilla.org/en-US/docs/WebSockets).

### start-movement

Request to start moving the Bomberman in the given direction.

**directions:** `"up", "down", "left", "right"`

```json
{"command": "start-movement", "arguments": ["up"]}
```

## stop-movement

Request to stop moving the Bomberman.

```json
{"command": "stop-movement", "arguments": []}
```

## place-bomb

Request to place a bomb at the tile of where the Bomberman stands.

```json
{"command": "place-bomb", "arguments": []}
```
