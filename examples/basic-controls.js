var bomberman = {
    bombs: {
        regularBomb: function () {
            ws.send(JSON.stringify({command: "place-bomb", arguments: []}));
        }
    },

    movement: {
        moves: {},

        moveUp: function (e) {
            if (e.repeat) return;
            this._rememberMovement(e.keyCode, "up");
            this._sendMovement("start-movement", "up");
        },

        moveDown: function (e, b) {
            if (e.repeat) return;
            this._rememberMovement(e.keyCode, "down");
            this._sendMovement("start-movement", "down");
        },

        moveLeft: function (e) {
            if (e.repeat) return;
            this._rememberMovement(e.keyCode, "left");
            this._sendMovement("start-movement", "left");
        },

        moveRight: function (e) {
            if (e.repeat) return;
            this._rememberMovement(e.keyCode, "right");
            this._sendMovement("start-movement", "right");
        },

        stopMovement: function (e) {
            if (!_.has(this.moves, e.keyCode.toString())) return;
            this._sendMovement("stop-movement");
            this._forgetMovement(e.keyCode);
        },

        _rememberMovement: function (keyCode, movement) {
            this.moves[keyCode.toString()] = movement;
        },

        _forgetMovement: function (keyCode) {
            delete this.moves[keyCode.toString()];
            if (_.keys(this.moves).length > 0) {
                var direction = _.last(_.values(this.moves));
                this._sendMovement("start-movement", direction);
            }
        },

        _sendMovement: function (command) {
            ws.send(JSON.stringify({
                command: command,
                arguments: Array.prototype.slice.call(arguments, 1)
            }));
        },
    }
};

_.bindAll(bomberman.bombs);
_.bindAll(bomberman.movement);

bind("x", bomberman.bombs.regularBomb, "keydown");
bind("w", bomberman.movement.moveUp, "keydown");
bind("s", bomberman.movement.moveDown, "keydown");
bind("a", bomberman.movement.moveLeft, "keydown");
bind("d", bomberman.movement.moveRight, "keydown");
bind(["w", "s", "a", "d"], bomberman.movement.stopMovement, "keyup");
