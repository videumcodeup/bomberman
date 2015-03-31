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
            bomberman.movement._rememberMovement(e.keyCode, "up");
            bomberman.movement._sendMovement("start-movement", "up");
        },

        moveDown: function (e, b) {
            if (e.repeat) return;
            bomberman.movement._rememberMovement(e.keyCode, "down");
            bomberman.movement._sendMovement("start-movement", "down");
        },

        moveLeft: function (e) {
            if (e.repeat) return;
            bomberman.movement._rememberMovement(e.keyCode, "left");
            bomberman.movement._sendMovement("start-movement", "left");
        },

        moveRight: function (e) {
            if (e.repeat) return;
            bomberman.movement._rememberMovement(e.keyCode, "right");
            bomberman.movement._sendMovement("start-movement", "right");
        },

        stopMovement: function (e) {
            if (!_.has(bomberman.movement.moves, e.keyCode.toString())) return;
            bomberman.movement._sendMovement("stop-movement");
            bomberman.movement._forgetMovement(e.keyCode);
        },

        _rememberMovement: function (keyCode, movement) {
            bomberman.movement.moves[keyCode.toString()] = movement;
        },

        _forgetMovement: function (keyCode) {
            delete bomberman.movement.moves[keyCode.toString()];
            if (_.keys(bomberman.movement.moves).length > 0) {
                var direction = _.last(_.values(bomberman.movement.moves));
                bomberman.movement._sendMovement("start-movement", direction);
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

bind("x", bomberman.bombs.regularBomb, "keydown");
bind("w", bomberman.movement.moveUp, "keydown");
bind("s", bomberman.movement.moveDown, "keydown");
bind("a", bomberman.movement.moveLeft, "keydown");
bind("d", bomberman.movement.moveRight, "keydown");
bind(["w", "s", "a", "d"], bomberman.movement.stopMovement, "keyup");
