(function () {
  var editor = ace.edit("editor");
  editor.setTheme("ace/theme/monokai");
  editor.getSession().setTabSize(2);
  editor.getSession().setUseSoftTabs(true);
  editor.getSession().setMode("ace/mode/javascript");
  editor.commands.addCommand({
    name: 'save',
    bindKey: 'Ctrl-s',
    exec: function(editor) {
      eval(editor.getValue());
    }
  });
  var bind = Mousetrap.bind;
  var newImage = function (path) {
    return _.tap(new Image(), function (img) {
      img.src = path;
    });
  };
  var loadSprites = function (requestedSprites) {
    var sprites = {}
    _.each(requestedSprites, function (path, key) {
      if (_.isArray(path)) {
        sprites[key] = _.map(path, newImage);
      } else {
        sprites[key] = newImage(path);
      }
    });
    return sprites;
  };
  var determineSprite = function (player) {
    if (!player.movement) {
      return playerSprites["down"][0];
    }
    var tileSpeed = (boardSize / tileRoot) / (boardSize * player.movement.speed) * 1000;
    var milliseconds = new Date().getMilliseconds();
    var spriteIndex = Math.floor((milliseconds / (tileSpeed / 8))) % 8;
    return playerSprites[player.movement.direction][spriteIndex];
  };
  var blockSprites = loadSprites({
    g: "images/Blocks/BackgroundTile.png",
    w: "images/Blocks/ExplodableBlock.png",
    s: "images/Blocks/SolidBlock.png",
  });
  var playerSprites = loadSprites({
    up: [
      "images/Bomberman/Up/f00.png",
      "images/Bomberman/Up/f01.png",
      "images/Bomberman/Up/f02.png",
      "images/Bomberman/Up/f03.png",
      "images/Bomberman/Up/f04.png",
      "images/Bomberman/Up/f05.png",
      "images/Bomberman/Up/f06.png",
      "images/Bomberman/Up/f07.png"],
    right: [
      "images/Bomberman/Right/f00.png",
      "images/Bomberman/Right/f01.png",
      "images/Bomberman/Right/f02.png",
      "images/Bomberman/Right/f03.png",
      "images/Bomberman/Right/f04.png",
      "images/Bomberman/Right/f05.png",
      "images/Bomberman/Right/f06.png",
      "images/Bomberman/Right/f07.png"],
    down: [
      "images/Bomberman/Down/f00.png",
      "images/Bomberman/Down/f01.png",
      "images/Bomberman/Down/f02.png",
      "images/Bomberman/Down/f03.png",
      "images/Bomberman/Down/f04.png",
      "images/Bomberman/Down/f05.png",
      "images/Bomberman/Down/f06.png",
      "images/Bomberman/Down/f07.png"],
    left: [
      "images/Bomberman/Left/f00.png",
      "images/Bomberman/Left/f01.png",
      "images/Bomberman/Left/f02.png",
      "images/Bomberman/Left/f03.png",
      "images/Bomberman/Left/f04.png",
      "images/Bomberman/Left/f05.png",
      "images/Bomberman/Left/f06.png",
      "images/Bomberman/Left/f07.png"],
  });
  var bombSprites = loadSprites({
    normal: "images/Bomb/f01.png"
  });
  var explosionSprites = loadSprites({
    normal: [
      "images/Flame/f00.png",
      "images/Flame/f01.png",
      "images/Flame/f02.png",
      "images/Flame/f03.png",
      "images/Flame/f04.png"
    ]
  });
  var bloodSprites = loadSprites({
    normal: [
      "images/blood/blood-1.png",
      "images/blood/blood-2.png",
      "images/blood/blood-3.png",
      "images/blood/blood-4.png",
      "images/blood/blood-5.png",
      "images/blood/blood-6.png"
    ]
  });
  var game = null;
  var boardSize = 640;
  var tileRoot = 12;
  var tileSize = boardSize / tileRoot;
  var playerSize = tileSize / (4 / 3);
  var bombSize = tileSize / (4 / 3);
  var explosionSize = tileSize / (4 / 3);
  var bloodSize = tileSize / (4 / 3);
  var renderedBloods = {};
  var canvas = document.getElementById("board");
  canvas.width = boardSize;
  canvas.height = boardSize;
  var ctx = canvas.getContext("2d");
  var renderBoard = function (board) {
    _.each(board, function (tile, i) {
      ctx.drawImage(blockSprites[tile], (i % tileRoot) * tileSize, Math.floor(i / tileRoot) * tileSize, tileSize, tileSize);
    });
  };
  var renderBombs = function (bombs) {
    _.each(bombs, function (bomb) {
      ctx.drawImage(bombSprites.normal, Math.floor((bomb.dimension.x * boardSize) - (bombSize / 2)), Math.floor((bomb.dimension.y * boardSize) - (bombSize / 2)), bombSize, bombSize);
    });
  };
  var renderExplosions = function (explosions) {
    var sprite = explosionSprites.normal[Math.floor((new Date().getMilliseconds() % 500) / 100)];
    _.each(explosions, function (explosion) {
      ctx.drawImage(sprite, Math.floor((explosion.dimension.x * boardSize) - (explosionSize / 2)), Math.floor((explosion.dimension.y * boardSize) - (explosionSize / 2)), explosionSize, explosionSize);
    });
  };
  var renderBloods = function (bloods) {
    _.each(bloods, function (blood) {
      if (!renderedBloods[blood.id]) {
        renderedBloods[blood.id] = new Date().getTime();
      }
      var delta = new Date().getTime() - renderedBloods[blood.id];
      if (delta < 600) {
        ctx.drawImage(bloodSprites.normal[Math.floor(delta / 100)], Math.floor((blood.dimension.x * boardSize) - (bloodSize / 2)), Math.floor((blood.dimension.y * boardSize) - (bloodSize / 2)), bloodSize, bloodSize);
      }
    });
  };
  var renderPlayers = function (players) {
    _.each(players, function (player) {
      var sprite = determineSprite(player);
      var playerHeight = sprite.height * (playerSize / sprite.width);
      ctx.drawImage(sprite, Math.floor(player.dimension.x * boardSize) - (playerSize / 2), Math.floor(player.dimension.y * boardSize) - (playerSize / 2) - (playerHeight / 2), playerSize, playerHeight);
    });
  };
  var render = function () {
    if (game) {
      ctx.clearRect(0, 0, boardSize, boardSize);
      renderBoard(game.board);
      renderBombs(game.bombs);
      renderExplosions(game.explosions);
      renderBloods(game.bloods);
      renderPlayers(game.players);
    }
    requestAnimationFrame(render);
  };
  var ws = new WebSocket("ws://127.0.0.1:3000");
  ws.onmessage = function (message) {
    game = JSON.parse(message.data);
  };
  requestAnimationFrame(render);
}());
