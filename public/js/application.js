/**
 * Created by Aivean on 11/2/15.
 */

$(function () {

	var game = new Phaser.Game(800, 600, Phaser.AUTO, '', {
		preload: preload,
		create: create,
		update: update,
		render: render
	}, false, false);

	var cursorPos;
	var cursors;
	var player;
	var players = {};
	var isoGroup;
	var socket;
	var latency = 0;
	var timeDiff = 0;
	var selectedTile = null;
	//var chunks = {};
	var updateQueue = new Queue();
	var sortIsScheduled = false;

	var scale = 33;

	function connect() {

		try {
			var host = wsUrl;
			console.log("host:", host);

			socket = new WebSocket(host);

			console.info("Connection state: " + socket.readyState);

			socket.onopen = function () {
				console.info(
					"Connection state: " + socket.readyState + " (open)"
				);
				socket.send(JSON.stringify({"t": "init"}));
				registerUpdate();
			};

			socket.onmessage = function (msg) {
				try {
					var data = JSON.parse(msg.data);
					if (data.t != "pong") {
						//console.log(data);
					}

					switch (data.t) {
						// moved
						case 'm':
							var p = players[data.id];
							if (p) {
								var hor = Math.abs(data.x * scale - p.isoX) >=
									Math.abs(data.y * scale - p.isoY);

								var anim;
								if (data.x * scale < p.isoX && hor) {
									anim = 'left';
								} else if (data.x * scale > p.isoX && hor) {
									anim = 'right';
								} else if (data.y * scale > p.isoY && !hor) {
									anim = 'left'/*'down'*/;
								} else {
									anim = /*'up'*/ 'right';
								}

								var tween = game.add.tween(p).to({
										isoX: (data.x * scale),
										isoY: (data.y * scale),
										isoZ: (data.z * scale + p.isoBounds.height / 2)
									}, Phaser.Math.max(
										data.ts + timeDiff - Date.now(), 50),
									Phaser.Easing.Linear.None,
									false);

								tween.onComplete.add(function () {
									sortIsScheduled = true;
									p.movementsStack -= 1;
									setTimeout(function () {
										if (p.movementsStack == 0) {
											p.animations.stop();
										}
									}, Math.max(latency * 2, 50));
								}, this);
								p.animations.play(anim);
								p.movementsStack += 1;
								tween.start();
							}
							break;

						// player added
						case 'pa':
							var newP = createPlayer(data.id,
								data.x * scale,
								data.y * scale,
								data.z * scale
							);
							if (data.cur) {
								player = newP;
								game.camera.focusOn(player);
								game.camera.follow(player);
							}
							sortIsScheduled = true;
							break;

						// tile added
						case 'ta':
							updateQueue.enqueue(function () {
								var tempGroup = game.add.group();
								data.tiles.forEach(function (tile) {
									var t = game.add.isoSprite(tile.x * scale,
										tile.y * scale, tile.z * scale,
										'tileset', tile.tile, tempGroup);
									//game.physics.isoArcade.enable(t);

									if (tile.overlay) {
										t.addChild(game.make.image(0, 0,
											'tileset', tile.overlay)).anchor.set(0.5, 1);
									}

									t.anchor.set(0.5);
									t.isoZ += t.isoBounds.height / 2;

									t.extra = {};
									t.extra.origIndex = {
										x: tile.x,
										y: tile.y,
										z: tile.z
									};
									t.extra.standable = tile.standable || false;
									t.extra.chunkN = data.c;

									t.extra.origZ = t.isoZ;

									t.extra.isWater =
										(tile.tile.indexOf('water') != -1);

									t.extra.isMovingWater =
										(tile.tile ==
										'water-open-0000-01' ||
										tile.tile == 'water-open-0000-01');

									t.extra.isSelectable = !t.extra.isWater;
									if (tile.tile.indexOf('higrass') != -1) {
										t.extra.isTransparent = true;
										t.extra.isSelectable = false;
									}

								});
								game.iso.topologicalSort(tempGroup);
								isoGroup.addMultiple(tempGroup);
								tempGroup.destroy();
								sortIsScheduled = true;
							});
							break;
						case 'tr':
							updateQueue.enqueue(function () {
								data.c.forEach(function (c) {
									isoGroup.removePredicate(function (tile) {
										return tile.hasOwnProperty('extra') &&
											tile.extra.chunkN == c;
									});
								});
							});
							break;

						// player removed
						case 'pr':
							if (players[data.id]) {
								var p = players[data.id];
								delete players[data.id];
								isoGroup.remove(p);
								p.destroy();
							}
							break;

						// player talking
						case 'pt':
							if (players[data.id]) {
								var p = players[data.id];
								var prefix = p == player ? 'You: ' : 'Someone: ';
								$("<p></p>").text(prefix + data.msg)
									.appendTo($("#chat-input div")).get(0).scrollIntoView();

								var bubble = new SpeechBubble(game, 0, -10,
									20, data.msg);
								p.addChild(bubble);
								setTimeout(function() {
									player.removeChild(bubble);
									bubble.destroy();
								}, 5000);
							}
							break;

						// pong received
						case 'pong':
							var curTime = Date.now();
							var roundTime = curTime - data.ts;
							latency = latency +
								((roundTime / 2.0) - latency) * 0.1;
							var curTimeDiff = curTime - data.serverTs - latency;
							timeDiff =
								timeDiff + (curTimeDiff - timeDiff) * 0.2;
							//console.log(latency, timeDiff);
							break;
					}

				} catch (e) {
					console.error(e, "message:", msg);
				}
			};

			socket.onclose = function () {
				console.info(
					'Socket Status: ' + socket.readyState + ' (Closed)');
				player = null;
			};

		} catch (exception) {
			console.error('Error', exception);
		}

	}

	function preload() {
		game.world.setBounds(0, 0, 50000, 50000);

		game.time.advancedTiming = true;

		game.plugins.add(new Phaser.Plugin.Isometric(game));

		game.load.spritesheet('player',
			assets['maleprotagonistallwalk2.png'], 64, 64);

		game.load.atlasJSONHash('tileset', assets['iso64x64_2.png'],
			assets['iso64x64_2.json']);

		game.load.atlasJSONHash('speech-bubble', assets['speech_bubble.png'],
			assets['speech_bubble.json']);

		//game.physics.startSystem(Phaser.Plugin.Isometric.ISOARCADE);
		game.iso.anchor.setTo(0.5, 0.1);
	}

	function create() {
		game.stage.disableVisibilityChange = true;
		//  We're going to be using physics, so enable the Arcade Physics system
		//game.physics.startSystem(Phaser.Physics.ARCADE);
		cursors = game.input.keyboard.createCursorKeys();

		isoGroup = game.add.group();
		//isoGroup.enableBody = true;
		//isoGroup.physicsBodyType = Phaser.Plugin.Isometric.ISOARCADE;

		cursorPos = new Phaser.Plugin.Isometric.Point3();

		function onClick() {
			// Update the cursor position.
			// It's important to understand that screen-to-isometric projection
			// means you have to specify a z position manually, as this cannot
			// be easily determined from the 2D pointer position without extra
			// trickery. By default, the z position is 0 if not set.

			function setTint(tile, tint) {
				tile.tint = tint;
				tile.children.forEach(function (c) {
					c.tint = tint;
				});
			}

			if (selectedTile) {
				setTint(selectedTile, 0xffffff);
			}

			selectedTile = null;

			// Loop through all tiles and test to see if the 3D position
			// from above intersects with the automatically generated
			// IsoSprite tile bounds.
			isoGroup.forEach(function (tile) {
				// if ground object
				if (!tile.hasOwnProperty('extra') || !tile.extra.isSelectable) {
					return;
				}
				game.iso.unproject(game.input.activePointer.position, cursorPos,
					tile.extra.standable ? tile.isoBounds.top : tile.isoBounds.bottom);
				var inBounds = tile.isoBounds.containsXY(cursorPos.x,
					cursorPos.y);
				// If it does, do a little animation and tint change.
				if (inBounds) {
					selectedTile = tile;
				}
			});

			//console.log("click", selectedTile);
			if (selectedTile) {
				setTint(selectedTile, 0x86bfda);

				var pos = selectedTile.extra.origIndex;
				//console.log("moveTo", pos);
				socket.send(JSON.stringify({
					"t": "m",
					"x": pos.x,
					"y": pos.y,
					"z": pos.z + 1
				}));
			}
		}

		//game.input.onTap.add(onClick, this);
		game.input.mouse.mouseDownCallback = onClick;
		//see here: http://docs.phaser.io/Keyboard.js.html for the keycodes
		var chatInput = $("#chat-input");
		game.input.keyboard.addKey(Phaser.KeyCode.ENTER).onDown.add(function(key) {
			if (chatInput.is(':visible')) {
				chatInput.hide();
				var $input = chatInput.find("input");
				var text = $input.val().trim();
				if (text.length != 0) {
					socket.send(JSON.stringify({"t": "say", "msg": text}));
				}
				$input.val("");
			} else {
				chatInput.show().find("input").focus();
				var lastChild = chatInput.find("div p").last();
				if (lastChild.length > 0) {
					lastChild.get(0).scrollIntoView();
				}
			}
		}, this);
		game.input.keyboard.addKey(Phaser.KeyCode.ESC).onDown.add(function(key) {
			if (chatInput.is(':visible')) {
				chatInput.hide();
			}
		}, this);
		chatInput.submit(function () {
			return false;
		});

		connect();
	}


	function createPlayer(id, x, y, z) {
		//
		// The player and its settings

		var player = game.add.isoSprite(x, y, z, 'player');
		//game.physics.isoArcade.enable(player);
		isoGroup.add(player);
		//player.scale.x = 2;


		player.anchor.set(0.5);
		player.isoZ += player.isoBounds.height / 2;

		//player.anchor.set(0.5, 1.25);
		//player.body.moves = false;

		//var player = game.add.sprite(x, y, 'player');

		//  We need to enable physics on the player
		//game.physics.arcade.enable(player);
		//player.body.collideWorldBounds = true;

		//  Our two animations, walking left and right.
		player.animations.add('left', [6, 7, 8, 9, 10, 11], 8, true);
		player.animations.add('right', [0, 1, 2, 3, 4, 5], 8, true);
		//player.animations.add('down', [1, 3, 5, 7, 9, 11], 8, true);
		//player.animations.add('up', [13, 15, 17, 19, 21, 23], 8, true);

		player.movementsStack = 0;

		players[id] = player;

		return player;
	}


	var logged = false;
	function update() {

		//isoGroup.forEach(function (t) {
		//	if (player && t != player &&
		//		player.isoBounds.intersects(t.isoBounds)) {
		//		t.alpha = 0.5;
		//	} else {
		//		t.alpha = 1;
		//	}
		//});

		var queueWasEmpty = updateQueue.isEmpty();
		while (!updateQueue.isEmpty()) {
			updateQueue.dequeue()();
		}

		isoGroup.forEach(function (w) {
			if (w.extra) {
				if (w.extra.isTransparent) {
					var trans = false;
					for(pk in players) {
						if (players.hasOwnProperty(pk) && players[pk].isoBounds.intersects(w.isoBounds)) {
							trans = true;
							break;
						}
					}
					w.alpha = trans ? 0.5 : 1;
				} else if (w.extra.isWater) {
					var dz = (-2 * Math.sin(
							(game.time.now / 1.5 + (w.isoX * 7)) * 0.004)) +
						(-1 * Math.sin((game.time.now + (w.isoY * 8)) * 0.005));
					if (w.extra.isMovingWater) {
						w.isoZ = w.extra.origZ + dz;
					}
					w.alpha = Phaser.Math.clamp(1 + (dz * 0.1), 0.2, 0.9);
				}
			}
		});

		sortIsScheduled = true;
		if (queueWasEmpty && sortIsScheduled) {
			game.iso.insertionSort(isoGroup);
			sortIsScheduled = false;
		}
		if (!logged) {
			console.log(isoGroup);
			logged = true;
		}
	}

	function render() {
		//isoGroup.forEach(function (tile) {
		//	game.debug.body(tile, 'rgba(189, 221, 235, 0.6)', false);
		//});
		//game.debug.text(game.time.fps || '--', 2, 14, "#00ff00");
	}

	function registerUpdate() {
		setInterval(function () {
			socket.send(JSON.stringify({
				"t": "ping",
				"ts": Date.now(),
			}));
		}, 1000);
	}
});