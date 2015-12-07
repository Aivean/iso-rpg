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

	var player;
	var players = {};
	var isoGroup;
	var socket;
	var latency = 0;
	var timeDiff = 0;
	var selectedTile = null;
	var updateQueue = new Queue();
	var depthGraph = new DepthGraph(game);

	var scale = 33;

	function addToConsole(elem) {
		$(elem).appendTo($("#chat-input div")).get(0).scrollIntoView();
	}


	function TileExtra(tile, chunkN, isoZ) {
		this.origIndex = {
			x: tile.x,
			y: tile.y,
			z: tile.z
		};
		this.standable = tile.standable || false;
		this.chunkN = chunkN;

		this.origZ = isoZ;
		this.height = tile.height;

		this.isWater =
			(tile.tile.indexOf('water') != -1);

		this.isMovingWater =
			(tile.tile ==
			'water-open-0000-01' ||
			tile.tile == 'water-open-0000-01');

		this.isSelectable = !this.isWater;
		if (tile.tile.indexOf('higrass') != -1) {
			this.isTransparent = true;
			this.isSelectable = false;
		}
	}

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
								if (hor && data.x * scale < p.isoX) {
									anim = p.extra.movements.west;
								} else if (hor && data.x * scale > p.isoX) {
									anim = p.extra.movements.east;
								} else if (data.y * scale > p.isoY) {
									anim = p.extra.movements.south;
								} else /*if (data.y * scale < p.isoY)*/ {
									anim = p.extra.movements.north;
								}

								var zShift = data.zShift * scale;

								var tween = game.add.tween(p).to({
										isoX: (data.x * scale),
										isoY: (data.y * scale),
										isoZ: (data.z * scale - Math.abs(p.width / 2)) + 1
												+ zShift

									}, Phaser.Math.max(
										data.ts + timeDiff - Date.now(), 50),
									Phaser.Easing.Linear.None,
									false);

								tween.onComplete.add(function () {
									p.extra.movementsStack -= 1;
									setTimeout(function () {
										if (p.extra.movementsStack == 0) {
											p.extra.movements.stop();
										}
									}, Math.max(latency * 2, 50));
								}, this);
								anim(); // run the animation
								p.extra.movementsStack += 1;
								tween.start();
							}
							break;

						// player added
						case 'pa':
							var newP = createPlayer(data.id,
								data.x * scale,
								data.y * scale,
								data.z * scale,
								data.sprite
							);
							if (data.cur) {
								player = newP;
								game.camera.focusOn(player);
								game.camera.follow(player);
							}
							break;

						// tile added
						case 'ta':
							updateQueue.enqueue(function () {
								data.tiles.forEach(function (tile) {
									var t = game.add.isoSprite(tile.x * scale,
										tile.y * scale, tile.z * scale,
										'tileset', tile.tile, isoGroup);
									//game.physics.isoArcade.enable(t);

									if (tile.overlay) {
										t.addChild(game.make.image(0, 0,
											'tileset', tile.overlay))
											.anchor.set(0.5, 1.5);
									}

									t.anchor.set(0.5, 1);
									t.isoZ -= Math.abs(t.width / 2);

									t.extra = new TileExtra(tile, data.c, t.isoZ);
								});
							});
							break;
						case 'tr':
							updateQueue.enqueue(function () {
								data.c.forEach(function (c) {
									isoGroup.removePredicate(function (tile) {
										return tile.extra &&
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

								addToConsole(
									$("<p></p>").text(prefix + data.msg));

								var bubble = new SpeechBubble(game, 0, -10 - p.height / 2,
									20, data.msg);
								p.addChild(bubble);
								setTimeout(function() {
									player.removeChild(bubble);
									bubble.destroy();
								}, 5000);
							}
							break;
						// server message
						case 'sm':
							addToConsole(
								$("<p></p>").css({ 'color': 'red'}).text(data.msg));
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

		game.load.spritesheet('poring',
			assets['poring.png'], 36, 32);

		game.load.atlasJSONHash('tileset', assets['iso64x64_2.png'],
			assets['iso64x64_2.json']);

		game.load.atlasJSONHash('speech-bubble', assets['speech_bubble.png'],
			assets['speech_bubble.json']);

		//game.physics.startSystem(Phaser.Plugin.Isometric.ISOARCADE);
		game.iso.anchor.setTo(0.5, 0.1);
	}

	function create() {
		game.stage.disableVisibilityChange = true;
		isoGroup = game.add.group();

		function onClick() {
			function setTint(tile, tint) {
				tile.tint = tint;
				tile.children.forEach(function (c) {
					c.tint = tint;
				});
			}

			if (selectedTile) {
				setTint(selectedTile, 0xffffff);
			}

			var pos = game.input.activePointer.position;
			var cursorPos = game.iso.unproject(pos);
			cursorPos = game.iso.projectXY(cursorPos);
			var x = cursorPos.x;
			var y = cursorPos.y;

			selectedTile = null;

			for (pk in players) {
				if (players.hasOwnProperty(pk)) {
					t = players[pk];
					if (t != player &&
						(!selectedTile || selectedTile.z < t.z)) {
						t.inputEnabled = true;
						t.input.pixelPerfectAlpha = 0.2;
						t.input.pixelPerfectOver = true;
						if (t.input.checkPointerOver(game.input.activePointer,
								false)) {

							selectedTile = t;
						}
						t.inputEnabled = false;
					}
				}
			}

			depthGraph.intersects([x,y,x,y]).forEach(function (t) {
				if (t.extra && t.extra.isSelectable) {
					t.inputEnabled = true;
					t.input.pixelPerfectAlpha = 0.2;
					t.input.pixelPerfectOver = true;
					if (t.input.checkPointerOver(game.input.activePointer,
							false)) {
						if (!selectedTile || selectedTile.z < t.z) {
							selectedTile = t;
						}
					}
					t.inputEnabled = false;
				}
			});

			if (selectedTile) {
				setTint(selectedTile, 0x86bfda);

				if (!game.input.keyboard.isDown(Phaser.Keyboard.SHIFT)) {
					if (selectedTile.extra && selectedTile.extra.id) {
						socket.send(JSON.stringify({
							"t": "a",
							"id" : selectedTile.extra.id
						}));
					} else {
						pos = selectedTile.extra.origIndex;
						//console.log("moveTo", pos);
						socket.send(JSON.stringify({
							"t": "m",
							"x": pos.x,
							"y": pos.y,
							"z": pos.z + 1
						}));
					}
				}
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
					if (/^\//.test(text)) { // system command
						text = text.substr(1);
						if (text == 'help') {
							var $ul = $("<ul></ul>");
							['ping', 'generateWorld', 'tileReplace',
								'tileRemove', 'tileInfo', 'tileExtra', 'location',
								'tileNeighbors', 'selfInfo'].forEach(function (c) {
								$("<li></li>").text(c).appendTo($ul);
							});
							addToConsole($ul);
						} else if (text == 'ping') {
							addToConsole(
								$('<p></p>').text('Latency: ' + latency));
							addToConsole(
								$('<p></p>').text('TimeDiff: ' + timeDiff));
						} else if (text == 'tileRemove' && selectedTile) {
							isoGroup.remove(selectedTile, true);
							selectedTile = null;
						} else if (text == 'tileInfo' && selectedTile) {
							console.log(selectedTile);
						} else if (text == 'selfInfo' && player) {
							console.log(player);
						} else if (text == 'tileExtra' && selectedTile) {
							console.log(selectedTile.extra);
						} else if (text == 'tileNeighbors' && selectedTile) {
							var oi = selectedTile.extra.origIndex;
							socket.send(JSON.stringify({"t": "admin",
								"cmd": 'tileNeighbors '+ oi.x + ' '+ oi.y + ' ' + oi.z}));
						} else if (/^tileReplace (.*)/.test(text) && selectedTile) {
							selectedTile.frameName = (/^tileReplace (.*)/.exec(text)[1]);
						} else {
							socket.send(JSON.stringify({"t": "admin", "cmd": text}));
						}
					} else {
						socket.send(JSON.stringify({"t": "say", "msg": text}));
					}
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


	function createPlayer(id, x, y, z, sprite) {
		sprite = sprite || "player";
		var player = game.add.isoSprite(x, y, z, sprite);
		isoGroup.add(player);

		player.anchor.set(0.5, 1);
		player.isoZ -= Math.abs(player.width / 2);

		player.extra = {id: id, movements: {}, movementsStack:0};
		function scaleAndAnimate(scaleX, anim) {
			return function () {
				player.scale.x = scaleX;
				player.animations.play(anim);
			};
		}

		if (sprite == 'poring') {
			player.alpha = 0.8;
			player.animations.add('south', [0, 1, 2, 3], 5, true);
			player.animations.add('west', [4,5,6,7], 5, true);
			player.extra.movements.south = scaleAndAnimate(1, 'south');
			player.extra.movements.west = scaleAndAnimate(1, 'west');
			player.extra.movements.north = scaleAndAnimate(-1, 'west');
			player.extra.movements.east = scaleAndAnimate(-1, 'south');
		} else  {
			player.animations.add('left', [6, 7, 8, 9, 10, 11], 8, true);
			player.animations.add('right', [0, 1, 2, 3, 4, 5], 8, true);

			player.extra.movements.south =
				player.extra.movements.west = scaleAndAnimate(1, 'left');
			player.extra.movements.north =
				player.extra.movements.east = scaleAndAnimate(1, 'right');
		}

		//common for now
		player.extra.movements.stop = function () {
			player.animations.stop();
		};

		player.extra.height = 3;

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

		if (queueWasEmpty) {
			depthGraph.topologicalSort(isoGroup.children, isModificationSuppressed);
			isoGroup.updateZ();
		}

		if (!logged) {
			console.log(isoGroup);
			logged = true;
		}
	}

	function isModificationSuppressed(t) {
		return t.extra && t.extra.isWater;
	}

	function render() {
		//game.debug.text(game.time.fps || '--', 2, 14, "#00ff00");
		//game.debug.text(depthGraph.elsAvg.get(), 2, 14, "#00ffff");

		//game.debug.text(depthGraph.sorted, 2, 24, "#00ff00");

		//if (game.input.keyboard.isDown(Phaser.Keyboard.SHIFT)) {
		//	game.debug.text(depthGraph.misplAvg.get(), 2, 34, "#ff0000");
		//}
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