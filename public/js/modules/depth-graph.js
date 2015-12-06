/**
 * Created by Aivean on 12/1/15.
 */

function DepthGraph() {

	var map = new Map();
	var idMap = new Map();
	var rTree = rbush(16);
	var currentFlag = true;
	var latestUpdate = null;
	var serialId = 0;

	var worker = new Worker(depthSortWorkerUrl);

	function returnBuffer(buf) {
		worker.postMessage({t: "ret", buffer: buf}, [buf]);
	}

	worker.onmessage = function (evt) {
		if (latestUpdate) {
			returnBuffer(latestUpdate.buffer);
			latestUpdate = null;
		}
		latestUpdate = evt.data;
	};

	this.intersects = function (a) {
		return rTree.search(rTreeKey(a)).map(function (el) {
			return el[4];
		});
	};

	function isModified(data, a) {
		return data.isoX != a.isoX ||
			data.isoY != a.isoY || data.isoZ != a.isoZ;
	}

	function depth(body) {
		return body.centerX + body.centerY + body.bottom /** 1.25*/;
	}

	function rTreeKey(a) {
		var isoB = a.isoBounds;
		var p =  a.game.iso.project(new Phaser.Plugin.Isometric.Point3(
			isoB.frontX,
			isoB.frontY,
			isoB.bottom
		));

		return [
			Math.floor(p.x - a.width / 2), Math.floor(p.y -  a.height),
			Math.floor(p.x  + a.width / 2), Math.floor(p.y), a
		];
	}

	this.rTreeKey = rTreeKey;

	var Data = function (a) {
		this.id = serialId++;
		this.rTreeKey = rTreeKey(a);
		this.currentFlag = currentFlag;
		this.currentPos = -1;
		this.isoX = a.isoX;
		this.isoY = a.isoY;
		this.isoZ = a.isoZ;
	};

	Data.prototype.update = function (a) {
		this.rTreeKey = rTreeKey(a);
		this.currentFlag = currentFlag;
		this.isoX = a.isoX;
		this.isoY = a.isoY;
		this.isoZ = a.isoZ;
	};

	Data.prototype.msg = function () {
		var a = this.rTreeKey[4];
		var bnds = a.body || a.isoBounds;

		return {
			id: this.id,
			rTreeKey: this.rTreeKey.slice(0, 4),
			depth: depth(bnds),
			bounds: {
				frontX: bnds.frontX,
				backX: bnds.backX,
				frontY: bnds.frontY,
				backY: bnds.backY,
				top: bnds.top,
				bottom: bnds.bottom
			}
		};
	};

	this.topologicalSort = function (arr, suppressModification) {
		var updateMsg = {
			t:'upd',
			updated:[],
			removed:[]
		};

		currentFlag = !currentFlag;
		var i;

		for (i = 0; i < arr.length; i++) {
			var a = arr[i];
			var data = map.get(a);
			if (!data) {
				data = new Data(a);
				rTree.insert(data.rTreeKey);
				map.set(a, data);
				idMap.set(data.id, data);
				updateMsg.updated.push(data.msg());
			} else {
				if (isModified(data, a) &&
					(!suppressModification || !suppressModification(a))) {
					rTree.remove(data.rTreeKey);
					data.update(a);
					rTree.insert(data.rTreeKey);
					updateMsg.updated.push(data.msg());
				}
				data.currentFlag = currentFlag;
				data.currentPos = -1;
			}
		}

		var toRemove = [];

		// marking obsolete elements to remove
		map.forEach(function (data, a) {
			if (data.currentFlag != currentFlag) {
				toRemove.push(a);
				rTree.remove(data.rTreeKey);
				updateMsg.removed.push(data.id);
				idMap.delete(data.id);
			}
		});

		// removing objects
		for (i = 0; i < toRemove.length; i++) {
			var tr = toRemove[i];
			map.delete(tr);
		}

		if (updateMsg.removed.length > 0 || updateMsg.updated.length > 0) {
			worker.postMessage(updateMsg);
		}

		// now to the sort part
		if (latestUpdate) {

			var order =
				new Uint32Array(latestUpdate.buffer, 0, latestUpdate.length);

			var idx = 0;
			for (i = 0; i < order.length; i++) {
				data = idMap.get(order[i]);
				if (data) {
					data.currentPos = idx++;
				}
			}

			for(i=0; i < arr.length; i++) {
				data = map.get(arr[i]);
				if (data.currentPos < 0) {
					data.currentPos = idx++;
				}
			}

			map.forEach(function (data, a) {
				arr[data.currentPos] = a;
			});

			returnBuffer(latestUpdate.buffer);
			latestUpdate = null;
		}

		return arr;
	};
}