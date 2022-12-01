package twentytwenty.twentyfour;

import util.java.Read;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class LobbyLayout {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/twentyfour/input.txt");

		System.out.println(getBlackTilesCount(input));
	}

	private static int getBlackTilesCountInitial(List<String> input) {
		return findInitialTiles(input).size();
	}

	private static int getBlackTilesCount(List<String> input) {
		Set<Tile> tiles = findInitialTiles(input);
		Set<Tile> nextTiles = new HashSet<>(tiles);

		for (int day = 0; day < 100; day++) {
			for (Tile tile : tiles) {
				if (tile.computeDay(tiles, nextTiles))
					nextTiles.remove(tile);
			}

			tiles = nextTiles;
			nextTiles = new HashSet<>(tiles);
		}

		return tiles.size();
	}

	private static Set<Tile> findInitialTiles(List<String> locations) {
		Set<Tile> tiles = new HashSet<>();

		for (String location : locations) {
			Tile tile = findTile(location);

			if (tiles.contains(tile))
				tiles.remove(tile);
			else
				tiles.add(tile);
		}

		return tiles;
	}

	private static Tile findTile(String location) {
		Tile tile = new Tile();

		for (int i = 0; i < location.length(); i++) {
			char c1 = location.charAt(i);

			if (c1 == 'e') {
				tile.addTo(TileDirection.EAST);
			} else if (c1 == 'w') {
				tile.addTo(TileDirection.WEST);
			} else {
				i++;
				char c2 = location.charAt(i);

				if (c1 == 'n') {
					if (c2 == 'e') {
						tile.addTo(TileDirection.NORTH_EAST);
					} else {
						tile.addTo(TileDirection.NORTH_WEST);
					}
				} else if (c1 == 's') {
					if (c2 == 'e') {
						tile.addTo(TileDirection.SOUTH_EAST);
					} else {
						tile.addTo(TileDirection.SOUTH_WEST);
					}
				}
			}
		}

		return tile;
	}

	private static class Tile {

		public int q;
		public int r;
		public int s;

		public Tile() {
			this(0, 0, 0);
		}

		public Tile(int q, int r, int s) {
			this.q = q;
			this.r = r;
			this.s = s;
		}

		public Tile add(TileDirection direction) {
			return new Tile(this.q + direction.q, this.r + direction.r, this.s + direction.s);
		}

		public void addTo(TileDirection direction) {
			this.q += direction.q;
			this.r += direction.r;
			this.s += direction.s;
		}

		public boolean computeDay(Set<Tile> tiles, Set<Tile> nextTiles) {
			int flippedTiles = 0;

			for (TileDirection direction : TileDirection.values()) {
				if (tiles.contains(add(direction))) {
					flippedTiles++;

				} else {
					computeNeighbours(direction, tiles, nextTiles);
				}
			}

			return flippedTiles == 0 || flippedTiles > 2;
		}

		public void computeNeighbours(TileDirection neighbourDirection, Set<Tile> tiles, Set<Tile> nextTiles) {
			int flippedTiles = 0;
			Tile tile = add(neighbourDirection);

			for (TileDirection direction : TileDirection.values()) {
				if (tiles.contains(tile.add(direction)))
					flippedTiles++;
			}

			if (flippedTiles == 2) {
				nextTiles.add(tile);
			}
		}

		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (o == null || getClass() != o.getClass()) return false;
			Tile tile = (Tile) o;
			return q == tile.q && r == tile.r && s == tile.s;
		}

		@Override
		public int hashCode() {
			return Objects.hash(q, r, s);
		}

	}

	private enum TileDirection {
		NORTH_EAST(+1, 0, -1),
		EAST(+1, -1, 0),
		SOUTH_EAST(0, -1, +1),
		SOUTH_WEST(-1, 0, +1),
		WEST(-1, +1, 0),
		NORTH_WEST(0, +1, -1);

		public final int q;
		public final int r;
		public final int s;

		TileDirection(int q, int r, int s) {
			this.q = q;
			this.r = r;
			this.s = s;
		}
	}

}
