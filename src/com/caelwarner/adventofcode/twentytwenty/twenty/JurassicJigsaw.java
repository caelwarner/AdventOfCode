package com.caelwarner.adventofcode.twentytwenty.twenty;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class JurassicJigsaw {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/twenty/input.txt");

		System.out.println(calculateWaterRoughness(input));
	}

	private static long findCornersProduct(List<String> input) {
		long cornersProduct = 1;
		List<Tile> tiles = new ArrayList<>();
		List<String> borders = new ArrayList<>();

		for (int i = 0; i < input.size(); i += 12) {
			tiles.add(new Tile(input.subList(i, i + 11)));
			borders.addAll(tiles.get(tiles.size() - 1).borders);
		}

		for (Tile tile : tiles) {
			if (tile.matches(borders) == 2)
				cornersProduct *= tile.id;
		}

		return cornersProduct;
	}

	private static int calculateWaterRoughness(List<String> input) {
		Tile map = new Tile(-1, buildImage(input));
		int seaMonsters = 0;

		for (int i = 1; i <= 10; i++) {
			if (i % 10 == 0)
				map.rotateCounterClockwise();
			else if (i % 5 == 0)
				map.rotateClockwise();
			else if (i % 2 == 0)
				map.flipVertically();
			else
				map.flipHorizontally();

			seaMonsters = findSeaMonsters(map.matrix);
			if (seaMonsters > 0)
				break;
		}

		return map.matrix.stream().mapToInt(line -> line.replace(".", "").length()).sum() - (seaMonsters * 15);
	}

	private static List<String> buildImage(List<String> input) {
		List<Tile> tiles = new ArrayList<>();
		List<Tile> tileMap = new ArrayList<>();
		List<String> borders = new ArrayList<>();

		for (int i = 0; i < input.size(); i += 12) {
			tiles.add(new Tile(input.subList(i, i + 11)));
			borders.addAll(tiles.get(tiles.size() - 1).borders);
		}

		// Find top-left most tile by checking for two border matches then checking if the matches are (90, 180)
		tileMap.add(tiles.stream()
				.filter(tile -> tile.matches(borders) == 2)
				.filter(tile -> tile.matches(borders, 1) && tile.matches(borders, 2))
				.findFirst().orElseThrow(() -> new RuntimeException("Can't find top left tile")));

		int width = (int) Math.sqrt(tiles.size());

		// Find position and orientation for each tile
		topLeftOrientation:
		for (int i = 1; i <= 4; i++) {
			if (i % 2 == 0)
				tileMap.get(0).flipHorizontally();
			else
				tileMap.get(0).flipVertically();

			for (int j = 0; j < tiles.size() - 1; j++) {
				Tile tile;

				if (tileMap.size() % width == 0)
					tile = findNextTile(tiles, tileMap, 2, 0, width - 1);
				else
					tile = findNextTile(tiles, tileMap, 1, 3, 0);

				if (tile == null)
					continue topLeftOrientation;

				tileMap.add(tile);
			}

			break;
		}

		// Assemble tiles together
		List<String> largeMap = new ArrayList<>();

		for (int i = 0; i < tileMap.size(); i++) {
			List<String> innerMatrix = tileMap.get(i).getInnerMatrix();

			if (i % width == 0) {
				largeMap.addAll(innerMatrix);
				continue;
			}

			for (int row = 1; row <= innerMatrix.size(); row++) {
				int j = largeMap.size() - row;
				largeMap.set(j, largeMap.get(j) + innerMatrix.get(innerMatrix.size() - row));
			}
		}

		return largeMap;
	}

	private static Tile findNextTile(List<Tile> tiles, List<Tile> tileMap, int placedAgainstBorder, int placedBorder, int width) {
		for (Tile tile : tiles) {
			if (tileMap.contains(tile))
				continue;

			for (int i = 1; i <= 10; i++) {
				if (i % 10 == 0)
					tile.rotateCounterClockwise();
				else if (i % 5 == 0)
					tile.rotateClockwise();
				else if (i % 2 == 0)
					tile.flipVertically();
				else
					tile.flipHorizontally();

				if (tileMap.get(tileMap.size() - width - 1).borders.get(placedAgainstBorder).equals(tile.borders.get(placedBorder))) {
					return tile;
				}
			}
		}

		return null;
	}

	private static int findSeaMonsters(List<String> map) {
		String top = ".{18}#.";
		String middle = "(#.{4}#){3}##";
		String bottom = "(.#.){6}..";

		int seaMonsters = 0;

		for (int row = 1; row < map.size() - 1; row++) {
			for (int col = 0; col < map.get(row).length() - 20; col++) {
				if (map.get(row - 1).substring(col, col + 20).matches(top) &&
						map.get(row).substring(col, col + 20).matches(middle) &&
						map.get(row + 1).substring(col, col + 20).matches(bottom)) {
					seaMonsters++;
				}
			}
		}

		return seaMonsters;
	}

	private static class Tile {

		public final int id;
		public List<String> matrix;
		public List<String> borders;

		public Tile(List<String> input) {
			this.id = Integer.parseInt(input.get(0).substring(5, 9));
			this.matrix = input.subList(1, input.size());
			this.borders = getBorders(matrix);
		}

		public Tile(int id, List<String> matrix) {
			this.id = id;
			this.matrix = matrix;
			this.borders = new ArrayList<>();
		}

		public int matches(List<String> allBorders) {
			int matches = 0;

			for (String border : allBorders) {
				if (this.borders.contains(border) || this.borders.contains(reverse(border)))
					matches++;
			}

			return matches - 4;
		}

		public boolean matches(List<String> allBorders, int borderIndex) {
			return allBorders.contains(this.borders.get(borderIndex)) || allBorders.contains(reverse(this.borders.get(borderIndex)));
		}

		public void flipHorizontally() {
			matrix = new ArrayList<>(matrix.stream().map(Tile::reverse).toList());
			borders = getBorders(matrix);
		}

		public void flipVertically() {
			Collections.reverse(matrix);
			borders = getBorders(matrix);
		}

		public void rotateClockwise() {
			List<String> newMatrix = new ArrayList<>();

			for (int col = 0; col < matrix.get(0).length(); col++) {
				StringBuilder builder = new StringBuilder();

				for (int row = matrix.size() - 1; row >= 0; row--) {
					builder.append(matrix.get(row).charAt(col));
				}

				newMatrix.add(builder.toString());
			}

			matrix = newMatrix;
		}

		public void rotateCounterClockwise() {
			List<String> newMatrix = new ArrayList<>();

			for (int col = matrix.get(0).length() - 1; col >= 0; col--) {
				StringBuilder builder = new StringBuilder();

				for (String row : matrix) {
					builder.append(row.charAt(col));
				}

				newMatrix.add(builder.toString());
			}

			matrix = newMatrix;
		}

		public List<String> getInnerMatrix() {
			List<String> innerMatrix = new ArrayList<>();

			for (int row = 1; row < matrix.size() - 1; row++) {
				innerMatrix.add(matrix.get(row).substring(1, matrix.get(row).length() - 1));
			}

			return innerMatrix;
		}

		private static List<String> getBorders(List<String> matrix) {
			List<String> borders = new ArrayList<>();

			borders.add(matrix.get(0));

			for (int x = matrix.get(0).length() - 1; x >= 0; x -= matrix.get(0).length() - 1) {
				StringBuilder builder = new StringBuilder();

				for (String row : matrix) {
					builder.append(row.charAt(x));
				}

				borders.add(builder.toString());
			}

			borders.add(2, matrix.get(matrix.size() - 1));

			return borders;
		}

		private static String reverse(String border) {
			return new StringBuilder(border).reverse().toString();
		}

	}

}
