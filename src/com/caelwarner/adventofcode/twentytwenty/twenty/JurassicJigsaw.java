package com.caelwarner.adventofcode.twentytwenty.twenty;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class JurassicJigsaw {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/twenty/input.txt");

		buildImage(input);
	}

	/*
	 * Find top-left most tile by checking for two border matches then checking if the matches are (90, 180)
	 */

	private static void buildImage(List<String> input) {
		List<Tile> tiles = new ArrayList<>();
		List<String> borders = new ArrayList<>();

		for (int i = 0; i < Math.round(input.size() / 12.0F); i++) {
			tiles.add(new Tile(input.subList(i * 12, (i * 12) + 11)));
		}

		Tile[][] tileMap = new Tile[(int) Math.sqrt(tiles.size())][(int) Math.sqrt(tiles.size())];
		List<String> fullImage = new ArrayList<>();

		for (Tile tile : tiles) {
			borders.addAll(tile.borders.values());
		}

		for (Tile tile : tiles) {
			int matches = (int) borders.stream().filter(border1 -> tile.borders.values().stream().anyMatch(border2 -> matchesBorder(border1, border2))).count() - 4;

			boolean isTopLeftMostTile = (Collections.frequency(borders, tile.borders.get(90)) == 2) && (Collections.frequency(borders, tile.borders.get(180)) == 2);

			if (matches == 2 && isTopLeftMostTile) {
				tileMap[0][0] = tile;
			}
		}

		for (int y = 0; y < tileMap.length; y++) {
			for (Tile tile : tiles) {
				if (y == 0)
					break;

				if (Arrays.stream(tileMap).anyMatch(ySlice -> Arrays.stream(ySlice).anyMatch(tile1 -> tile1 != null && tile1.id == tile.id))) {
					continue;
				}

				Tile prevTile = tileMap[y - 1][0];

				if (tile.borders.values().stream().anyMatch(border -> matchesBorder(border, prevTile.borders.get(180)))) {
					tileMap[y][0] = tile;

					break;
				}
			}

			xLoop:
			for (int x = 1; x < tileMap[y].length; x++) {
				for (Tile tile : tiles) {
					if (Arrays.stream(tileMap).anyMatch(ySlice -> Arrays.stream(ySlice).anyMatch(tile1 -> tile1 != null && tile1.id == tile.id))) {
						continue;
					}

					Tile prevTile = tileMap[y][x - 1];

					for (int r = 0; r < 360; r += 90) {
						if (tile.borders.values().stream().anyMatch(border -> matchesBorder(border, prevTile.borders.get(90)))) {
							if (r != 0)
								tile.rotateMatrix(r);

							tileMap[y][x] = tile;

							continue xLoop;
						}
					}
				}
			}
		}
	}

	private static long findCornersProduct(List<String> input) {
		List<Tile> tiles = new ArrayList<>();
		List<String> borders = new ArrayList<>();
		long cornerProduct = 1;

		for (int i = 0; i < Math.round(input.size() / 12.0F); i++) {
			tiles.add(new Tile(input.subList(i * 12, (i * 12) + 11)));
		}

		for (Tile tile : tiles) {
			borders.addAll(tile.borders.values());
		}

		for (Tile tile : tiles) {
			int matches = (int) borders.stream().filter(border -> tile.borders.containsValue(border)).count() - 4;

			if (matches == 2) {
				cornerProduct *= tile.id;
			}
		}

		return cornerProduct;
	}

	private static boolean matchesBorder(String border1, String border2) {
		StringBuilder builder = new StringBuilder();

		String reversedBorder1 = builder.append(border1).reverse().toString();

		return border2.equals(border1) || border2.equals(reversedBorder1);
	}

	public static class Tile {

		int id;
		List<String> matrix;
		Map<Integer, String> borders;

		public Tile(List<String> input) {
			this.id = Integer.parseInt(input.get(0).split("[ :]")[1]);
			this.matrix = input.subList(1, input.size());
			this.borders = getBorders();
		}

		public void rotateMatrix(int degrees) {
			List<String> rotatedMatrix = new ArrayList<>();
			StringBuilder builder = new StringBuilder();

			for (int r = 0; r < degrees; r += 90) {

				for (int i = 0; i < matrix.size(); i++) {
					int finalI = i;

					builder.setLength(0);
					rotatedMatrix.add(builder.append(matrix.stream().map(row -> row.substring(finalI, finalI + 1)).collect(Collectors.joining())).reverse().toString());
				}

				matrix = new ArrayList<>(rotatedMatrix);
				rotatedMatrix.clear();
			}

			borders = getBorders();
		}

		private Map<Integer, String> getBorders() {
			Map<Integer, String> borders = new HashMap<>();

			borders.put(0, matrix.get(0));
			borders.put(90, matrix.stream().map(row -> row.substring(matrix.size() - 1)).collect(Collectors.joining()));
			borders.put(180, matrix.get(matrix.size() - 1));
			borders.put(270, matrix.stream().map(row -> row.substring(0, 1)).collect(Collectors.joining()));

			return borders;
		}
	}

}
