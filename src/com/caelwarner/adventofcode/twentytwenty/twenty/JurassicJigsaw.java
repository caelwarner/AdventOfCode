package com.caelwarner.adventofcode.twentytwenty.twenty;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class JurassicJigsaw {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwenty/twenty/input.txt");

		System.out.println(findCornersProduct(input));
	}

	private static long findCornersProduct(List<String> input) {
		List<Tile> tiles = new ArrayList<>();
		List<String> borders = new ArrayList<>();
		long cornerProduct = 1;

		for (int i = 0; i < Math.round(input.size() / 12.0F); i++) {
			tiles.add(new Tile(input.subList(i * 12, (i * 12) + 11)));
		}

		for (Tile tile : tiles) {
			borders.addAll(tile.getBorders());
		}

		for (Tile tile : tiles) {
			int matches = (int) (borders.stream().filter(border -> tile.getBorders().contains(border)).count() - 8) / 2;

			if (matches == 2) {
				cornerProduct *= tile.id;
			}
		}

		return cornerProduct;
	}

	public static class Tile {

		int id;
		List<String> matrix;

		public Tile(List<String> input) {
			this.id = Integer.parseInt(input.get(0).split("[ :]")[1]);
			this.matrix = input.subList(1, input.size());
		}

		public List<String> getBorders() {
			List<String> borders = new ArrayList<>();
			List<String> reveresedBorders = new ArrayList<>();

			borders.add(matrix.get(0));
			borders.add(matrix.get(matrix.size() - 1));
			borders.add(matrix.stream().map(row -> row.substring(0, 1)).collect(Collectors.joining()));
			borders.add(matrix.stream().map(row -> row.substring(matrix.size() - 1)).collect(Collectors.joining()));

			for (String border : borders) {
				StringBuilder builder = new StringBuilder();

				builder.append(border);
				builder.reverse();

				reveresedBorders.add(builder.toString());
			}

			borders.addAll(reveresedBorders);

			return borders;
		}
	}

}
