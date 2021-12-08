package com.caelwarner.adventofcode.twentytwentyone.five;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class HydrothermalVenture {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwentyone/five/input.txt");

		System.out.println(findSafestPath(input));
	}

	private static int findSafestPath(List<String> input) {
		List<Line> lines = new ArrayList<>();
		input.forEach(line -> lines.add(new Line(line)));

		FieldMap map = new FieldMap(lines.stream().mapToInt(Line::getLargestCoord).max().orElse(0) + 1);
		lines.forEach(line -> map.markLine(line.getCoords()));

		return map.getSafePoints();
	}

	private static class FieldMap {
		private final int[][] map;

		public FieldMap(int mapSize) {
			this.map = new int[mapSize][mapSize];
		}

		public void markLine(int[][] line) {
			if (line == null) {
				return;
			}

			for (int[] coord : line) {
				map[coord[1]][coord[0]]++;
			}
		}

		public int getSafePoints() {
			return Arrays.stream(map).mapToInt(row -> (int) Arrays.stream(row).filter(location -> location >= 2).count()).sum();
		}
	}

	private static class Line {
		public int x1;
		public int y1;
		public int x2;
		public int y2;

		public Line(String input) {
			List<String> coords = Arrays.stream(input.split("(,|->| )")).filter(value -> !value.equals("")).collect(Collectors.toList());

			x1 = Integer.parseInt(coords.get(0));
			y1 = Integer.parseInt(coords.get(1));
			x2 = Integer.parseInt(coords.get(2));
			y2 = Integer.parseInt(coords.get(3));
		}

		public int getLargestCoord() {
			return Arrays.stream(new int[]{x1, y1, x2, y2}).max().getAsInt();
		}

		public int[][] getCoords() {
			if (x1 == x2) {
				int[][] coords = new int[Math.abs(y2 - y1) + 1][2];

				for (int i = 0; i < coords.length; i++) {
					int increment = y1 > y2 ? i * -1 : i;

					coords[i][0] = x1;
					coords[i][1] = y1 + increment;
				}

				return coords;

			} else if (y1 == y2) {
				int[][] coords = new int[Math.abs(x2 - x1) + 1][2];

				for (int i = 0; i < coords.length; i++) {
					int increment = x1 > x2 ? i * -1 : i;

					coords[i][0] = x1 + increment;
					coords[i][1] = y1;
				}

				return coords;

			} else {
				int[][] coords = new int[Math.abs(x2 - x1) + 1][2];

				for (int i = 0; i < coords.length; i++) {
					int xIncrement = x1 > x2 ? i * -1 : i;
					int yIncrement = y1 > y2 ? i * -1 : i;

					coords[i][0] = x1 + xIncrement;
					coords[i][1] = y1 + yIncrement;
				}

				return coords;
			}
		}
	}

}
