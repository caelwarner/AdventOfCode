package twentytwentyone.nine;

import util.java.Convert;
import util.java.Iterate;
import util.java.Read;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class SmokeBasin {

	public static void main(String[] args) {
		List<List<Integer>> input = Read.as2DIntList("twentytwentyone/nine/input.txt");

		System.out.println(findLargestBasins(input));
	}

	private static int getRiskLevelsSum(List<List<Integer>> input) {
		int[][] map = Convert.to2DArray(input);

		return findLowestPoints(map).stream().mapToInt(point -> point.height() + 1).sum();
	}

	private static long findLargestBasins(List<List<Integer>> input) {
		List<Point> lowestPoints = findLowestPoints(Convert.to2DArray(input));
		Point[][] map = new Point[input.size()][input.get(0).size()];

		for (int row = 0; row < map.length; row++) {
			for (int col = 0; col < map[row].length; col++) {
				map[row][col] = new Point(input.get(row).get(col), col, row);
			}
		}

		List<Integer> basinSizes = lowestPoints.stream()
				.map(point -> map[point.y][point.x].walkBasin(map, new ArrayList<>()))
				.sorted(Collections.reverseOrder())
				.toList();

		long product = 1;

		for (int i = 0; i < 3; i++) {
			product *= basinSizes.get(i);
		}

		return product;
	}

	private static List<Point> findLowestPoints(int[][] map) {
		List<Point> lowestPoints = new ArrayList<>();

		int height = map.length;
		int width = map[0].length;

		for (int row = 0; row < height; row++) {
			for (int col = 0; col < width; col++) {
				int currentHeight = map[row][col];

				// Above
				if (row > 0 && currentHeight >= map[row - 1][col])
					continue;

				// Below
				if (row + 1 < height && currentHeight >= map[row + 1][col])
					continue;

				// Left
				if (col > 0 && currentHeight >= map[row][col - 1])
					continue;

				// Right
				if (col + 1 < width && currentHeight >= map[row][col + 1])
					continue;

				lowestPoints.add(new Point(currentHeight, col, row));
			}
		}

		return lowestPoints;
	}

	private record Point(int height, int x, int y) {
		public int walkBasin(Point[][] map, List<Point> visited) {
			if (height == 9 || visited.contains(this))
				return 0;

			visited.add(this);
			AtomicInteger size = new AtomicInteger(1);

			Iterate.around2DArray(map, y, x, (neighbour, row, col) -> {
				size.addAndGet(neighbour.walkBasin(map, visited));
			});

			return size.get();
		}

	}

}
