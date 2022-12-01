package twentytwentyone.thirteen;

import util.java.Iterate;
import util.java.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class TransparentOrigami {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwentyone/thirteen/input.txt");

		System.out.println(Arrays.deepToString(completeAllFolds(input)));
	}

	private static int foldOnceCountDots(List<String> input) {
		AtomicInteger dotsNumber = new AtomicInteger();
		int maxX = 0;
		int maxY = 0;

		List<Dot> dots = parseDots(input);
		List<FoldInstruction> instructions = parseInstructions(input);

		maxX = dots.stream().map(Dot::x).max(Integer::compare).orElse(0);
		maxY = dots.stream().map(Dot::y).max(Integer::compare).orElse(0);
		char[][] map = createBlank2DArray(maxX + 1, maxY + 1);

		for (Dot dot : dots)
			map[dot.y()][dot.x()] = '#';

		map = instructions.get(0).fold(map);

		Iterate.over2DArray(map, (point, row, col) -> {
			if (point == '#')
				dotsNumber.getAndIncrement();
		});

		return dotsNumber.get();
	}

	private static char[][] completeAllFolds(List<String> input) {
		int maxX = 0;
		int maxY = 0;

		List<Dot> dots = parseDots(input);
		List<FoldInstruction> instructions = parseInstructions(input);

		maxX = dots.stream().map(Dot::x).max(Integer::compare).orElse(0);
		maxY = dots.stream().map(Dot::y).max(Integer::compare).orElse(0);
		char[][] map = createBlank2DArray(maxX + 1, maxY + 1);

		for (Dot dot : dots)
			map[dot.y()][dot.x()] = '#';

		for (FoldInstruction instruction : instructions) {
			map = instruction.fold(map);
		}

		return map;
	}

	private static List<Dot> parseDots(List<String> input) {
		List<Dot> dots = new ArrayList<>();

		for (String line : input) {
			if (!line.startsWith("fold along") && !line.equals("")) {
				String[] dot = line.split(",");
				dots.add(new Dot(Integer.parseInt(dot[0]), Integer.parseInt(dot[1])));
			}
		}

		return dots;
	}

	private static List<FoldInstruction> parseInstructions(List<String> input) {
		List<FoldInstruction> instructions = new ArrayList<>();

		for (String line : input) {
			if (line.startsWith("fold along")) {
				String[] instruction = line.split("fold along ")[1].split("=");
				instructions.add(new FoldInstruction(instruction[0], Integer.parseInt(instruction[1])));
			}
		}

		return instructions;
	}

	private static char[][] createBlank2DArray(int width, int height) {
		char[][] map = new char[height][width];
		for (char[] row : map)
			Arrays.fill(row, '.');

		return map;
	}

	private record Dot(int x, int y) {}

	private record FoldInstruction(String axis, int location) {

		@SuppressWarnings("DuplicatedCode")
		private char[][] fold(char[][] map) {
			char[][] foldedMap;

			if (axis.equals("y")) {
				foldedMap = createBlank2DArray(map[0].length, location);

				Iterate.over2DArray(map, (point, row, col) -> {
					if (row < location)
						foldedMap[row][col] = point;

					else if (row > location) {
						int reflectedRow = ((row - location) * -1) + location;

						if (foldedMap[reflectedRow][col] == '.')
							foldedMap[reflectedRow][col] = point;
					}
				});

			} else {
				foldedMap = createBlank2DArray(location, map.length);

				Iterate.over2DArray(map, (point, row, col) -> {
					if (col < location)
						foldedMap[row][col] = point;

					else if (col > location) {
						int reflectedCol = ((col - location) * -1) + location;

						if (foldedMap[row][reflectedCol] == '.')
							foldedMap[row][reflectedCol] = point;
					}
				});
			}

			return foldedMap;
		}
	}

}
