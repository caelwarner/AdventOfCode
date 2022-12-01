package twentytwenty.seventeen;

import util.java.Read;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ConwayCubes {

	public static final char inactive = ".".charAt(0);
	public static final char active = "#".charAt(0);

	public static void main(String[] args) {
		List<List<Character>> input = Read.as2DCharacterList("adventofcode/twentytwenty/seventeen/input.txt");

		System.out.println(findActiveCubes4D(input));
	}

	private static int findActiveCubes4D(List<List<Character>> input) {
		List<List<List<List<Character>>>> dimension = new ArrayList<List<List<List<Character>>>>() {{
			List<List<List<Character>>> input3D = new ArrayList<List<List<Character>>>() {{
				add(new ArrayList<>(blank2DArray(input)));
				add(input);
				add(new ArrayList<>(blank2DArray(input)));
			}};

			add(new ArrayList<>(blank3DArray(input3D)));
			add(input3D);
			add(new ArrayList<>(blank3DArray(input3D)));
		}};

		for (int j = 0; j < 6; j++) {
			grow4DDimension(dimension);
		}

		for (int i = 0; i < 6; i++) {
			grow4DDimension(dimension);

			List<List<List<List<Character>>>> nextDimension = copy4DArray(dimension);

			for (int w = 0; w < dimension.size(); w++) {
				for (int z = 0; z < dimension.size(); z++) {
					for (int y = 0; y < dimension.size(); y++) {
						for (int x = 0; x < dimension.size(); x++) {
							int activeCubes = getNearbyActiveCubes(x, y, z, w, dimension);

							if (activeCubes == 3 && dimension.get(w).get(z).get(y).get(x) == inactive) {
								nextDimension.get(w).get(z).get(y).set(x, active);

							} else if (!(activeCubes == 2 || activeCubes == 3) && dimension.get(w).get(z).get(y).get(x) == active) {
								nextDimension.get(w).get(z).get(y).set(x, inactive);
							}
						}
					}
				}
			}

			dimension = copy4DArray(nextDimension);
		}

		return dimension.stream()
				.mapToInt(z -> z.stream()
						.mapToInt(y -> y.stream()
								.mapToInt(x -> (int) x.stream()
										.filter(c -> c == active)
										.count()
								).sum()
						).sum()
				).sum();
	}

	private static int findActiveCubes3D(List<List<Character>> input) {
		List<List<List<Character>>> dimension = new ArrayList<List<List<Character>>>() {{
			add(new ArrayList<>(blank2DArray(input)));
			add(input);
			add(new ArrayList<>(blank2DArray(input)));
		}};

		for (int j = 0; j < 6; j++) {
			grow3DDimension(dimension);
		}

		for (int i = 0; i < 6; i++) {
			grow3DDimension(dimension);

			List<List<List<Character>>> nextDimension = copy3DArray(dimension);

			for (int z = 0; z < dimension.size(); z++) {
				for (int y = 0; y < dimension.size(); y++) {
					for (int x = 0; x < dimension.size(); x++) {
						int activeCubes = getNearbyActiveCubes(x, y, z, dimension);

						if (activeCubes == 3 && dimension.get(z).get(y).get(x) == inactive) {
							nextDimension.get(z).get(y).set(x, active);

						} else if (!(activeCubes == 2 || activeCubes == 3) && dimension.get(z).get(y).get(x) == active) {
							nextDimension.get(z).get(y).set(x, inactive);
						}
					}
				}
			}

			dimension = copy3DArray(nextDimension);
		}

		return dimension.stream()
				.mapToInt(y -> y.stream()
						.mapToInt(x -> (int) x.stream()
								.filter(c -> c == active)
								.count()
						).sum()
				).sum();
	}

	private static int getNearbyActiveCubes(int xPos, int yPos, int zPos, List<List<List<Character>>> input) {
		int activeCubes = 0;

		for (int z = -1; z <= 1; z++) {
			for (int y = -1; y <= 1; y++) {
				for (int x = -1; x <= 1; x++) {
					if (x == 0 && y == 0 && z == 0)
						continue;

					try {
						if (input.get(zPos + z).get(yPos + y).get(xPos + x) == active)
							activeCubes++;

					} catch (IndexOutOfBoundsException ignored) {}
				}
			}
		}

		return activeCubes;
	}

	private static int getNearbyActiveCubes(int xPos, int yPos, int zPos, int wPos, List<List<List<List<Character>>>> input) {
		int activeCubes = 0;

		for (int w = -1; w <= 1; w++) {
			for (int z = -1; z <= 1; z++) {
				for (int y = -1; y <= 1; y++) {
					for (int x = -1; x <= 1; x++) {
						if (x == 0 && y == 0 && z == 0 && w == 0)
							continue;

						try {

							if (input.get(wPos + w).get(zPos + z).get(yPos + y).get(xPos + x) == active)
								activeCubes++;

						} catch (IndexOutOfBoundsException ignored) {}
					}
				}
			}
		}

		return activeCubes;
	}

	private static void printDimension(List<List<List<Character>>> dimension) {
		for (List<List<Character>> ySlice : dimension) {
			for (List<Character> xSlice : ySlice) {
				for (Character character : xSlice) {
					System.out.print(character);
				}

				System.out.print("\n");
			}

			System.out.print("\n");
		}
	}

	private static void grow3DDimension(List<List<List<Character>>> dimension) {
		for (List<List<Character>> ySlice : dimension) {
			for (List<Character> xSlice : ySlice) {
				xSlice.add(0, inactive);
				xSlice.add(inactive);
			}

			ySlice.add(0, new ArrayList<>(blank1DArray(ySlice.get(0))));
			ySlice.add(new ArrayList<>(blank1DArray(ySlice.get(0))));
		}

		dimension.add(0, new ArrayList<>(blank2DArray(dimension.get(0))));
		dimension.add(new ArrayList<>(blank2DArray(dimension.get(0))));
	}

	private static void grow4DDimension(List<List<List<List<Character>>>> dimension) {
		for (List<List<List<Character>>> zSlice : dimension) {
			for (List<List<Character>> ySlice : zSlice) {
				for (List<Character> xSlice : ySlice) {
					xSlice.add(0, inactive);
					xSlice.add(inactive);
				}

				ySlice.add(0, new ArrayList<>(blank1DArray(ySlice.get(0))));
				ySlice.add(new ArrayList<>(blank1DArray(ySlice.get(0))));
			}

			zSlice.add(0, new ArrayList<>(blank2DArray(zSlice.get(0))));
			zSlice.add(new ArrayList<>(blank2DArray(zSlice.get(0))));
		}

		dimension.add(0, new ArrayList<>(blank3DArray(dimension.get(0))));
		dimension.add(new ArrayList<>(blank3DArray(dimension.get(0))));
	}

	private static void trim4DDimension(List<List<List<List<Character>>>> dimension) {

	}

	private static List<Character> blank1DArray(List<Character> input) {
		return input.stream()
				.map(c -> c = inactive)
				.collect(Collectors.toList());
	}

	private static List<List<Character>> blank2DArray(List<List<Character>> input) {
		return input.stream()
				.map(x -> new ArrayList<>(x.stream()
						.map(c -> c = inactive)
						.collect(Collectors.toList())))
				.collect(Collectors.toList());
	}

	private static List<List<List<Character>>> blank3DArray(List<List<List<Character>>> input) {
		return copy3DArray(input.stream()
				.map(y -> y.stream()
						.map(x -> x.stream()
								.map(c -> c = inactive)
								.collect(Collectors.toList()))
						.collect(Collectors.toList()))
				.collect(Collectors.toList()));
	}

	private static List<List<List<Character>>> copy3DArray(List<List<List<Character>>> input) {
		List<List<List<Character>>> copiedArray = new ArrayList<>();

		for (List<List<Character>> ySlice : input) {
			List<List<Character>> copiedYSlice = new ArrayList<>();

			for (List<Character> xSlice : ySlice) {
				copiedYSlice.add(new ArrayList<>(xSlice));
			}

			copiedArray.add(copiedYSlice);
		}

		return copiedArray;
	}

	private static List<List<List<List<Character>>>> copy4DArray(List<List<List<List<Character>>>> input) {
		List<List<List<List<Character>>>> copiedArray = new ArrayList<>();

		for (List<List<List<Character>>> zSlice : input) {
			copiedArray.add(copy3DArray(zSlice));
		}

		return copiedArray;
	}

}
