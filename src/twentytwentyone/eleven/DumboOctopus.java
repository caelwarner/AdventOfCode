package twentytwentyone.eleven;

import util.java.Convert;
import util.java.Iterate;
import util.java.Read;

import java.util.ArrayList;
import java.util.List;

public class DumboOctopus {

	public static void main(String[] args) {
		List<List<Integer>> input = Read.as2DIntList("adventofcode/twentytwentyone/eleven/input.txt");

		System.out.println(findFirstStepAllFlash(input));
	}

	private static int countFlashes(List<List<Integer>> input) {
		Octopus[][] map = Convert.to2DArray(Octopus.class, input, Octopus::new);

		for (int step = 0; step < 100; step++) {
			Iterate.over2DArray(map, (octopus, row, col) -> octopus.energyLevel++);
			Iterate.over2DArray(map, (octopus, row, col) -> octopus.flash(map));
			Iterate.over2DArray(map, (octopus, row, col) -> {
				if (octopus.flashed) {
					octopus.energyLevel = 0;
					octopus.flashed = false;
				}
			});
		}

		return Octopus.totalFlashes;
	}

	private static int findFirstStepAllFlash(List<List<Integer>> input) {
		Octopus[][] map = Convert.to2DArray(Octopus.class, input, Octopus::new);
		int step = 0;

		while (Octopus.flashedOctpuses.size() < map.length * map[0].length) {
			Octopus.flashedOctpuses.clear();

			Iterate.over2DArray(map, ((octopus, row, col) -> octopus.energyLevel++));
			Iterate.over2DArray(map, (octopus, row, col) -> octopus.flash(map));
			Iterate.over2DArray(map, ((octopus, row, col) -> {
				if (octopus.flashed) {
					octopus.energyLevel = 0;
					octopus.flashed = false;
				}
			}));

			step++;
		}

		return step;
	}

	private static class Octopus {

		private static int totalFlashes = 0;
		private static final List<Octopus> flashedOctpuses = new ArrayList<>();

		public int energyLevel;
		public int x;
		public int y;

		private boolean flashed = false;

		public Octopus(int energyLevel, int y, int x) {
			this.energyLevel = energyLevel;
			this.x = x;
			this.y = y;
		}

		public void flash(Octopus[][] map) {
			if (energyLevel <= 9 || flashed)
				return;

			this.flashed = true;
			totalFlashes++;
			flashedOctpuses.add(this);

			Iterate.around2DArrayDiagonally(map, y, x, (neighbourOctopus, row, col) -> {
				neighbourOctopus.energyLevel++;
				neighbourOctopus.flash(map);
			});
		}

	}
}
