package twentytwentyone.twelve;

import util.java.Read;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class PassagePathing {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwentyone/twelve/input.txt");

		System.out.println(findUniquePaths(input));
	}

	private static int findUniquePaths(List<String> input) {
		Set<Cave> caves = new HashSet<>();

		for (String line : input) {
			String[] caveNames = line.split("-");

			for (int i = 0; i < 2; i++) {
				Cave cave = getOrCreateCave(caves, caveNames[i]);
				cave.connected.add(getOrCreateCave(caves, caveNames[(i + 1) % 2]));
			}
		}

		Cave start = getOrCreateCave(caves, "start");
		Cave end = getOrCreateCave(caves, "end");

		return start.walk(new ArrayList<>(), new ArrayList<>(), end, 0);
	}

	private static Cave getOrCreateCave(Set<Cave> caves, String caveName) {
		Optional<Cave> optional = caves.stream()
				.filter(c -> c.name.equals(caveName))
				.findFirst();

		if (optional.isEmpty()) {
			Cave cave = new Cave(caveName, Character.isUpperCase(caveName.charAt(0)));
			caves.add(cave);

			return cave;
		}

		return optional.get();
	}

	private static class Cave {

		final String name;
		final boolean isLarge;

		final List<Cave> connected = new ArrayList<>();

		public Cave(String name, boolean isLarge) {
			this.name = name;
			this.isLarge = isLarge;
		}

		private int walk(List<Cave> visited, List<Cave> smallCaves, Cave end, int paths) {
			if (!isLarge && smallCaves.size() - 1 > new HashSet<>(smallCaves).size())
				return paths;

			if (this == end)
				return 1;

			visited.add(this);
			if (!isLarge)
				smallCaves.add(this);

			int totalPaths = 0;

			for (Cave cave : connected) {
				if (!cave.name.equals("start"))
					totalPaths += cave.walk(visited, smallCaves, end, paths);
			}

			visited.remove(visited.size() - 1);
			if (!isLarge)
				smallCaves.remove(smallCaves.size() - 1);

			return totalPaths;
		}

		@Override
		public String toString() {
			return name;
		}

	}
}
