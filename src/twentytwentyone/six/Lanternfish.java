package twentytwentyone.six;

import util.java.Read;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Lanternfish {

	public static void main(String[] args) {
		String input = Read.asString("adventofcode/twentytwentyone/six/input.txt");

		System.out.println(simulateFishFast(input));
	}

	private static int simulateFish(String input) {
		List<Integer> fish = Arrays.stream(input.split(",")).map(Integer::parseInt).collect(Collectors.toList());

		for (int days = 0; days <= 80; days++) {
			int newFish = 0;

			for (int i = 0; i < fish.size(); i++) {
				if (fish.get(i) < 0) {
					fish.set(i, 6);
					newFish++;
				}

				fish.set(i, fish.get(i) - 1);
			}

			for (int i = 0; i < newFish; i++) {
				fish.add(7);
			}
		}

		return fish.size();
	}

	private static long simulateFishFast(String input) {
		long[] fish = new long[9];

		for (int i : Arrays.stream(input.split(",")).mapToInt(Integer::parseInt).toArray()) {
			fish[i]++;
		}

		for (int days = 0; days <= 256; days++) {
			long newFish = fish[0];

			System.arraycopy(fish, 1, fish, 0, fish.length - 1);

			fish[6] += newFish;
			fish[8] = newFish;
		}

		return Arrays.stream(fish).sum() - fish[8];
	}

}
