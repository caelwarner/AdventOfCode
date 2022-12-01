package twentytwentyone.one;

import util.java.Read;

import java.util.List;

public class SonarSweep {

	public static void main(String[] args) {
		List<Integer> input = Read.asIntList("adventofcode/twentytwentyone/one/input.txt");

		System.out.println(countDepthSumIncreases(input));
	}

	private static int countDepthIncreases(List<Integer> input) {
		int previousDepth = 0;
		int depthIncreases = 0;

		for (int depth : input) {
			if (depth > previousDepth) {
				depthIncreases++;
			}

			previousDepth = depth;
		}

		return depthIncreases - 1;
	}

	private static int countDepthSumIncreases(List<Integer> input) {
		int previousDepthSum = 0;
		int depthSumIncreases = 0;

		for (int i = 0; i < input.size() - 2; i++) {
			int sum = input.get(i) + input.get(i + 1) + input.get(i + 2);

			if (sum > previousDepthSum) {
				depthSumIncreases++;
			}

			previousDepthSum = sum;
		}

		return depthSumIncreases - 1;
	}

}
