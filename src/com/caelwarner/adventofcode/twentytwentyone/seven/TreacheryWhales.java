package com.caelwarner.adventofcode.twentytwentyone.seven;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TreacheryWhales {

	public static void main(String[] args) {
		String input = Read.asString("adventofcode/twentytwentyone/seven/input.txt");

		System.out.println(getLowestFuel(input));
	}

	private static int getLowestFuel(String input) {
		int[] crabPositions = Arrays.stream(input.split(",")).mapToInt(Integer::parseInt).toArray();
		List<Integer> possibleFuelOutcomes = new ArrayList<>();

		for (int i = 1; i <= Arrays.stream(crabPositions).max().orElse(0); i++) {
			int fuel = 0;

			for (int pos : crabPositions) {
				int difference = Math.abs(pos - i);
				fuel += (int) ((difference * (difference + 1)) / 2.0F);
			}

			possibleFuelOutcomes.add(fuel);
		}

		return possibleFuelOutcomes.stream().mapToInt(fuel -> fuel).min().orElse(0);
	}

}
