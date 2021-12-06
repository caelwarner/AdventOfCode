package com.caelwarner.adventofcode.twentytwentyone.two;

import com.caelwarner.util.Read;

import java.util.List;

public class Dive {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwentyone/two/input.txt");

		System.out.println(getDistanceAdvanced(input));
	}

	private static int getDistance(List<String> input) {
		int horizontalPosition = 0;
		int depth = 0;

		for (String line : input) {
			String command = line.split(" ")[0];
			int value = Integer.parseInt(line.split(" ")[1]);

			if (command.equals("forward")) {
				horizontalPosition += value;

			} else if (command.equals("up")) {
				depth -= value;

			} else if (command.equals("down")) {
				depth += value;
			}
		}

		return horizontalPosition * depth;
	}

	private static int getDistanceAdvanced(List<String> input) {
		int horizontalPosition = 0;
		int depth = 0;
		int aim = 0;

		for (String line : input) {
			String command = line.split(" ")[0];
			int value = Integer.parseInt(line.split(" ")[1]);

			if (command.equals("forward")) {
				horizontalPosition += value;
				depth += aim * value;

			} else if (command.equals("up")) {
				aim -= value;

			} else if (command.equals("down")) {
				aim += value;
			}
		}

		return horizontalPosition * depth;
	}

}
