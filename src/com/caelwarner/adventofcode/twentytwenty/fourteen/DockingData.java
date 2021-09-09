package com.caelwarner.adventofcode.twentytwenty.fourteen;

import com.caelwarner.util.Read;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DockingData {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwenty/fourteen/input.txt");

		System.out.println(calculateMemorySumVersion1(input));
	}

	private static long calculateMemorySumVersion1(List<String> input) {
		char[] mask = new char[36];
		Map<Integer, Long> memory = new HashMap<>();

		for (String line : input) {
			if (line.startsWith("mask")) {
				mask = line.split("= ")[1].toCharArray();

			} else if (line.startsWith("mem")) {
				char[] binary = new char[36];
				char[] value = Integer.toBinaryString(Integer.parseInt(line.split("= ")[1])).toCharArray();

				Arrays.fill(binary, "0".charAt(0));
				System.arraycopy(value, 0, binary, binary.length - value.length, value.length);

				for (int i = 0; i < binary.length; i++) {
					if (mask[i] == "1".charAt(0)) {
						binary[i] = "1".charAt(0);

					} else if (mask[i] == "0".charAt(0)) {
						binary[i] = "0".charAt(0);

					}
				}

				int location = Integer.parseInt(line.split("[\\[\\]]")[1]);
				long finalValue = Long.parseLong(String.valueOf(binary), 2);

				memory.put(location, finalValue);
			}
		}

		return memory.values().stream().reduce(0L, Long::sum);
	}

}
