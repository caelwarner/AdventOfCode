package twentytwenty.fourteen;

import util.java.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DockingData {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/fourteen/input.txt");

		System.out.println(calculateMemorySumVersion2(input));
	}

	private static long calculateMemorySumVersion2(List<String> input) {
		Map<Long, Long> memory = new HashMap<>();
		char[] mask = new char[36];

		for (String line : input) {
			if (line.startsWith("mask")) {
				mask = line.split("= ")[1].toCharArray();

			} else if (line.startsWith("mem")) {
				List<Long> addresses = findAllAddresses(Integer.parseInt(line.split("[\\[\\]]")[1]), mask);
				long value = Long.parseLong(line.split("= ")[1]);

				for (Long address : addresses) {
					memory.put(address, value);
				}
			}
		}

		return memory.values().stream().reduce(0L, Long::sum);
	}

	private static long calculateMemorySumVersion1(List<String> input) {
		Map<Long, Long> memory = new HashMap<>();
		char[] mask = new char[36];

		for (String line : input) {
			if (line.startsWith("mask")) {
				mask = line.split("= ")[1].toCharArray();

			} else if (line.startsWith("mem")) {
				long address = Long.parseLong(line.split("[\\[\\]]")[1]);
				long value = bitmaskValue(Integer.parseInt(line.split("= ")[1]), mask);

				memory.put(address, value);
			}
		}

		return memory.values().stream().reduce(0L, Long::sum);
	}

	private static List<Long> findAllAddresses(int address, char[] mask) {
		mask = bitmaskAddress(address, mask);

		List<Long> addresses = new ArrayList<>();
		int floating = String.valueOf(mask).length() - String.valueOf(mask).replace("X", "").length();

		for (int i = 0; i < Math.pow(2, floating); i++) {
			char[] finalAddress = Arrays.copyOf(mask, 36);

			char[] floatingReplacement = Integer.toBinaryString((1 << floating) | i).substring(1).toCharArray();
			int replacementIndex = 0;

			for (int j = 0; j < finalAddress.length; j++) {
				if (finalAddress[j] == "X".charAt(0)) {
					finalAddress[j] = floatingReplacement[replacementIndex];
					replacementIndex++;
				}
			}

			addresses.add(Long.parseLong(String.valueOf(finalAddress), 2));
		}

		return addresses;
	}

	private static long bitmaskValue(int value, char[] mask) {
		char[] binary = new char[36];
		char[] masked = Integer.toBinaryString(value).toCharArray();

		Arrays.fill(binary, "0".charAt(0));
		System.arraycopy(masked, 0, binary, binary.length - masked.length, masked.length);

		for (int i = 0; i < binary.length; i++) {
			if (mask[i] == "1".charAt(0)) {
				binary[i] = "1".charAt(0);

			} else if (mask[i] == "0".charAt(0)) {
				binary[i] = "0".charAt(0);

			}
		}

		return Long.parseLong(String.valueOf(binary), 2);
	}

	private static char[] bitmaskAddress(int address, char[] mask) {
		char[] binary = new char[36];
		char[] masked = Integer.toBinaryString(address).toCharArray();

		Arrays.fill(binary, "0".charAt(0));
		System.arraycopy(masked, 0, binary, binary.length - masked.length, masked.length);

		for (int i = 0; i < binary.length; i++) {
			if (mask[i] == "1".charAt(0)) {
				binary[i] = "1".charAt(0);

			} else if (mask[i] == "X".charAt(0)) {
				binary[i] = "X".charAt(0);

			}
		}

		return binary;
	}
}
