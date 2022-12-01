package twentytwentyone.three;

import util.java.Read;

import java.util.ArrayList;
import java.util.List;

public class BinaryDiagnostic {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwentyone/three/input.txt");

		System.out.println(calculateLifeSupportRating(input));
	}

	private static int calculatePowerConsumption(List<String> input) {
		StringBuilder gamma = new StringBuilder();
		StringBuilder epsilon = new StringBuilder();

		for (int i = 0; i < input.get(0).length(); i++) {
			gamma.append(getMostCommonBit(input, i, true));
			epsilon.append(getMostCommonBit(input, i, false));
		}

		return Integer.parseInt(gamma.toString(), 2) * Integer.parseInt(epsilon.toString(), 2);
	}

	private static int calculateLifeSupportRating(List<String> input) {
		String oxygenGeneratorRating = getBitCriteriaString(input, true);
		String co2ScrubberRating = getBitCriteriaString(input, false);

		return Integer.parseInt(oxygenGeneratorRating, 2) * Integer.parseInt(co2ScrubberRating, 2);
	}

	private static char getMostCommonBit(List<String> input, int pos, boolean highBit) {
		int commonBit = 0;

		for (String line : input) {
			if (line.charAt(pos) == '1') {
				commonBit++;
			}
		}

		if (highBit)
			return commonBit >= input.size() / 2.0F ? '1' : '0';

		return commonBit >= input.size() / 2.0F ? '0' : '1';
	}

	private static String getBitCriteriaString(List<String> input, boolean mostCommon) {
		List<String> list = new ArrayList<>(input);
		List<String> tempList = new ArrayList<>();
		int pos = 0;

		while (list.size() > 1) {
			tempList.clear();

			char commonBit = getMostCommonBit(list, pos, true);

			for (String line : list) {
				if (mostCommon) {
					if (line.charAt(pos) == commonBit) {
						tempList.add(line);
					}
				} else {
					if (line.charAt(pos) != commonBit) {
						tempList.add(line);
					}
				}
			}

			list = new ArrayList<>(tempList);
			pos++;
		}

		return list.get(0);
	}

}
