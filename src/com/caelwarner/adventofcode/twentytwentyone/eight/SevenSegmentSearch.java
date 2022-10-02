package com.caelwarner.adventofcode.twentytwentyone.eight;

import com.caelwarner.util.Read;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SevenSegmentSearch {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwentyone/eight/input.txt");

		System.out.println(findOutputValuesSum(input));
	}

	/*
	 * 1. Find 1, 4, 7, 8 through unique lengths
	 * 2. Find 6 since it's length 6 and only has 1 match with 1. Find 5 since it's length 5 and has 3 matches with 4.
	 *    Find 3 since it's length 5 and has 3 matches with 7. Find 9 since it's length 6 and has 4 matches with 4
	 * 3. Find 2 since it's length 5 and it's not 3 or 5. Find 0 since it's length 6 and it's not 6 or 9
	 */

	private static int countEasyDigits(List<String> input) {
		int easyDigits = 0;

		for (String line : input) {
			String[] digits = line.split(" \\| ")[1].split(" ");

			for (String digit : digits) {
				if (digit.length() == 2 || digit.length() == 3 || digit.length() == 4 || digit.length() == 7) {
					easyDigits++;
				}
			}
		}

		return easyDigits;
	}

	private static long findOutputValuesSum(List<String> input) {
		long sum = 0;

		for (String line : input) {
			String[] signalPatterns = line.split(" \\| ")[0].split(" ");
			String[] digits = line.split(" \\| ")[1].split(" ");
			String[] guide = new String[10];

			// Loop 1
			for (String signalPattern : signalPatterns) {
				int length = signalPattern.length();

				if (length == 2)
					guide[1] = signalPattern;

				else if (length == 3)
					guide[7] = signalPattern;

				else if (length == 4)
					guide[4] = signalPattern;

				else if (length == 7)
					guide[8] = signalPattern;
			}

			// Loop 2
			for (String signalPattern : signalPatterns) {
				int length = signalPattern.length();

				if (length == 6 && matches(signalPattern, guide[1]) == 1)
					guide[6] = signalPattern;

				else if (length == 5 && matches(signalPattern, guide[4]) == 3 && matches(signalPattern, guide[1]) == 1)
					guide[5] = signalPattern;

				else if (length == 5 && matches(signalPattern, guide[7]) == 3)
					guide[3] = signalPattern;

				else if (length == 6 && matches(signalPattern, guide[4]) == 4)
					guide[9] = signalPattern;
			}

			// Loop 3
			for (String signalPattern : signalPatterns) {
				int length = signalPattern.length();

				if (length == 5 && !signalPattern.equals(guide[3]) && !signalPattern.equals(guide[5]))
					guide[2] = signalPattern;

				else if (length == 6 && !signalPattern.equals(guide[6]) && !signalPattern.equals(guide[9]))
					guide[0] = signalPattern;
			}

			int multiplier = 1000;
			int outputValue = 0;

			for (String digit : digits) {
				for (int i = 0; i < guide.length; i++) {
					if (digit.replaceAll("[" + guide[i] + "]", "").equals("") && digit.length() == guide[i].length()) {
						outputValue += i * multiplier;
						break;
					}
				}

				multiplier /= 10;
			}

			sum += outputValue;
		}

		return sum;
	}

	private static int matches(String input, String regexInput) {
		Pattern pattern = Pattern.compile("[" + regexInput + "]");
		Matcher matcher = pattern.matcher(input);

		return (int) matcher.results().count();
	}

}
