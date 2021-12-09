package com.caelwarner.adventofcode.twentytwentyone.eight;

import com.caelwarner.util.Read;

import java.util.List;

public class SevenSegmentSearch {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwentyone/eight/input.txt");

		System.out.println(countEasyDigits(input));
	}

	/*
	 * 1. find A by removing 1 from 7
	 * 2. find C by removing 6 from 1. 6 can be found since it's the only 6 segment digit where 1 - 6 != 0
	 * 3. find E by it'll be the only missing segment in 5 + 1. 5 can be found since it's the only 5 segment digit that is missing C
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

//	private static int findOutputValuesSum(List<String> input) {
//
//	}

	private static class SevenSegmentDisplay {

		String a = "";
		String b = "";
		String c = "";
		String d = "";
		String e = "";
		String f = "";
		String g = "";

	}

}
