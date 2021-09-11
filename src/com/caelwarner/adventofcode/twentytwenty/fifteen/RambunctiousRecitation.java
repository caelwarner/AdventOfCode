package com.caelwarner.adventofcode.twentytwenty.fifteen;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class RambunctiousRecitation {

	public static void main(String[] args) {
		String input = Read.asString("adventofcode/twentytwenty/fifteen/input.txt");

		System.out.println(findNthPlaceEfficient(Arrays.stream(input.split(",")).map(Integer::parseInt).collect(Collectors.toList()), 30000000));
	}

	private static int findNthPlaceEfficient(List<Integer> input, int nth) {
		Map<Integer, Number> memory = new HashMap<>();
		Number previousNumber = null;

		for (int i = 0; i < nth; i++) {
			if (i < input.size()) {
				previousNumber = addToMemory(memory, new Number(input.get(i), i));
				
				continue;
			}
			
			if (previousNumber.timesSeen == 1) {
				previousNumber = addToMemory(memory, new Number(0, i));

			} else {
				previousNumber = addToMemory(memory, new Number(previousNumber.index - previousNumber.previousIndex, i));
			}
		}

		return previousNumber.number;
	}

	private static int findNthPlace(List<Integer> input, int nth) {
		List<Number> memory = new ArrayList<>();

		for (int i = 0; i < nth; i++) {
			if (i < input.size()) {
				memory.add(new Number(input.get(i), i));

				continue;
			}

			int finalI = i;
			if (memory.stream().filter(number -> number.number == memory.get(finalI - 1).number).count() == 1) {
				memory.add(new Number(0, i));

			} else {
				for (int j = memory.size() - 2; j >= 0; j--) {
					if (memory.get(i - 1).number == memory.get(j).number) {
						memory.add(new Number((i - 1) - memory.get(j).index, i));

						break;
					}
				}
			}
		}

		return memory.get(memory.size() - 1).number;
	}

	private static Number addToMemory(Map<Integer, Number> memory, Number number) {
		if (memory.containsKey(number.number)) {
			Number memoryNumber = memory.get(number.number);

			memoryNumber.previousIndex = memoryNumber.index;
			memoryNumber.index = number.index;

			memoryNumber.timesSeen++;

		} else {
			memory.put(number.number, number);
		}

		return memory.get(number.number);
	}

	public static class Number {

		public final int number;
		public int index;
		public int previousIndex;
		public int timesSeen = 1;

		public Number(int number, int index) {
			this.number = number;
			this.index = index;
		}
	}

}
