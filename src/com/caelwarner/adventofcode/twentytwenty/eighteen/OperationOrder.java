package com.caelwarner.adventofcode.twentytwenty.eighteen;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class OperationOrder {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/eighteen/input.txt");

		System.out.println(sumOfExpressions(input));
	}

	private static long sumOfExpressions(List<String> input) {
		long sum = 0;

		for (String line : input) {
			sum += calculateExpression(changeOrderOfOperations(line));
		}

		return sum;
	}

	private static String changeOrderOfOperations(String input) {
		List<String> array = new ArrayList<>(Arrays.asList(input.replace("(", "( ").replace(")", " )").split(" ")));

		for (int i = 0; i < array.size(); i++) {
			String term = array.get(i);

			if (!term.equals("+")) {
				continue;
			}

			int level = 0;

			for (int j = 1; j < array.size(); j++) {
				String checkTerm = array.get(i - j);

				if (checkTerm.equals(")")) {
					level++;

				} else if (checkTerm.equals("(")) {
					level--;

					if (level == 0) {
						array.add(i - j, "(");
						i++;

						break;
					}

				} else if (level == 0 && !checkTerm.equals("+")) {
					if ((i - j) > 0 && !array.get(i - j - 1).equals("+")) {
						array.add(i - j, "(");
						i++;

						break;
					} else if ((i - j) == 0) {
						array.add(0, "(");
						i++;

						break;
					}
				}
			}

			level = 0;

			for (int j = 1; j < array.size(); j++) {
				String checkTerm = array.get(i + j);

				if (checkTerm.equals("(")) {
					level++;

				} else if (checkTerm.equals(")")) {
					level--;

					if (level == 0) {
						array.add(i + j, ")");

						break;
					}

				} else if (level == 0 && !checkTerm.equals("+")) {
					if ((i + j) < array.size() - 1 && !array.get(i + j + 1).equals("+")) {
						array.add(i + j + 1, ")");

						break;

					} else if ((i + j) == array.size() - 1) {
						array.add(")");

						break;
					}
				}
			}
		}

		return String.join(" ", array).replace("( ", "(").replace(" )", ")");
	}

	private static long calculateExpression(String input) {
		long value = 1;
		String nextOperation = "*";

		String[] array = input.replace("(", "( ").replace(")", " )").split(" ");

		mainLoop:
		for (int i = 0; i < array.length; i++) {
			String term = array[i];

			if (term.equals("*")) {
				nextOperation = "*";

			} else if (term.equals("+")) {
				nextOperation = "+";

			} else if (term.equals("(")) {
				int level = 0;

				for (int j = 1; j < array.length; j++) {
					String nestedTerm = array[i + j];

					if (nestedTerm.equals("(")) {
						level++;

					} else if (nestedTerm.equals(")")) {
						if (level == 0) {
							String substring = String.join(" ", Arrays.copyOfRange(array, i + 1, i + j)).replace("( ", "(").replace(" )", ")");

							if (nextOperation.equals("*")) {
								value *= calculateExpression(substring);

							} else if (nextOperation.equals("+")) {
								value += calculateExpression(substring);

							}

							i += j;

							continue mainLoop;
						}

						level--;
					}
				}

			} else {
				if (nextOperation.equals("*")) {
					value *= Integer.parseInt(term);

				} else if (nextOperation.equals("+")) {
					value += Integer.parseInt(term);
				}
			}
		}

		return value;
	}

}
