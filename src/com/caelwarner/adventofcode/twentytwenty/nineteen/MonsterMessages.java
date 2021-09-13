package com.caelwarner.adventofcode.twentytwenty.nineteen;

import com.caelwarner.util.Read;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class MonsterMessages {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwenty/nineteen/input.txt");

		System.out.println(countValidMessages(input));
	}

	private static int countValidMessagesChangedRules(List<String> input) {
		int validMessages = 0;

		Map<Integer, Rule> ruleMap = buildRuleMap(input);

		Pattern pattern42 = Pattern.compile(generateRegex(42, ruleMap));
		Pattern pattern31 = Pattern.compile(generateRegex(31, ruleMap));

		for (String line : input.subList(input.indexOf("") + 1, input.size())) {
			int index = 0;

			Matcher matcher42 = pattern42.matcher(line);
			int count42 = 0;


			while (matcher42.find()) {
				count42++;
				index += matcher42.end() - matcher42.start();
			}

			Matcher matcher31 = pattern31.matcher(line.substring(index));
			int count31 = 0;

			while (matcher31.find()) {
				count31++;
				index += matcher31.end() - matcher31.start();
			}

			if (index == line.length() && count42 > count31 && count31 > 0) {
				validMessages++;
			}
		}

		return validMessages;
	}

	private static int countValidMessages(List<String> input) {
		int validMessages = 0;

		Map<Integer, Rule> ruleMap = buildRuleMap(input);
		String regex = generateRegex(0, ruleMap);

		for (String line : input.subList(input.indexOf("") + 1, input.size()))
			if (line.matches(regex))
				validMessages++;

		return validMessages;
	}

	private static String generateRegex(int ruleNumber, Map<Integer, Rule> ruleMap) {
		Rule rule = ruleMap.get(ruleNumber);
		StringBuilder message = new StringBuilder();

		if (rule.subRules2 == null && rule.subRules1.size() == 1) {
			if (rule.subRules1.get(0) == -1) {
				return "a";

			} else if (rule.subRules1.get(0) == -2) {
				return "b";

			}
		}

		if (ruleNumber == 8) {
			message.append(generateRegex(42, ruleMap));
			message.append("+");

			return message.toString();

		} else if (ruleNumber == 11) {
			message.append("(");

			for (int i = 1; i <= 15; i++) {
				for (int j = 0; j < i; j++) {
					message.append(generateRegex(42, ruleMap));
				}

				for (int j = 0; j < i; j++) {
					message.append(generateRegex(31, ruleMap));
				}

				if (i != 15) {
					message.append("|");
				}
			}

			message.append(")");

			return message.toString();

		}

		if (rule.subRules2 != null) {
			message.append("(");

			for (int subRule : rule.subRules1) {
				message.append(generateRegex(subRule, ruleMap));
			}

			message.append("|");

			for (int subRule : rule.subRules2) {
				message.append(generateRegex(subRule, ruleMap));
			}

			message.append(")");

		} else {
			for (int subRule : rule.subRules1) {
				message.append(generateRegex(subRule, ruleMap));
			}
		}

		return message.toString();
	}

	private static Map<Integer, Rule> buildRuleMap(List<String> input) {
		Map<Integer, Rule> ruleMap = new HashMap<>();

		for (String line : input) {
			if (line.equals(""))
				break;

			Rule rule;

			if (line.contains("|")) {
				String substring1 = line.split(": ")[1].split("\\| ")[0];
				String substring2 = line.split(": ")[1].split("\\| ")[1];

				rule = new Rule(
						Arrays.stream(substring1.split(" ")).map(Integer::parseInt).collect(Collectors.toList()),
						Arrays.stream(substring2.split(" ")).map(Integer::parseInt).collect(Collectors.toList())
				);

			} else if (line.contains("\"")) {
				int subRule = line.contains("a") ? -1 : -2;

				rule = new Rule(Collections.singletonList(subRule));

			} else {
				rule = new Rule(Arrays.stream(line.split(": ")[1].split(" ")).map(Integer::parseInt).collect(Collectors.toList()));
			}

			ruleMap.put(Integer.parseInt(line.split(":")[0]), rule);
		}

		return ruleMap;
	}

	public static class Rule {

		public final List<Integer> subRules1;
		public final List<Integer> subRules2;

		public Rule(List<Integer> subRules1) {
			this.subRules1 = subRules1;
			this.subRules2 = null;
		}

		public Rule(List<Integer> subRules1, List<Integer> subRules2) {
			this.subRules1 = subRules1;
			this.subRules2 = subRules2;
		}
	}

}
