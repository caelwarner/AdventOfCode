package com.caelwarner.adventofcode.twentytwentyone.ten;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class SyntaxScoring {

	private static final Map<Character, Character> OPENING_TO_CLOSING = Map.of(
			'(', ')',
			'[', ']',
			'{', '}',
			'<', '>'
	);

	private static final Map<Character, Character> CLOSING_TO_OPENING = Map.of(
			')', '(',
			']', '[',
			'}', '{',
			'>', '<'
	);

	private static final Map<Character, Integer> ILLEGAL_VALUES = Map.of(
			')', 3,
			']', 57,
			'}', 1197,
			'>', 25137
	);

	private static final Map<Character, Integer> AUTOCOMPLETE_VALUES = Map.of(
			')', 1,
			']', 2,
			'}', 3,
			'>', 4
	);

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwentyone/ten/input.txt");

		System.out.println(getAutocompleteScore(input));
	}

	private static long getErrorScore(List<String> input) {
		List<Character> illegalChars = new ArrayList<>();

		for (String line : input) {
			findIllegalCharacter(line).ifPresent(illegalChars::add);
		}

		return illegalChars.stream().mapToLong(ILLEGAL_VALUES::get).sum();
	}
	private static long getAutocompleteScore(List<String> input) {
		List<Long> scores = new ArrayList<>();

		// Remove corrupt lines
		input.removeIf(line -> findIllegalCharacter(line).isPresent());

		for (String line : input) {
			List<Character> chars = new ArrayList<>();
			long score = 0;
			int i = 0;

			do {
				if (i < line.length()) {
					char c = line.charAt(i);

					// Opening character
					if (OPENING_TO_CLOSING.containsKey(c))
						chars.add(c);

					// Check if closing character is legal
					else if (CLOSING_TO_OPENING.get(c) == chars.get(chars.size() - 1))
						chars.remove(chars.size() - 1);

				} else {
					char closing_char = OPENING_TO_CLOSING.get(chars.get(chars.size() - 1));

					score *= 5;
					score += AUTOCOMPLETE_VALUES.get(closing_char);

					chars.remove(chars.size() - 1);
				}

				i++;
			} while (chars.size() > 0 || i < line.length());

			scores.add(score);
		}

		Collections.sort(scores);

		return scores.get((scores.size() - 1) / 2);
	}

	private static Optional<Character> findIllegalCharacter(String line) {
		List<Character> chars = new ArrayList<>();

		for (int i = 0; i < line.length(); i++) {
			char c = line.charAt(i);

			// Opening character
			if (OPENING_TO_CLOSING.containsKey(c))
				chars.add(c);

				// Check if closing character is legal
			else if (CLOSING_TO_OPENING.get(c) == chars.get(chars.size() - 1))
				chars.remove(chars.size() - 1);

			else {
				return Optional.of(c);
			}
		}

		return Optional.empty();
	}

}
