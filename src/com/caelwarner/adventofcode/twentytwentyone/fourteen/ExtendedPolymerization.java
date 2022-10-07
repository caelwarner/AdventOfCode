package com.caelwarner.adventofcode.twentytwentyone.fourteen;

import com.caelwarner.util.Read;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

public class ExtendedPolymerization {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwentyone/fourteen/input.txt");

		System.out.println(getElementsResultFast(input));
	}

	private static long getElementsResult(List<String> input) {
		String polymerTemplate = input.get(0);
		Map<String, String> pairInsertions = getPairInsertionsString(input);

		for (int step = 0; step < 10; step++) {
			StringBuilder builder = new StringBuilder(polymerTemplate);

			for (int i = polymerTemplate.length() - 2; i >= 0; i--) {
				String element = pairInsertions.get(polymerTemplate.substring(i, i + 2));
				builder.insert(i + 1, element);
			}

			polymerTemplate = builder.toString();
		}

		Map<Integer, Long> elements = polymerTemplate.chars().boxed().collect(Collectors.groupingBy(i -> i, Collectors.counting()));

		return Collections.max(elements.values()) - Collections.min(elements.values());
	}

	private static long getElementsResultFast(List<String> input) {
		/*
		 * 1. Build a tree of pairs for each pair insertion out to 5 steps
		 * 2. To get to higher steps take the leaf nodes of the tree and total all of their elements together and add it to the root
		 * 3. Effectively extend the tree by finding the leaf nodes of each leaf node
		 */


		String polymerTemplate = input.get(0);
		Map<String, String> pairInsertionsString = getPairInsertionsString(input);
		Map<String, PairInsertionData> pairInsertions = new HashMap<>();

		for (Map.Entry<String, String> pairInsertion : pairInsertionsString.entrySet()) {
			pairInsertions.put(pairInsertion.getKey(), new PairInsertionData(pairInsertion.getValue(), createSubPairsMap(pairInsertionsString), createElementsMap(pairInsertionsString)));
		}

		// Walk tree up to 5 steps of pair insertions
		for (Map.Entry<String, PairInsertionData> pairInsertion : pairInsertions.entrySet()) {
			walkTree(0, pairInsertion.getKey().substring(0, 1), pairInsertion.getKey().substring(1, 2), pairInsertion.getValue().subPairs(), pairInsertionsString, pairInsertion.getValue().elements());
		}

		for (int i = 5; i < 40; i *= 2) {
			Map<String, PairInsertionData> newPairInsertions = new HashMap<>();

			for (Map.Entry<String, PairInsertionData> pairInsertion : pairInsertions.entrySet()) {
				Map<String, AtomicLong> subPairs = createSubPairsMap(pairInsertionsString);
				Map<String, AtomicLong> elements = createElementsMap(pairInsertionsString);

				addMaps(elements, pairInsertion.getValue().elements(), 1);

				for (Map.Entry<String, AtomicLong> pair : pairInsertion.getValue().subPairs().entrySet()) {
					addMaps(elements, pairInsertions.get(pair.getKey()).elements(), pair.getValue().get());
					addMaps(subPairs, pairInsertions.get(pair.getKey()).subPairs(), pair.getValue().get());
				}

				PairInsertionData data = new PairInsertionData(pairInsertion.getValue().element(), subPairs, elements);
				newPairInsertions.put(pairInsertion.getKey(), data);
			}

			pairInsertions = newPairInsertions;
		}

		Map<String, AtomicLong> totalElements = createElementsMap(pairInsertionsString);

		// Add occurrences from base polymer template
		for (int i = 0; i < polymerTemplate.length(); i++) {
			totalElements.get(polymerTemplate.substring(i, i + 1)).incrementAndGet();
		}

		// Add occurrences from tree of pair insertions
		for (int i = 0; i < polymerTemplate.length() - 1; i++) {
			PairInsertionData data = pairInsertions.get(polymerTemplate.substring(i, i + 2));
			addMaps(totalElements, data.elements(), 1);
		}

		return totalElements.values().stream().mapToLong(AtomicLong::get).max().orElse(0) - totalElements.values().stream().mapToLong(AtomicLong::get).min().orElse(0);
	}

	private static Map<String, String> getPairInsertionsString(List<String> input) {
		Map<String, String> pairInsertions = new HashMap<>();

		for (int i = 2; i < input.size(); i++) {
			String[] pairInsertion = input.get(i).split(" -> ");
			pairInsertions.put(pairInsertion[0], pairInsertion[1]);
		}

		return pairInsertions;
	}

	private static Map<String, AtomicLong> createElementsMap(Map<String, String> pairInsertionsString) {
		Map<String, AtomicLong> elements = new HashMap<>();

		for (String result : pairInsertionsString.values()) {
			if (!elements.containsKey(result))
				elements.put(result, new AtomicLong());
		}

		return elements;
	}

	private static Map<String, AtomicLong> createSubPairsMap(Map<String, String> pairInsertionsString) {
		Map<String, AtomicLong> subPairs = new HashMap<>();

		pairInsertionsString.keySet().forEach(pair -> subPairs.put(pair, new AtomicLong()));

		return subPairs;
	}

	private static void addMaps(Map<String, AtomicLong> a, Map<String, AtomicLong> b, long multiplier) {
		for (String element : a.keySet()) {
			a.get(element).getAndAdd(b.get(element).get() * multiplier);
		}
	}

	private static void walkTree(int steps, String left, String right, Map<String, AtomicLong> subPairs, Map<String, String> pairInsertionsString, Map<String, AtomicLong> elements) {
		steps++;

		if (steps > 5) {
			subPairs.get(left + right).incrementAndGet();
			return;
		}

		String element = pairInsertionsString.get(left + right);
		elements.get(element).incrementAndGet();

		walkTree(steps, left, element, subPairs, pairInsertionsString, elements);
		walkTree(steps, element, right, subPairs, pairInsertionsString, elements);
	}

	private record PairInsertionData(String element, Map<String, AtomicLong> subPairs, Map<String, AtomicLong> elements) {}

}
