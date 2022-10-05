package com.caelwarner.adventofcode.twentytwenty.sixteen;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.List;

public class TicketTranslation {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/sixteen/input.txt");

		System.out.println(findDepartureProduct(input));
	}

	private static long findDepartureProduct(List<String> input) {
		List<String> inputFields = input.subList(0, input.indexOf(""));
		List<String> inputTickets = input.subList(input.lastIndexOf("") + 2, input.size());
		String inputMyTicket = input.get(input.indexOf("") + 2);

		List<Field> fields = new ArrayList<>();
		List<List<Integer>> tickets = new ArrayList<>();
		List<Integer> myTicket = new ArrayList<>();

		for (String line : inputFields) {
			String ranges = line.split(": ")[1];

			fields.add(new Field(
					line.split(":")[0],
					new Range(Integer.parseInt(ranges.split(" or ")[0].split("-")[0]), Integer.parseInt(ranges.split(" or ")[0].split("-")[1])),
					new Range(Integer.parseInt(ranges.split(" or ")[1].split("-")[0]), Integer.parseInt(ranges.split(" or ")[1].split("-")[1]))
			));
		}

		for (String number : inputMyTicket.split(",")) {
			myTicket.add(Integer.parseInt(number));
		}

		for (String line : inputTickets) {
			List<Integer> ticket = new ArrayList<>();

			numbers:
			for (String number : line.split(",")) {
				for (Field field : fields) {
					if (field.inRange(Integer.parseInt(number))) {
						ticket.add(Integer.parseInt(number));

						continue numbers;
					}
				}
			}

			if (ticket.size() == fields.size())
				tickets.add(ticket);
		}

		while (fields.stream().anyMatch(field -> field.index == -1)) {
			for (int i = 0; i < fields.size(); i++) {
				int solutions = 0;
				Field correctField = null;

				fieldLoop:
				for (Field field : fields) {
					if (field.index != -1) {
						continue;
					}

					for (List<Integer> ticket : tickets) {
						if (!field.inRange(ticket.get(i))) {
							continue fieldLoop;
						}
					}

					solutions++;
					correctField = field;
				}

				if (solutions == 1) {
					correctField.index = i;
				}
			}
		}

		return fields.stream()
				.filter(field -> field.name.startsWith("departure"))
				.mapToLong(field -> myTicket.get(field.index))
				.reduce(1, (a, b) -> a * b);
	}

	private static long findErrorRate(List<String> input) {
		List<Range> ranges = new ArrayList<>();
		long errorRate = 0;

		boolean createRanges = true;
		boolean checkNearbyTickets = false;

		for (String line : input) {
			if (createRanges) {
				if (line.equals("")) {
					createRanges = false;

					continue;
				}

				for (String range : line.split(": ")[1].split(" or ")) {
					ranges.add(new Range(Integer.parseInt(range.split("-")[0]), Integer.parseInt(range.split("-")[1])));
				}

				continue;
			}

			if (line.startsWith("nearby tickets")) {
				checkNearbyTickets = true;

				continue;
			}

			if (checkNearbyTickets) {
				numbers:
				for (String number : line.split(",")) {
					int value = Integer.parseInt(number);

					for (Range range : ranges) {
						if (range.inRange(value)) {
							continue numbers;
						}
					}

					errorRate += value;
				}
			}
		}

		return errorRate;
	}

	public static class Field {

		public final String name;
		public final Range range1;
		public final Range range2;
		public int index = -1;

		public Field(String name, Range range1, Range range2) {
			this.name = name;
			this.range1 = range1;
			this.range2 = range2;
		}

		public boolean inRange(int value) {
			return range1.inRange(value) || range2.inRange(value);
		}
	}

	public static class Range {

		public final int min;
		public final int max;

		public Range(int min, int max) {
			this.min = min;
			this.max = max;
		}

		public boolean inRange(int value) {
			return value >= min && value <= max;
		}
	}

}
