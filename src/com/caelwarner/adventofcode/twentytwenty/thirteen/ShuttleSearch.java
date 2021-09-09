package com.caelwarner.adventofcode.twentytwenty.thirteen;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ShuttleSearch {

	public static void main(String[] args) {
		List<String> input = Read.asStringArray("adventofcode/twentytwenty/thirteen/input.txt");

		System.out.println(findEarliestSequence(input));
	}

	private static long findEarliestSequence(List<String> input) {
		int[] busIDs = Arrays.stream(input.get(1).split(","))
				.map(id -> id.equals("x") ? "-1" : id)
				.mapToInt(Integer::parseInt)
				.toArray();

		List<Bus> buses = new ArrayList<>();

		for (int i = 0; i < busIDs.length; i++) {
			if (busIDs[i] != -1) {
				buses.add(new Bus(busIDs[i], i));
			}
		}

		long m = buses.stream()
				.mapToLong(bus -> bus.id)
				.reduce(1, (a, b) -> a * b);

		return buses.stream()
				.map(bus -> (bus.id - bus.offset) * (m / bus.id) * modInverse((m / bus.id), bus.id))
				.reduce(0L, Long::sum) % m;
	}

	private static int findEarliestBus(List<String> input) {
		int arrivalTime = Integer.parseInt(input.get(0));
		int[] busIDs = Arrays.stream(input.get(1).split(","))
				.filter(id -> !id.equals("x"))
				.mapToInt(Integer::parseInt).
				toArray();

		int earliestBus = 0;
		int shortestWait = Integer.MAX_VALUE;

		for (int id : busIDs) {
			int wait = id - (arrivalTime % id);

			if (wait < shortestWait) {
				shortestWait = wait;
				earliestBus = id;
			}
		}

		return earliestBus * shortestWait;
	}

	private static long modInverse(long a, long m) {
		long x;

		for (x = 0; x < m; x++) {
			if ((a * x) % m == 1) {
				break;
			}
		}

		return x;
	}

	private static class Bus {
		public final int id;
		public final int offset;

		public Bus(int id, int offset) {
			this.id = id;
			this.offset = offset;
		}
	}

}
