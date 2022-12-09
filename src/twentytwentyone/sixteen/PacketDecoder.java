package twentytwentyone.sixteen;

import util.java.Read;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

public class PacketDecoder {

	public static void main(String[] args) {
		String input = Read.asString("twentytwentyone/sixteen/input.txt");

		System.out.println(evaluateExpression(input));
	}

	private static int sumVersionNumbers(String input) {
		StringBuilder binary = new StringBuilder(new BigInteger(input, 16).toString(2));
		while (binary.length() % 4 != 0) {
			binary.insert(0, "0");
		}

		return parseVersionNumber(binary);
	}

	private static long evaluateExpression(String input) {
		StringBuilder binary = new StringBuilder(new BigInteger(input, 16).toString(2));
		while (binary.length() % 4 != 0) {
			binary.insert(0, "0");
		}

		return parsePackets(binary);
	}

	private static int parseVersionNumber(StringBuilder binary) {
		int version = munch(binary, 3);
		int typeID = munch(binary, 3);

		if (typeID == 4) {
			while (true) {
				int delimiter = munch(binary, 1);
				munch(binary, 4);

				if (delimiter == 0) break;
			}

		} else {
			if (munch(binary, 1) == 1) {
				int subPacketsCount = munch(binary, 11);

				for (int i = 0; i < subPacketsCount; i++) {
					version += parseVersionNumber(binary);
				}
			} else {
				int subPacketsBits = munch(binary, 15);
				int remainingBits = binary.length();

				while (remainingBits - subPacketsBits != binary.length()) {
					version += parseVersionNumber(binary);
				}
			}
		}

		return version;
	}

	private static long parsePackets(StringBuilder binary) {
		munch(binary, 3);
		int typeID = munch(binary, 3);

		if (typeID == 4) {
			StringBuilder literal = new StringBuilder();
			while (true) {
				int delimiter = munch(binary, 1);
				literal.append(munchString(binary, 4));

				if (delimiter == 0) break;
			}

			return Long.parseLong(literal.toString(), 2);

		} else {
			List<Long> subPackets = new ArrayList<>();

			if (munch(binary, 1) == 1) {
				int subPacketsCount = munch(binary, 11);
				for (int i = 0; i < subPacketsCount; i++) {
					subPackets.add(parsePackets(binary));
				}

			} else {
				int subPacketsBits = munch(binary, 15);
				int remainingBits = binary.length();
				while (remainingBits - subPacketsBits != binary.length()) {
					subPackets.add(parsePackets(binary));
				}
			}

			return Operator.fromTypeId(typeID).evaluate(subPackets);
		}
	}

	private static int munch(StringBuilder builder, int bits) {
		return Integer.parseInt(munchString(builder, bits), 2);
	}

	private static String munchString(StringBuilder builder, int bits) {
		String result = builder.substring(0, bits);
		builder.delete(0, bits);

		return result;
	}

	private enum Operator {
		SUM(0, subPackets -> subPackets.stream().mapToLong(l -> l).sum()),
		PRODUCT(1, subPackets -> subPackets.stream().reduce(1L, (a, b) -> a * b)),
		MIN(2, subPackets -> subPackets.stream().mapToLong(l -> l).min().orElseThrow()),
		MAX(3, subPackets -> subPackets.stream().mapToLong(l -> l).max().orElseThrow()),
		GREATER_THAN(5, subPackets -> subPackets.get(0) > subPackets.get(1) ? 1L : 0L),
		LESS_THAN(6, subPackets -> subPackets.get(0) < subPackets.get(1) ? 1L : 0L),
		EQUAL(7, subPackets -> subPackets.get(0).equals(subPackets.get(1)) ? 1L : 0L);

		private final int typeId;
		private final Function<List<Long>, Long> operation;

		Operator(int typeId, Function<List<Long>, Long> operation) {
			this.typeId = typeId;
			this.operation = operation;
		}

		public static Operator fromTypeId(int typeId) {
			return Arrays.stream(Operator.values())
					.filter(operator -> operator.typeId == typeId)
					.findFirst()
					.orElseThrow();
		}

		public long evaluate(List<Long> subPackets) {
			return operation.apply(subPackets);
		}
	}

}
