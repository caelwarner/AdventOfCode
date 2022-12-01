package twentytwentyone.sixteen;

import util.java.Read;

import java.math.BigInteger;

public class PacketDecoder {

	public static void main(String[] args) {
		String input = Read.asString("adventofcode/twentytwentyone/sixteen/input.txt");

		System.out.println(sumVersionNumbers(input));
	}

	private static int sumVersionNumbers(String input) {
		int versions = 0;
		StringBuilder binary = new StringBuilder(new BigInteger(input, 16).toString(2));




		return 0;
	}

	private static int parsePacket(StringBuilder binary) {
//		versions += getAndRemove(binary, 3);
		int typeID = getAndRemove(binary, 3);

		if (typeID == 4) {

		} else {
			if (getAndRemove(binary, 1) == 1) {
				int subPacketsCount = getAndRemove(binary, 11);

				for (int i = 0; i < subPacketsCount; i++) {

				}
			}
		}

		return 0;
	}

	private static int getAndRemove(StringBuilder builder, int bits) {
		String result = builder.substring(0, bits);
		builder.delete(0, bits);

		return Integer.parseInt(result, 2);
	}

}
