package com.caelwarner.adventofcode.twentytwenty.twentyfive;

import com.caelwarner.util.Read;

import java.util.List;

public class ComboBreaker {

	public static void main(String[] args) {
		List<Integer> input = Read.asIntList("adventofcode/twentytwenty/twentyfive/input.txt");

		System.out.println(crackEncryptionKey(input));
	}

	private static long crackEncryptionKey(List<Integer> input) {
		int loopSize = getLoopSize(input.get(0));
		long encryptionKey = 1;

		for (int i = 0; i < loopSize; i++) {
			encryptionKey *= input.get(1);
			encryptionKey %= 20201227;
		}

		return encryptionKey;
	}

	private static int getLoopSize(int publicKey) {
		int i = 0;
		long num = 1;

		while (num != publicKey) {
			num *= 7;
			num %= 20201227;

			i++;
		}

		return i;
	}

}
