package com.caelwarner.adventofcode.twentytwenty.twentytwo;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class CrabCombat {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/twentytwo/input.txt");

		System.out.println(getWinningScoreRecursive(input));
	}

	private static int getWinningScore(List<String> input) {
		Queue<Integer>[] players = parseDecks(input);
		Queue<Integer> player1 = players[0];
		Queue<Integer> player2 = players[1];

		while (!player1.isEmpty() && !player2.isEmpty()) {
			int card1 = player1.remove();
			int card2 = player2.remove();

			if (card1 > card2) {
				player1.add(card1);
				player1.add(card2);
			} else {
				player2.add(card2);
				player2.add(card1);
			}
		}

		return calculateScore(player1, player2);
	}

	private static int getWinningScoreRecursive(List<String> input) {
		Queue<Integer>[] players = parseDecks(input);
		Queue<Integer> player1 = players[0];
		Queue<Integer> player2 = players[1];

		playGameRecursive(player1, player2);

		return calculateScore(player1, player2);
	}

	@SuppressWarnings("unchecked")
	private static Queue<Integer>[] parseDecks(List<String> input) {
		Queue<Integer>[] players = new Queue[2];
		players[0] = new LinkedList<>();
		players[1] = new LinkedList<>();

		boolean first = true;
		for (int i = 1; i < input.size(); i++) {
			if (input.get(i).equals("")) {
				first = false;
				i++;
				continue;
			}

			if (first)
				players[0].add(Integer.parseInt(input.get(i)));
			else
				players[1].add(Integer.parseInt(input.get(i)));
		}

		return players;
	}

	private static boolean playGameRecursive(Queue<Integer> player1, Queue<Integer> player2) {
		List<String> previousRounds = new ArrayList<>();

		while (!player1.isEmpty() && !player2.isEmpty()) {
			String deck1 = player1.toString();
			String deck2 = player2.toString();

			if (previousRounds.contains(deck1) || previousRounds.contains(deck2)) {
				player2.clear();
				break;
			}

			previousRounds.add(deck1);
			previousRounds.add(deck2);

			int card1 = player1.remove();
			int card2 = player2.remove();

			if (card1 <= player1.size() && card2 <= player2.size()) {
				if (playGameRecursive(new LinkedList<>(player1.stream().limit(card1).toList()), new LinkedList<>(player2.stream().limit(card2).toList()))) {
					player1.add(card1);
					player1.add(card2);
				} else {
					player2.add(card2);
					player2.add(card1);
				}

			} else if (card1 > card2) {
				player1.add(card1);
				player1.add(card2);
			} else {
				player2.add(card2);
				player2.add(card1);
			}
		}

		return player2.isEmpty();
	}

	private static int calculateScore(Queue<Integer> player1, Queue<Integer> player2) {
		int score = 0;
		Queue<Integer> winner = player2.isEmpty() ? player1 : player2;

		for (int i = winner.size(); i > 0; i--)
			score += winner.remove() * i;

		return score;
	}

}
