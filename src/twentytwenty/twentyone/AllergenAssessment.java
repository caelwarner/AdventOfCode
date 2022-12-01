package twentytwenty.twentyone;

import util.java.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

public class AllergenAssessment {

	private static final Random random = new Random();

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/twentyone/input.txt");

		System.out.println(getDangerousIngredientsList(input));
	}

	private static int findSafeIngredients(List<String> input) {
		int safeIngredients = 0;

		List<Food> foods = new ArrayList<>();
		Set<String> uniqueAllergens = new HashSet<>();
		Collection<String> knownAllergens = new ArrayList<>();

		parseFoods(input, foods, uniqueAllergens);

		while (knownAllergens.size() != uniqueAllergens.size())
			knownAllergens = findKnownAllergens(foods, uniqueAllergens).values();

		for (Food food : foods) {
			for (String ingredient : food.ingredients()) {
				if (!knownAllergens.contains(ingredient))
					safeIngredients++;
			}
		}

		return safeIngredients;
	}

	private static String getDangerousIngredientsList(List<String> input) {
		List<Food> foods = new ArrayList<>();
		Set<String> uniqueAllergens = new HashSet<>();
		Map<String, String> knownAllergens = new HashMap<>();

		parseFoods(input, foods, uniqueAllergens);

		while (knownAllergens.size() != uniqueAllergens.size())
			knownAllergens = findKnownAllergens(foods, uniqueAllergens);

		List<String> sortedAllergens = knownAllergens.entrySet().stream().sorted(Map.Entry.comparingByKey()).map(Map.Entry::getValue).toList();

		StringBuilder builder = new StringBuilder();

		sortedAllergens.forEach(allergen -> {
			builder.append(allergen);
			builder.append(",");
		});

		builder.deleteCharAt(builder.length() - 1);

		return builder.toString();
	}

	private static void parseFoods(List<String> input, List<Food> foods, Set<String> uniqueAllergens) {
		for (String line : input) {
			String[] split = line.replace(")", "").replace(",", "").split(" \\(contains ");

			Set<String> ingredients = Arrays.stream(split[0].split(" ")).collect(Collectors.toSet());
			Set<String> allergens = Arrays.stream(split[1].split(" ")).collect(Collectors.toSet());

			foods.add(new Food(ingredients, allergens));
			uniqueAllergens.addAll(allergens);
		}
	}

	private static Map<String, String> findKnownAllergens(List<Food> foods, Set<String> uniqueAllergens) {
		Map<String, String> knownAllergens = new HashMap<>();

		for (String allergen : uniqueAllergens) {
			Set<String> ingredients = new HashSet<>();

			foods.forEach(food -> {
				if (food.allergens().contains(allergen)) {
					if (ingredients.size() == 0)
						ingredients.addAll(food.ingredients());
					else
						ingredients.retainAll(food.ingredients());
				}
			});

			ingredients.removeAll(knownAllergens.values());

			if (ingredients.size() > 0)
				knownAllergens.put(allergen, ingredients.stream().toList().get(random.nextInt(ingredients.size())));
		}

		return knownAllergens;
	}

	private record Food(Set<String> ingredients, Set<String> allergens) {}

}
