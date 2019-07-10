package yacht

import (
	"fmt"
)

// Score a dice.
func Score(dice []int, category string) int {
	a, b := rules[category]
	if !b {
		panic(fmt.Sprintf("Could not find category %s", category))
	}

	m, err := getMap(dice)
	if err != nil {
		return 0
	}
	return a(m)
}

const (
	choice         = "choice"
	bigStraight    = "big straight"
	littleStraight = "little straight"
	fourOfAKind    = "four of a kind"
	fullHouse      = "full house"
	yacht          = "yacht"
	count1         = "ones"
	count2         = "twos"
	count3         = "threes"
	count4         = "fours"
	count5         = "fives"
	count6         = "sixes"
)

type ruleEvaluator func(map[int]int) int

var rules = (func() map[string]ruleEvaluator {
	r := make(map[string]ruleEvaluator)

	singleIndexOfCount := func(dice map[int]int, count int) (int, error) {
		result := 0
		for i := 1; i <= 6; i++ {
			if dice[i] == count {
				if result != 0 {
					return 0, fmt.Errorf("Found more than one digit with %d occurences", count)
				}
				result = i
			}
		}
		if result == 0 {
			return 0, fmt.Errorf("Could not find digit with %d occurences", count)
		}
		return result, nil
	}

	r[yacht] = func(dice map[int]int) int {
		if _, err := singleIndexOfCount(dice, 5); err != nil {
			return 0
		}
		return 50
	}

	countOnly := func(only int) func(dice map[int]int) int {
		return func(dice map[int]int) int {
			return only * dice[only]
		}
	}
	r[count1] = countOnly(1)
	r[count2] = countOnly(2)
	r[count3] = countOnly(3)
	r[count4] = countOnly(4)
	r[count5] = countOnly(5)
	r[count6] = countOnly(6)

	r[fullHouse] = func(dice map[int]int) int {
		three, err := singleIndexOfCount(dice, 3)
		if err != nil {
			return 0
		}
		two, err := singleIndexOfCount(dice, 2)
		if err != nil {
			return 0
		}
		return 3*three + 2*two
	}

	r[fourOfAKind] = func(dice map[int]int) int {
		five, err := singleIndexOfCount(dice, 5)
		if err == nil {
			return 4 * five
		}

		four, err := singleIndexOfCount(dice, 4)
		if err != nil {
			return 0
		}
		return 4 * four
	}

	r[littleStraight] = func(dice map[int]int) int {
		zero, err := singleIndexOfCount(dice, 0)
		if err != nil || zero != 6 {
			return 0
		}
		return 30
	}

	r[bigStraight] = func(dice map[int]int) int {
		zero, err := singleIndexOfCount(dice, 0)
		if err != nil || zero != 1 {
			return 0
		}
		return 30
	}

	r[choice] = func(dice map[int]int) int {
		result := 0
		for val, count := range dice {
			result += val * count
		}
		return result
	}

	return r
})()

func getMap(dice []int) (map[int]int, error) {
	result := map[int]int{
		1: 0,
		2: 0,
		3: 0,
		4: 0,
		5: 0,
		6: 0,
	}

	if len(dice) != 5 {
		return result, fmt.Errorf("Illegal dice with %d numbers", len(dice))
	}

	for _, v := range dice {
		result[v]++
	}

	return result, nil
}
