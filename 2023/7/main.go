package main

import (
	"fmt"
	"utils"
	"strings"
	"sort"
	"strconv"
)

func getCardMap(hand string) map[byte]int {
	cardMap := make(map[byte]int)
	for i := 0; i < len(hand); i++ {
		_, ok := cardMap[hand[i]]
		if ok {
			cardMap[hand[i]]++
		} else {
			cardMap[hand[i]] = 1
		}
	}
	return cardMap
}

func getStrength(hand string) int {
	cardMap := getCardMap(hand)
	if len(cardMap) == 5 {
		return 6
	}
	if len(cardMap) == 4 {
		return 5
	}
	if len(cardMap) == 3 {
		for _, v := range cardMap {
			if v == 3 {
				return 3
			}
		}
		return 4
	}
	if len(cardMap) == 2 {
		for _, v := range cardMap {
			if v == 4 {
				return 1
			}
		}
		return 2
	}
	return 0
}

func compareHands(a string, b string, withJokers bool) bool {
	cardToIntMap := map[string]int{
		"A": 14,
		"K": 13,
		"Q": 12,
		"J": 11,
		"T": 10,
	}
	for i:= 1; i <= 9; i++ {
		cardToIntMap[fmt.Sprint(i)] = i
	}
	if withJokers {
		cardToIntMap["J"] = 0
	}
	for i := 0; i < 5; i++ {
		if a[i] != b[i] {
			return cardToIntMap[string(a[i])] < cardToIntMap[string(b[i])]
		}
	}
	return false
}

func optimizeHand(hand string) int {
	cardMap := getCardMap(hand)
	joker, ok := cardMap['J']
	if !ok || joker == 5 {
		return getStrength(hand)
	}
	mostFrequent := byte('J')
	maxFrequency := -1
	for k, v := range cardMap {
		if k == 'J' {
			continue
		}
		if v > maxFrequency {
			mostFrequent = k
			maxFrequency = v
		}
	}
	optimized := strings.Replace(hand, "J", string(mostFrequent), joker)
	return getStrength(optimized)
}

func sol2() int {
	lineChan := make(chan string)

	go utils.ReadInputLines("input.txt", lineChan)

	handsByStrength := make([][]string, 7)

	for line := range lineChan {
		if (len(line) == 0) {
			continue
		}
		hand := strings.Fields(line)[0]
		strength := optimizeHand(hand)
		handsByStrength[strength] = append(handsByStrength[strength], line)
	}

	rankedHands := make([]string, 0)

	for i := 6; i >= 0; i-- {
		sort.Slice(handsByStrength[i], func(j, k int) bool  {return compareHands(handsByStrength[i][j], handsByStrength[i][k], true) })
		rankedHands = append(rankedHands, handsByStrength[i]...)
	}

	sum := 0

	for i := 0; i < len(rankedHands); i++ {
		bet := strings.Fields(rankedHands[i])[1]
		betNum, _ := strconv.Atoi(bet)
		sum += betNum * (i+1)
	}

	return sum
}

func sol1() int {
	lineChan := make(chan string)

	go utils.ReadInputLines("input.txt", lineChan)

	handsByStrength := make([][]string, 7)

	for line := range lineChan {
		if (len(line) == 0) {
			continue
		}
		hand := strings.Fields(line)[0]
		strength := getStrength(hand)
		handsByStrength[strength] = append(handsByStrength[strength], line)
	}

	rankedHands := make([]string, 0)

	for i := 6; i >= 0; i-- {
		sort.Slice(handsByStrength[i], func(j, k int) bool  {return compareHands(handsByStrength[i][j], handsByStrength[i][k], false) })
		rankedHands = append(rankedHands, handsByStrength[i]...)
	}

	sum := 0

	for i := 0; i < len(rankedHands); i++ {
		bet := strings.Fields(rankedHands[i])[1]
		betNum, _ := strconv.Atoi(bet)
		sum += betNum * (i+1)
	}

	return sum
}

func main() {
	fmt.Println(sol2())
}