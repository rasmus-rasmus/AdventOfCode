package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
	"regexp"
)

func sol(partTwo bool) int {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	sum := 0

	scanner := bufio.NewScanner(f)

	for scanner.Scan() {
		line := scanner.Text()
		intVal := getValueFromLine(line, partTwo)
		sum += intVal
	}
	return sum
}

func getValueFromLine(line string, partTwo bool) int {
	digits := []string{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"}
	if partTwo {
		digits = append(digits, []string{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}...)
	}
	digitMap := map[string]string{"one": "1",
							  "two": "2",
							  "three": "3",
							  "four": "4",
							  "five": "5",
							  "six": "6",
							  "seven": "7",
							  "eight": "8",
							  "nine": "9"}
	firstIdx, lastIdx := len(line), -1
	var firstDigit, lastDigit string
	for _, pattern := range digits {
		matchIndc := regexp.MustCompile(pattern).FindAllStringIndex(line, -1)
		if (len(matchIndc) == 0) {
			continue
		}
		firstMatch, lastMatch := matchIndc[0], matchIndc[len(matchIndc)-1]
		if firstMatch[0] < firstIdx {
			firstIdx = firstMatch[0]
			firstDigit = string(line[firstMatch[0]])
			if firstMatch[0] != firstMatch[1] - 1 {
				firstDigit = digitMap[line[firstMatch[0]: firstMatch[1]]]
			}
		}
		if lastMatch[0] > lastIdx {
			lastIdx = lastMatch[0]
			lastDigit = string(line[lastMatch[0]])
			if lastMatch[0] != lastMatch[1] - 1 {
				lastDigit = digitMap[line[lastMatch[0]: lastMatch[1]]]
			}
		}
	}
	val := firstDigit + lastDigit
	intVal, err := strconv.Atoi(val)
	if err != nil {
		fmt.Println(line)
		return 0
	}
	return intVal
}

func main() {
	fmt.Println(sol(true))
}