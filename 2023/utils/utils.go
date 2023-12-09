package utils

import (
	"os"
	"bufio"
	"errors"
	"strings"
	"strconv"
)

func ReadInputLines(fileName string, ch chan string) error {
	f, err := os.Open(fileName)
	if err != nil {
		return errors.New("Could not open file")
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	for scanner.Scan() {
		line := scanner.Text()
		ch <- line
	}
	close(ch)
	return nil
}


func GetNumeralsFromString(s string) []int {
	numeralsOut := []int{}
	numberStrings := strings.Fields(s)
	for _, strNum := range numberStrings {
		numeral, _ := strconv.Atoi(strNum)
		numeralsOut = append(numeralsOut, numeral)
	}
	return numeralsOut
}

func GetIndexOfValue[T comparable](anySlice []T, val T) (bool, int) {
	for i, v := range anySlice {
		if v == val {
			return true, i
		}
	}
	return false, -1
}

type Summable interface {
	int | float64
}

func Sum[T Summable](seq []T) T {
	var sum T
	for _, v := range seq {
		sum += v
	}
	return sum
}