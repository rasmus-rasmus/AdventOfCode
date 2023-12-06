package main

import (
	"fmt"
	"math"
)


func findFirstWinningStrategy(time int, distance int) int {
	for t := 1; t < time; t++ {
		d := t * (time - t)
		if d > distance {
			return t
		}
	}
	return -1
}

func sol1() int {
	times := []int{40, 70, 98, 79}
	distances := []int{215, 1051, 2147, 1005}
	winningStrategies := 1
	for i := 0; i < len(times); i++ {
		time := times[i]
		distance := distances[i]
		firstWinning := findFirstWinningStrategy(time, distance)
		localWinningStrategies := time - firstWinning - (firstWinning - 1)
		winningStrategies *= localWinningStrategies
	}
	return winningStrategies
}

func sol2() int {
	time := 40709879
	distance := 215105121471005
	a := -1
	b := time
	c := -distance
	disc := float64(b*b - 4*a*c)
	sol1 := (float64(-b) - math.Sqrt(disc)) / float64(2 * a)
	sol2 := (float64(-b) + math.Sqrt(disc)) / float64(2 * a)
	firstWinning := int(math.Ceil(min(sol1, sol2)))
	return time - firstWinning - (firstWinning - 1)
}

func main() {
	fmt.Println(sol2())
}