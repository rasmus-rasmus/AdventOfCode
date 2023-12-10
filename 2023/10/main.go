package main

import (
	"fmt"
	"utils"
	"regexp"
)

const (
	North = 0
	East  = 1
	South = 2
	West  = 3
)

func moveInDirection(point [2]int, dir int) [2]int {
	switch dir {
	case North:
		return [2]int{point[0]-1, point[1]}
	case East:
		return [2]int{point[0], point[1]+1}
	case South:
		return [2]int{point[0]+1, point[1]}
	case West:
		return [2]int{point[0], point[1]-1}
	default:
		return point
	}
}

func getDirectionsFromCurrentPipe(pipe string) [2]int {
	switch pipe {
	case "F":
		return [2]int{East, South}
	case "|":
		return [2]int{North, South}
	case "L":
		return [2]int{North, East}
	case "-":
		return [2]int{East, West}
	case "J":
		return [2]int{North, West}
	case "7":
		return [2]int{South, West}
	default:
		// panic(fmt.Sprintf("Invalid pipe: %v", pipe))
		return [2]int{-1, -1}
	}
}

func findDirectionsOfStartingPos(startingPos [2]int, grid []string) [2]int {
	dirsFound := []int{}
	for dir:=0; dir < 4; dir++ {
		nbrPos := moveInDirection(startingPos, dir)
		if nbrPos[0] < 0 || nbrPos[0] >= len(grid) {
			continue
		}
		if nbrPos[1] < 0 || nbrPos[1] >= len(grid[nbrPos[0]]) {
			continue
		}
		nbr := string(grid[nbrPos[0]][nbrPos[1]])
		if nbr == "." {
			continue
		}
		nbrDirs := getDirectionsFromCurrentPipe(nbr)
		if nbrDirs[0] == (dir + 2) % 4 || nbrDirs[1] == (dir + 2) % 4 {
			dirsFound = append(dirsFound, dir)
		}
	}
	return [2]int{dirsFound[0], dirsFound[1]}
}

func getNextPosAndMove(currPos [2]int, currMove int, grid []string) (nextPos [2]int, nextMove int) {
	nextPos = moveInDirection(currPos, currMove)
	nextPipe := string(grid[nextPos[0]][nextPos[1]])
	nextDirs := getDirectionsFromCurrentPipe(nextPipe)
	if nextDirs[0] == (currMove + 2) % 4 {
		return nextPos, nextDirs[1]
	} 
	return nextPos, nextDirs[0]
}

// Uses the (signed) intersections of the East-bound ray emanating from `pos` with the trace of 
// of the loop starting from `startingPos` to compute the winding number of said loop around `pos`.
func computeWindingNumber(pos [2]int, startingPos [2]int, grid []string) int {
	windingNumber := 0
	startingDirs := findDirectionsOfStartingPos(startingPos, grid)
	if pos[0] == startingPos[0] && pos[1] < startingPos[1] {
		// If the starting position it self is a vertical intersection of the ray this
		// isn't detected by the below traversal of the loop. There is surely a prettier way
		// to do this, but it took me so long to make this code produce the correct solution,
		// that I simply couldn't find the energy for a refactor :'(
		if (startingDirs[0] == North && startingDirs[1] == South) {
			windingNumber += 1
		} else if (startingDirs[0] == South && startingDirs[1] == North) {
			windingNumber -= 1
		}
	}
	currPos := startingPos
	currMove := startingDirs[0]
	nextPos, nextMove := getNextPosAndMove(startingPos, startingDirs[0], grid)

	// Traverse the loop recording the sign (1 for South-to-North, -1 for North-to-South) of each intersection
	// an intersection may span several grid positions if the line is "tangent" to part of the loop. In this
	// case it must be recorded whether the traversal of the loop "leaves" the ray to the same side as it "entered" it,
	// in which case we record no intersection, or to the opposite side, in which case we do record an intersection.
	for nextPos != startingPos {
		if nextPos == pos || currPos == pos {
			// If `pos` is on the loop, we assign it a winding number of 0.
			return 0
		}
		if nextPos[0] == pos[0] && nextPos[1] > pos[1] {
			// Traversal has "entered" the ray.
			if currMove == South {
				for nextPos != startingPos && nextPos[0] == pos[0] {
					// Continue traversal until it "leaves" the ray again
					currPos, currMove = nextPos, nextMove
					nextPos, nextMove = getNextPosAndMove(currPos, currMove, grid)
					if currPos == pos {
						return 0
					}
				}
				if currMove == South {
					// Trajectory "left" the ray to the opposite side; a proper intersection!!
					windingNumber -= 1
				}
				continue
			} else if currMove == North {
				// Continue traversal until it "leaves" the ray again
				for nextPos != startingPos && nextPos[0] == pos[0] {
					currPos, currMove = nextPos, nextMove
					nextPos, nextMove = getNextPosAndMove(currPos, currMove, grid)
					if currPos == pos {
						return 0
					}
				}
				if currMove == North {
					// Trajectory "left" the ray to the opposite side; a proper intersection!!
					windingNumber += 1
				}
				continue
			}
		}

		// No intersection - we move on
		currPos, currMove = nextPos, nextMove
		nextPos, nextMove = getNextPosAndMove(currPos, currMove, grid)
	}

	return windingNumber
}

func sol1(inputFile string) int {
	lineChan := make(chan string)

	go utils.ReadInputLines(inputFile, lineChan)
	grid := []string{}
	i := 0
	var startingPos [2]int
	for line := range lineChan {
		grid = append(grid, line)
		sIdx := regexp.MustCompile(`S`).FindStringIndex(line)
		if len(sIdx) != 0 {
			startingPos[0] = i
			startingPos[1] = sIdx[0]
		}

		i++
	}
	startingDirs := findDirectionsOfStartingPos(startingPos, grid)
	nextPos, nextMove := getNextPosAndMove(startingPos, startingDirs[0], grid)
	loopCounter := 1
	for nextPos != startingPos {
		nextPos, nextMove = getNextPosAndMove(nextPos, nextMove, grid)
		loopCounter++
	}

	return loopCounter / 2
}

func sol2(inputFile string) int {
	lineChan := make(chan string)

	go utils.ReadInputLines(inputFile, lineChan)
	grid := []string{}
	i := 0
	var startingPos [2]int
	for line := range lineChan {
		grid = append(grid, line)
		sIdx := regexp.MustCompile(`S`).FindStringIndex(line)
		if len(sIdx) != 0 {
			startingPos[0] = i
			startingPos[1] = sIdx[0]
		}

		i++
	}
	
	sum := 0

	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[i]); j++ {
			pos := [2]int{i, j}
			windingNumber := computeWindingNumber(pos, startingPos, grid)
			if windingNumber != 0 {
				sum += 1
			}
		}
	}
	// Ideas for optimisation: For any positions falling outside the "extents" of the loop,
	// There is no reason to compute winding numbers, since the are certain to not fall in
	// the interior of the loop. Computing extents would require one inital traversal of the loop.

	return sum
}

func main() {
	fmt.Println(sol2("input.txt"))
}