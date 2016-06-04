package main

import "os"
import "fmt"

func main() {
	args := os.Args[1:]
	if doHelp(args) {
		fmt.Println("this is the help screen")
	} else {
		fullpaths := mapToFullPaths(selectPaths(args))
		pathsString := join(fullpaths, "\n")
		fmt.Println(pathsString)
		if doCopy(args) {
			fmt.Println("copy")
			copyToClipboard(pathsString)
		}
	}
}

func includes(haystack []string, needles ...string) bool {
	for _, hay := range haystack {
		for _, needle := range needles {
			if hay == needle {
				return true
			}
		}
	}
	return false
}

func doHelp(args []string) bool {
	return includes(args, "-h", "--help")
}

func doCopy(args []string) bool {
	return includes(args, "-c", "--copy")
}

func mapToFullPaths(relativePaths []string) []string {
	return relativePaths // FIXME
}

func selectPaths(args []string) []string {
	paths := []string{}
	for _, maybePath := range args {
		if !isFlag(maybePath) {
			paths = append(paths, maybePath)
		}
	}
	return paths
}

func isFlag(maybeFlag string) bool {
	return maybeFlag == "-c" || maybeFlag == "--copy" ||
		maybeFlag == "-h" || maybeFlag == "--help"
}

func join(strs []string, delimiter string) string {
	joined := ""
	for index, str := range strs {
		if index != 0 {
			joined += delimiter
		}
		joined += str
	}
	return joined
}

func copyToClipboard(str string) {
	// FIXME
}
