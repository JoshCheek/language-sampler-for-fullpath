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

func selectPaths(paths []string) []string {
	return paths // FIXME
}

func join(fullpaths []string, delimiter string) string {
	return delimiter // FIXME
}

func copyToClipboard(str string) {
	// FIXME
}
