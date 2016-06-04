package main

import "os"
import "fmt"

func includes(haystack []string, needle string) bool {
	for _, hay := range haystack {
		if hay == needle {
			return true
		}
	}
	return false
}

func doHelp(args []string) bool {
	return includes(args, "-h") || includes(args, "--help")
}

func doCopy(args []string) bool {
	return false // FIXME
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

func main() {
	args := os.Args[1:]
	if doHelp(args) {
		fmt.Println("this is the help screen")
	} else {
		fullpaths := mapToFullPaths(selectPaths(args))
		pathsString := join(fullpaths, "\n")
		fmt.Println(pathsString)
		if doCopy(args) {
			copyToClipboard(pathsString)
		}
	}
}
