package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
)

type Invocation struct {
	fullpaths []string
	doHelp    bool
	doCopy    bool
}

func main() {
	args := os.Args[1:]
	invocation := parseArgs(args)
	if invocation.doHelp {
		printHelp(os.Stdin)
	} else {
		pathsString := join(invocation.fullpaths, "\n")
		fmt.Println(pathsString)
		if invocation.doCopy {
			copyToClipboard(pathsString)
		}
	}
}

func parseArgs(args []string) Invocation {
	data := Invocation{
		fullpaths: mapToFullPaths(selectPaths(args)),
		doHelp:    doHelp(args),
		doCopy:    doCopy(args),
	}
	return data
}

func printHelp(outstream io.Writer) {
	fmt.Fprintln(outstream, "usage: fullpath *[relative-paths] [-c]")
	fmt.Fprintln(outstream, "")
	fmt.Fprintln(outstream, "  Prints the fullpath of the paths")
	fmt.Fprintln(outstream, "  If no paths are given as args, it will read them from stdin")
	fmt.Fprintln(outstream, "")
	fmt.Fprintln(outstream, "  If there is only one path, the trailing newline is omitted")
	fmt.Fprintln(outstream, "")
	fmt.Fprintln(outstream, "  The -c flag will copy the results into your pasteboard")
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
	paths := []string{}
	for _, relPath := range relativePaths {
		absPath, _ := filepath.Abs(relPath)
		paths = append(paths, absPath)
	}
	return paths
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
	cmd := exec.Command("pbcopy")
	stdin, _ := cmd.StdinPipe()
	cmd.Start()
	fmt.Fprint(stdin, str)
	stdin.Close()
	cmd.Wait()
}
