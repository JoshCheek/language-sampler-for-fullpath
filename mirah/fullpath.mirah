#!/usr/bin/env jruby -S mirah

import java.util.ArrayList

class Fullpath
  def initialize(dir:String, argv:ArrayList)
    @dir  = dir
    @argv = argv
  end

  def call
    if help?
      puts "usage: fullpath *[relative-paths] [-c]"
      puts
      puts "  Prints the fullpath of the paths"
      puts "  If no paths are given as args, it will read them from stdin"
      puts
      puts "  If there is only one path, the trailing newline is omitted"
      puts
      puts "  The -c flag will copy the results into your pasteboard"
    else
      if @argv.size == 1
        print "#{@dir}/#{argv[0]}"
      else
        @argv.each do |path|
          puts "#{@dir}/#{path}"
        end
      end
    end
  end

  def help?
    @argv.each { |arg| return true if arg == "-h" || arg == "--help" }
    false
  end
end

dir = System.getProperty("user.dir")

# The ARGV constant is only available when run through jruby
# but it takes way longer when I do it that way
# so just defining the constant for now
argv = ArrayList.new
argv.add("a")
argv.add("b")
argv.add("c")
Fullpath.new(dir, argv).call

argv = ArrayList.new
argv.add("-h")
Fullpath.new(dir, argv).call

argv = ArrayList.new
argv.add("a")
Fullpath.new(dir, argv).call
