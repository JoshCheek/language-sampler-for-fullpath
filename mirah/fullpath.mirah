#!/usr/bin/env jruby -S mirah

import java.util.ArrayList

class Fullpath
  def initialize(argv:ArrayList)
    @argv = argv
  end

  def call
    dir = System.getProperty("user.dir")
    if @argv.size == 1
      print "#{dir}/#{argv[0]}"
    else
      @argv.each do |path|
        puts "#{dir}/#{path}"
      end
    end
  end
end

# The ARGV constant is only available when run through jruby
# but it takes way longer when I do it that way
# so just defining the constant for now
argv = ArrayList.new
argv.add("a")
argv.add("b")
argv.add("c")
Fullpath.new(argv).call

argv = ArrayList.new
argv.add("a")
Fullpath.new(argv).call
