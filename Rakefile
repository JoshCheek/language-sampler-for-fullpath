def self.cucumber(lang)
  original_path = ENV['PATH']
  puts "\e[35m=====  #{lang}  =====\e[39m"
  ENV['FULLPATH_DIR'] = File.expand_path(lang, __dir__)
  sh 'bundle exec cucumber'
end

desc 'Build / test all languages'
task :default


# =====  Go  =====
desc 'Build / test fullpath in Go'
task golang: 'golang/fullpath' do
  cucumber 'golang'
end
file 'golang/fullpath' => 'golang/fullpath.go' do
  sh 'go', 'build', '-o', 'golang/fullpath', 'golang/fullpath.go'
end
task go: :golang # shorthand
task default: :golang


# =====  Ruby  =====
desc 'Test fullpath in Ruby'
task :ruby do
  cucumber 'ruby'
end
task default: :ruby


# =====  Elixir  =====
desc 'Build / test fullpath in Elixir'
task elixir: 'elixir/fullpath' do
  cucumber 'elixir'
end
file 'elixir/fullpath' do # TODO: if we declare deps, can avoid rebuilding every time we run tests
  cd('elixir') { sh 'mix', 'escript.build' }
end
task default: :elixir


# =====  Common Lisp  =====
desc 'Build / test fullpath in Common Lisp'
task cl: 'common_lisp/fullpath' do
  cucumber 'common_lisp'
end
file 'common_lisp/fullpath' => 'common_lisp/fullpath.lisp' do
  sh 'sbcl',
       '--load', 'common_lisp/fullpath.lisp',
       '--eval', %[(save-lisp-and-die
                     "common_lisp/fullpath"
                     :executable           t
                     :toplevel             'main
                     :save-runtime-options t)],
       '--end-toplevel-options'
end
task default: :cl


# =====  Kotlin  =====
desc 'Build / test fullpath in Kotlin'
task kotlin: 'kotlin/fullpath' do
  cucumber 'kotlin'
end
task default: :kotlin

file 'kotlin/fullpath' => 'kotlin/fullpath.jar' do
  sh 'echo "#!/usr/bin/env java -jar" >  kotlin/fullpath'
  sh 'cat kotlin/fullpath.jar         >> kotlin/fullpath'
  chmod '+x', 'kotlin/fullpath'
end

file 'kotlin/fullpath.jar' => 'kotlin/fullpath.kt' do
  sh 'kotlinc',
     '-include-runtime',
     '-d', 'kotlin/fullpath.jar',
     'kotlin/fullpath.kt'
end
