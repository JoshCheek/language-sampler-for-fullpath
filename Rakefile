def self.cucumber(lang)
  original_path = ENV['PATH']
  puts "\e[35m=====  #{lang}  =====\e[39m"
  ENV['FULLPATH_DIR'] = File.expand_path(lang, __dir__)
  sh 'bundle exec cucumber'
end

desc 'Build / test all languages'
task :default


desc 'Build / test fullpath in Go'
task golang: 'golang/fullpath' do
  cucumber 'golang'
end
file 'golang/fullpath' => 'golang/fullpath.go' do
  sh 'go', 'build', '-o', 'golang/fullpath', 'golang/fullpath.go'
end
task go: :golang # shorthand
task default: :golang


desc 'Test fullpath in Ruby'
task :ruby do
  cucumber 'ruby'
end
task default: :ruby


desc 'Build / test fullpath in Elixir'
task elixir: 'elixir/fullpath' do
  cucumber 'elixir'
end
file 'elixir/fullpath' do # TODO: if we declare deps, can avoid rebuilding every time we run tests
  cd('elixir') { sh 'mix', 'escript.build' }
end
task default: :elixir


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
