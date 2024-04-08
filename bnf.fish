#!/usr/bin/env fish

# (def f [x y z]
#   (type Int Int Int Int)
#   (+ x y z)
# )
#
# (def IntList []
#   (type Type)
#   (List Int)
# )
#
# (let x (type Int) 5 # let x: Int = 5 in print x 
#   (print x)
# )
#
# (def fetch-file [])

function log 
  set_color --bold green
  printf '>>> '
  set_color --bold white
  printf $argv
  echo
  set_color normal
end

function end_step 
  echo ''
end

function fail
  set_color --bold red
  printf '<<< '
  set_color --bold magenta
  printf $argv
  echo
  exit 1
end

if test (count $argv) -lt 1
  printf 'usage: bnf.fish grammar-path.cf\n' >&2
  exit 1
end

set grammar_file ".cf"

set root_path (dirname (status --current-filename))
set lib_path "$root_path/lib"
set app_path "$root_path/test"
set grammar_path $argv[1]
set grammar_name (basename $grammar_name .cf)
set target_grammar_path "$lib_path/$grammar_file" 

if test -e "$target_grammar_path"
  rm "$target_grammar_path"
end


log 'copying %s to %s' "$grammar_path" "$target_grammar_path"
cp "$grammar_path" "$target_grammar_path"
end_step


log 'running bnfc...'
cd $lib_path 
if not bnfc --functor --haskell --make $grammar_file
  fail
end
make > /dev/null 2>&1
cd ..
end_step

log 'copying Test.hs to %s' "$app_path"
cp "$lib_path/Test.hs" "$app_path"
end_step


log 'running cabal build'
cabal build

